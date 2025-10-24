package hephaistos

import "core:fmt"
import "core:reflect"
import "core:slice"

import "ast"
import "types"
import "tokenizer"

import spv "spirv-odin"

VOID      :: 1
VOID_PROC :: 2 // proc() -> ()

CG_Storage_Class :: enum {
	Global = 1,
	Function,
	Uniform,
	Input,
	Output,
}

CG_Type_Info :: struct {
	type:      spv.Id,
	nil_value: spv.Id,
	ptr_types: [CG_Storage_Class]spv.Id,
}

Type_Hash_Entry :: struct {
	hash: u64,
	type: ^types.Type,
	info: CG_Type_Info,
}

Type_Registry :: [][dynamic]Type_Hash_Entry

CG_Context :: struct {
	constant_cache:     map[types.Const_Value]struct{ id: spv.Id, type: ^types.Type, },
	string_cache:       map[string]spv.Id,
	type_registry:      Type_Registry,

	meta:               spv.Builder,
	memory_model:       spv.Builder,
	entry_points:       spv.Builder,
	exectution_modes:   spv.Builder,
	debug_a:            spv.Builder,
	debug_b:            spv.Builder,
	annotations:        spv.Builder,
	types:              spv.Builder,
	globals:            spv.Builder,
	functions:          spv.Builder,

	current_id:         spv.Id,
	checker:            ^Checker,
	scopes:             [dynamic]CG_Scope,

	capabilities:       map[spv.Capability]struct{},
	referenced_globals: map[spv.Id]struct{},

	link_name:          string,
}

CG_Entity :: struct {
	type:  ^types.Type,
	value: CG_Value,
}

CG_Value :: struct {
	id:            spv.Id,
	storage_class: CG_Storage_Class,
	type:         ^types.Type,
}

CG_Scope :: struct {
	entities:     map[string]CG_Entity,
	label:        string,
	label_id:     spv.Id,
	return_value: spv.Id,
	return_type:  ^types.Type,
	outputs:      []spv.Id,
}

cg_lookup_entity :: proc(ctx: ^CG_Context, name: string) -> ^CG_Entity {
	#reverse for scope in ctx.scopes {
		e := &scope.entities[name]
		if e != nil {
			return e
		}
	}
	unreachable()
}

cg_insert_entity :: proc(ctx: ^CG_Context, name: string, storage_class: CG_Storage_Class, type: ^types.Type, id: spv.Id) {
	spv.OpName(&ctx.debug_b, id, name)
	ctx.scopes[len(ctx.scopes) - 1].entities[name] = {
		type  = type,
		value = {
			id            = id,
			storage_class = storage_class,
		},
	}
}

cg_scope_push :: proc(ctx: ^CG_Context, label: string = "") -> ^CG_Scope {
	append(&ctx.scopes, CG_Scope {
		label = label,
	})
	return &ctx.scopes[len(ctx.scopes) - 1]
}

cg_scope_pop :: proc(ctx: ^CG_Context) -> CG_Scope {
	return pop(&ctx.scopes)
}

cg_scope :: proc(
	ctx:     ^CG_Context,
	builder: ^spv.Builder,
	stmts:   []^ast.Stmt,
	next:    spv.Id = 0,
	label:   string = "",
) -> (start_label: spv.Id) {
	scope := cg_scope_push(ctx, label)
	defer cg_scope_pop(ctx)

	start_label    = spv.OpLabel(builder)
	scope.label_id = start_label
	returned      := cg_stmt_list(ctx, builder, stmts)
	if !returned && next != 0 {
		spv.OpBranch(builder, next)
	}

	return
}

cg_init :: proc(ctx: ^CG_Context, checker: ^Checker, file_name: Maybe(string) = nil, file_source: Maybe(string) = nil) {
	ctx.checker                     = checker
	ctx.meta.current_id             = &ctx.current_id
	ctx.memory_model.current_id     = &ctx.current_id
	ctx.entry_points.current_id     = &ctx.current_id
	ctx.exectution_modes.current_id = &ctx.current_id
	ctx.debug_a.current_id          = &ctx.current_id
	ctx.debug_b.current_id          = &ctx.current_id
	ctx.annotations.current_id      = &ctx.current_id
	ctx.types.current_id            = &ctx.current_id
	ctx.globals.current_id          = &ctx.current_id
	ctx.functions.current_id        = &ctx.current_id

	ctx.type_registry = make([][dynamic]Type_Hash_Entry, 1024)

	append(&ctx.meta.data, spv.MAGIC_NUMBER)
	append(&ctx.meta.data, spv.VERSION)
	append(&ctx.meta.data, 'H' << 0 | 'E' << 8 | 'P' << 16 | 'H' << 24)
	append(&ctx.meta.data, 4194303)
	append(&ctx.meta.data, 0)
	cg_scope_push(ctx)

	void := spv.OpTypeVoid(&ctx.types)
	assert(void == VOID)
	void_proc := spv.OpTypeFunction(&ctx.types, void)
	assert(void_proc == VOID_PROC)

	spv.OpName(&ctx.debug_b, void,      "$VOID")
	spv.OpName(&ctx.debug_b, void_proc, "$VOID_PROC")

	if file_name, ok := file_name.?; ok {
		spv.OpSource(&ctx.debug_a, .Unknown, 0, cg_string(ctx, file_name), file_source)
	}
}

register_type :: proc(registry: ^Type_Registry, t: ^types.Type) -> (type_info: ^CG_Type_Info, ok: bool) {
	hash := types.type_hash(t)
	if hash == 0 {
		hash = 1
	}
	entry := &registry[hash % u64(len(registry))]
	for &e in entry {
		if e.type == t {
			return &e.info, true
		}
	}
	for &e in entry {
		if e.hash == hash && types.equal(t, e.type) {
			return &e.info, true
		}
	}
	append(entry, Type_Hash_Entry {
		hash = hash,
		type = t,
	})
	return &entry[len(entry) - 1].info, false
}

cg_string :: proc(ctx: ^CG_Context, str: string) -> spv.Id {
	if id, ok := ctx.string_cache[str]; ok {
		return id
	}
	id := spv.OpString(&ctx.debug_a, str)
	ctx.string_cache[str] = id
	return id
}

cg_decl :: proc(ctx: ^CG_Context, builder: ^spv.Builder, decl: ^ast.Decl, global: bool) {
	value_builder     := builder
	decl_builder      := &ctx.functions
	storage_class     := CG_Storage_Class.Function
	spv_storage_class := spv.StorageClass.Function

	if global {
		value_builder     = nil
		decl_builder      = &ctx.globals
		storage_class     = .Global
		spv_storage_class = .Private
	}

	switch v in decl.derived_decl {
	case ^ast.Decl_Value:
		if v.uniform {
			value_builder     = nil
			decl_builder      = &ctx.globals
			storage_class     = .Uniform
			spv_storage_class = .Uniform
		}

		prev_link_name := ctx.link_name
		if v.link_name != "" {
			ctx.link_name = v.link_name
		}
		defer if v.link_name != "" {
			ctx.link_name = prev_link_name
		}

		if v.mutable {
			if len(v.values) == 0 {
				for type, i in v.types {
					type_info := cg_type(ctx, type)
					init: Maybe(spv.Id)
					if !v.uniform {
						init = cg_nil_value(ctx, type_info)
					}
					id   := spv.OpVariable(decl_builder, cg_type_ptr(ctx, type_info, storage_class), spv_storage_class, init)
					name := v.lhs[i].derived_expr.(^ast.Expr_Ident).ident.text
					cg_insert_entity(ctx, name, storage_class, type, id)

					if v.uniform {
						spv.OpDecorate(&ctx.annotations, type_info.type, .Block)
						spv.OpDecorate(&ctx.annotations, id,             .Binding,       u32(v.binding))
						spv.OpDecorate(&ctx.annotations, id,             .DescriptorSet, u32(v.descriptor_set))
					}
				}
			} else {
				for value, i in v.values {
					type      := v.types[i]
					type_info := cg_type(ctx, type)
					init: spv.Id
					if global {
						init = cg_expr(ctx, nil, value).id
					} else {
						init = cg_nil_value(ctx, type_info)
					}
					id := spv.OpVariable(decl_builder, cg_type_ptr(ctx, type_info, storage_class), spv_storage_class, init)
					if !global {
						init := cg_expr(ctx, value_builder, value)
						spv.OpStore(value_builder, id, init.id)
					}
					name := v.lhs[i].derived_expr.(^ast.Expr_Ident).ident.text
					cg_insert_entity(ctx, name, storage_class, type, id)
				}
			}
		} else {
			for value, i in v.values {
				if value.type.kind != .Proc {
					continue
				}

				name := v.lhs[i].derived_expr.(^ast.Expr_Ident).ident.text

				prev_link_name := ctx.link_name
				if v.link_name == "" {
					ctx.link_name = name
				}
				defer if v.link_name == "" {
					ctx.link_name = prev_link_name
				}

				id := cg_expr(ctx, nil, value)
				cg_insert_entity(ctx, name, nil, value.type, id.id)
			}
		}
	}
}

@(require_results)
cg_generate :: proc(ctx: ^CG_Context, stmts: []^ast.Stmt) -> []u32 {
	// capabilities
	spv.OpCapability(&ctx.meta, .Shader)

	// memory model
	spv.OpMemoryModel(&ctx.memory_model, .Logical, .Simple)

	b: spv.Builder = { current_id = &ctx.current_id }
	cg_stmt_list(ctx, &b, stmts, true)

	spirv := slice.concatenate([][]u32{
		ctx.meta.data[:],
		ctx.memory_model.data[:],
		ctx.entry_points.data[:],
		ctx.exectution_modes.data[:],
		ctx.debug_a.data[:],
		ctx.debug_b.data[:],
		ctx.annotations.data[:],
		ctx.types.data[:],
		ctx.globals.data[:],
		ctx.functions.data[:],
		b.data[:],
	})
	spirv[spv.ID_BOUND_INDEX] = u32(ctx.current_id + 1)
	return spirv
}

cg_constant :: proc(ctx: ^CG_Context, value: types.Const_Value) -> CG_Value {
	if cached, ok := ctx.constant_cache[value]; ok {
		return {
			id   = cached.id,
			type = cached.type,
		}
	}

	id:    spv.Id
	type: ^types.Type
	defer ctx.constant_cache[value] = { id = id, type = type, }

	switch v in value {
	case i64:
		type = types.t_i32
		ti  := cg_type(ctx, type)
		if v == {} {
			id = cg_nil_value(ctx, ti)
			break
		}
		id = spv.OpConstant(&ctx.types, ti.type, u32(v))
	case f64:
		type = types.t_f32
		ti  := cg_type(ctx, type)
		if v == {} {
			id = cg_nil_value(ctx, ti)
			break
		}
		id = spv.OpConstant(&ctx.types, ti.type, transmute(u32)f32(v))
	case bool:
		type = types.t_bool
		ti  := cg_type(ctx, type)
		if v == {} {
			id = cg_nil_value(ctx, ti)
			break
		}
		if v {
			id = spv.OpConstantTrue(&ctx.types, ti.type)
		} else {
			id = spv.OpConstantFalse(&ctx.types, ti.type)
		}
	case string:
		panic("")
	}

	return { id = id, type = type, }
}

cg_nil_value :: proc {
	cg_nil_value_from_type,
	cg_nil_value_from_type_info,
}

@(require_results)
cg_nil_value_from_type :: proc(ctx: ^CG_Context, type_info: ^CG_Type_Info) -> spv.Id {
	if type_info.nil_value == 0 {
		type_info.nil_value = spv.OpConstantNull(&ctx.types, type_info.type)
	}
	return type_info.nil_value
}

@(require_results)
cg_nil_value_from_type_info :: proc(ctx: ^CG_Context, type: ^types.Type) -> spv.Id {
	return cg_nil_value_from_type(ctx, cg_type(ctx, type))
}

cg_type_ptr :: proc {
	cg_type_ptr_from_type,
	cg_type_ptr_from_type_info,
}

@(require_results)
cg_type_ptr_from_type :: proc(ctx: ^CG_Context, type_info: ^CG_Type_Info, storage_class: CG_Storage_Class) -> spv.Id {
	if type_info.ptr_types[storage_class] == 0 {
		spv_storage_class: spv.StorageClass
		switch storage_class {
		case .Global:
			spv_storage_class = .Private
		case .Function:
			spv_storage_class = .Function
		case .Uniform:
			spv_storage_class = .Uniform
		case .Input:
			spv_storage_class = .Input
		case .Output:
			spv_storage_class = .Output
		}
		type_info.ptr_types[storage_class] = spv.OpTypePointer(&ctx.types, spv_storage_class, type_info.type)
	}
	return type_info.ptr_types[storage_class]
}

@(require_results)
cg_type_ptr_from_type_info :: proc(ctx: ^CG_Context, type: ^types.Type, storage_class: CG_Storage_Class) -> spv.Id {
	return cg_type_ptr_from_type(ctx, cg_type(ctx, type), storage_class)
}

cg_type :: proc(ctx: ^CG_Context, type: ^types.Type) -> ^CG_Type_Info {
	assert(type != nil)

	registry := &ctx.type_registry
	info, ok := register_type(registry, type)
	if ok {
		assert(info.type != 0)
		return info
	}

	#partial switch type.kind {
	case .Uint:
		info.type = spv.OpTypeInt(&ctx.types, u32(type.size * 8), 0)
		cap: spv.Capability
		switch type.size {
		case 1:
			cap = .Int8
		case 2:
			cap = .Int16
		case 8:
			cap = .Int64
		}
		if cap != nil && cap not_in ctx.capabilities {
			ctx.capabilities[cap] = {}
			spv.OpCapability(&ctx.meta, cap)
		}
	case .Int:
		info.type = spv.OpTypeInt(&ctx.types, u32(type.size * 8), 1)
		cap: spv.Capability
		switch type.size {
		case 1:
			cap = .Int8
		case 2:
			cap = .Int16
		case 8:
			cap = .Int64
		}
		if cap != nil && cap not_in ctx.capabilities {
			ctx.capabilities[cap] = {}
			spv.OpCapability(&ctx.meta, cap)
		}
	case .Float:
		info.type = spv.OpTypeFloat(&ctx.types, u32(type.size * 8))
		cap: spv.Capability
		switch type.size {
		case 2:
			cap = .Float16
		case 8:
			cap = .Float64
		}
		if cap != nil && cap not_in ctx.capabilities {
			ctx.capabilities[cap] = {}
			spv.OpCapability(&ctx.meta, cap)
		}
	case .Bool:
		info.type = spv.OpTypeBool(&ctx.types)
	case .Struct, .Tuple:
		type_struct := type.variant.(^types.Struct)
		if len(type_struct.fields) != 0 {
			fields := make([]spv.Id, len(type_struct.fields))
			for &f, i in fields {
				f = cg_type(ctx, type_struct.fields[i].type).type
			}
			info.type = spv.OpTypeStruct(&ctx.types, ..fields)
			for f, i in type_struct.fields {
				spv.OpMemberName(&ctx.debug_b, info.type, u32(i), f.name.text)
				if type.kind == .Struct {
					spv.OpMemberDecorate(&ctx.annotations, info.type, u32(i), .Offset, u32(f.offset))
				}
			}
		} else {
			info.type = VOID
		}
	case .Vector:
		type_vector := type.variant.(^types.Vector)
		info.type = spv.OpTypeVector(&ctx.types, cg_type(ctx, type_vector.elem).type, u32(type_vector.count))
	case .Matrix:
		type_matrix := type.variant.(^types.Matrix)
		info.type = spv.OpTypeMatrix(&ctx.types, cg_type(ctx, type_matrix.col_type).type, u32(type_matrix.cols))
		if .Matrix not_in ctx.capabilities {
			ctx.capabilities[.Matrix] = {}
			spv.OpCapability(&ctx.meta, .Matrix)
		}
	case .Proc:
		type_proc := type.variant.(^types.Proc)
		args      := make([]spv.Id, len(type_proc.args))
		for &arg, i in args {
			arg = cg_type(ctx, type_proc.args[i].type).type
		}
		info.type = spv.OpTypeFunction(&ctx.types, cg_type(ctx, type_proc.return_type).type, ..args)
	}

	return info
}

cg_proc_lit :: proc(ctx: ^CG_Context, p: ^ast.Expr_Proc_Lit) -> CG_Value {
	type             := p.type.variant.(^types.Proc)
	return_type_info := cg_type(ctx, type.return_type)
	proc_type_id     := cg_type(ctx, type).type
	return_type_id   := return_type_info.type
	if p.shader_kind != nil {
		proc_type_id   = VOID_PROC
		return_type_id = VOID
	}
	id := spv.OpFunction(&ctx.functions, return_type_id, {}, proc_type_id)

	scope := cg_scope_push(ctx)
	defer cg_scope_pop(ctx)

	return_value: spv.Id
	body := spv.Builder { current_id = &ctx.current_id, }

	outputs:   [dynamic]spv.Id
	interface: [dynamic]spv.Id
	if p.shader_kind != nil {
		for arg, i in type.args {
			id := spv.OpVariable(&ctx.globals, cg_type_ptr(ctx, arg.type, .Input), .Input, nil)
			cg_insert_entity(ctx, p.args[i].ident.text, .Input, arg.type, id)
			append(&interface, id)
			spv.OpDecorate(&ctx.annotations, id, .Location, u32(i))
		}
		spv.OpLabel(&ctx.functions)
		if len(type.returns) != 0 {
			for ret, i in type.returns {
				type_info := cg_type(ctx, ret.type)
				id        := spv.OpVariable(&ctx.globals, cg_type_ptr(ctx, type_info, .Output), .Output, nil)
				name      := ret.name.text
				if ret.value != nil {
					init := cg_constant(ctx, ret.value)
					spv.OpStore(&body, id, init.id)
				}
				cg_insert_entity(ctx, name, .Output, ret.type, id)
				append(&interface, id)
				append(&outputs,   id)
				spv.OpDecorate(&ctx.annotations, id, .Location, u32(i))
			}
		}
	} else {
		for arg, i in type.args {
			id := spv.OpFunctionParameter(&ctx.functions, cg_type(ctx, arg.type).type)
			cg_insert_entity(ctx, p.args[i].ident.text, nil, arg.type, id)
		}
		spv.OpLabel(&ctx.functions)
		return_value: spv.Id
		if len(type.returns) != 0 {
			return_value = spv.OpVariable(&ctx.functions, cg_type_ptr(ctx, return_type_info, .Function), .Function, cg_nil_value(ctx, return_type_info))
			spv.OpName(&ctx.debug_b, return_value, "$return_tuple")
			for ret, i in type.returns {
				type_info := cg_type(ctx, ret.type)
				id        := spv.OpAccessChain(&ctx.functions, cg_type_ptr(ctx, type_info, .Function), return_value, cg_constant(ctx, i64(i)).id)
				name      := ret.name.text
				if ret.value != nil {
					init := cg_constant(ctx, ret.value)
					spv.OpStore(&body, id, init.id)
				}
				cg_insert_entity(ctx, name, .Function, ret.type, id)
			}
		}
	}

	scope.return_value = return_value
	scope.return_type  = type.return_type
	scope.outputs      = outputs[:]

	returned := cg_stmt_list(ctx, &body, p.body)
	if !returned {
		if len(type.returns) != 0 {
			value := spv.OpLoad(&body, return_type_info.type, return_value)
			spv.OpReturnValue(&body, value)
		} else {
			spv.OpReturn(&body)
		}
	}

	append(&ctx.functions.data, ..body.data[:])
	spv.OpFunctionEnd(&ctx.functions)

	if p.shader_kind != nil {
		execution_mode: spv.ExecutionModel
		switch p.shader_kind {
		case .Vertex:
			execution_mode = .Vertex
		case .Fragment:
			execution_mode = .Fragment
		case .Geometry:
			execution_mode = .Geometry
		case .Tesselation:
			execution_mode = .TessellationControl
		case .Compute:
			execution_mode = .Kernel
		}
		reserve(&interface, len(interface) + len(ctx.referenced_globals))
		for g in ctx.referenced_globals {
			append(&interface, g)
		}
		clear(&ctx.referenced_globals)
		name := "$entry_point"
		if len(ctx.link_name) != 0 {
			name = ctx.link_name
		}
		spv.OpEntryPoint(&ctx.entry_points, execution_mode, id, name, ..interface[:])
		if p.shader_kind == .Fragment {
			spv.OpExecutionMode(&ctx.entry_points, id, .OriginUpperLeft)
		}
	}

	return { id = id, }
}

cg_deref :: proc(ctx: ^CG_Context, builder: ^spv.Builder, value: CG_Value) -> spv.Id {
	if value.storage_class == nil {
		return value.id
	}

	return spv.OpLoad(builder, cg_type(ctx, value.type).type, value.id)
}

@(require_results)
cg_expr_binary :: proc(
	ctx:                  ^CG_Context,
	builder:              ^spv.Builder,
	op:                   tokenizer.Token_Kind,
	lhs_value, rhs_value: CG_Value,
	type:                 ^types.Type,
) -> spv.Id {
	lhs       := cg_deref(ctx, builder, lhs_value)
	rhs       := cg_deref(ctx, builder, rhs_value)
	lhs_type  := lhs_value.type
	rhs_type  := rhs_value.type
	type_info := cg_type(ctx, type)

	if op_is_relation(op) {
		t := types.op_result_type(lhs_type, rhs_type)
		#partial switch t.kind {
		case .Int:
			#partial switch op {
			case .Equal:
				return spv.OpIEqual(builder, type_info.type, lhs, rhs)
			case .Not_Equal:
				return spv.OpINotEqual(builder, type_info.type, lhs, rhs)
			case .Less:
				return spv.OpSLessThan(builder, type_info.type, lhs, rhs)
			case .Less_Equal:
				return spv.OpSLessThanEqual(builder, type_info.type, lhs, rhs)
			case .Greater:
				return spv.OpSGreaterThan(builder, type_info.type, lhs, rhs)
			case .Greater_Equal:
				return spv.OpSGreaterThanEqual(builder, type_info.type, lhs, rhs)
			}
		case .Uint:
			#partial switch op {
			case .Equal:
				return spv.OpIEqual(builder, type_info.type, lhs, rhs)
			case .Not_Equal:
				return spv.OpINotEqual(builder, type_info.type, lhs, rhs)
			case .Less:
				return spv.OpULessThan(builder, type_info.type, lhs, rhs)
			case .Less_Equal:
				return spv.OpULessThanEqual(builder, type_info.type, lhs, rhs)
			case .Greater:
				return spv.OpUGreaterThan(builder, type_info.type, lhs, rhs)
			case .Greater_Equal:
				return spv.OpUGreaterThanEqual(builder, type_info.type, lhs, rhs)
			}
		case .Float:
			#partial switch op {
			case .Equal:
				return spv.OpFOrdEqual(builder, type_info.type, lhs, rhs)
			case .Not_Equal:
				return spv.OpFOrdNotEqual(builder, type_info.type, lhs, rhs)
			case .Less:
				return spv.OpFOrdLessThan(builder, type_info.type, lhs, rhs)
			case .Less_Equal:
				return spv.OpFOrdLessThanEqual(builder, type_info.type, lhs, rhs)
			case .Greater:
				return spv.OpFOrdGreaterThan(builder, type_info.type, lhs, rhs)
			case .Greater_Equal:
				return spv.OpFOrdGreaterThanEqual(builder, type_info.type, lhs, rhs)
			}
		case .Bool:
			#partial switch op {
			case .Equal:
				return spv.OpLogicalEqual(builder, type_info.type, lhs, rhs)
			case .Not_Equal:
				return spv.OpLogicalNotEqual(builder, type_info.type, lhs, rhs)
			}
		}
		panic("")
	}

	#partial switch type.kind {
	case .Int:
		#partial switch op {
		case .Add:
			return spv.OpIAdd(builder, type_info.type, lhs, rhs)
		case .Subtract:
			return spv.OpISub(builder, type_info.type, lhs, rhs)
		case .Multiply:
			return spv.OpIMul(builder, type_info.type, lhs, rhs)
		case .Divide:
			return spv.OpSDiv(builder, type_info.type, lhs, rhs)
		case .Modulo:
			return spv.OpSMod(builder, type_info.type, lhs, rhs)
		}
	case .Uint:
		#partial switch op {
		case .Add:
			return spv.OpIAdd(builder, type_info.type, lhs, rhs)
		case .Subtract:
			return spv.OpISub(builder, type_info.type, lhs, rhs)
		case .Multiply:
			return spv.OpIMul(builder, type_info.type, lhs, rhs)
		case .Divide:
			return spv.OpUDiv(builder, type_info.type, lhs, rhs)
		case .Modulo:
			return spv.OpUMod(builder, type_info.type, lhs, rhs)
		}
	case .Float:
		#partial switch op {
		case .Add:
			return spv.OpFAdd(builder, type_info.type, lhs, rhs)
		case .Subtract:
			return spv.OpFSub(builder, type_info.type, lhs, rhs)
		case .Multiply:
			return spv.OpFMul(builder, type_info.type, lhs, rhs)
		case .Divide:
			return spv.OpFDiv(builder, type_info.type, lhs, rhs)
		case .Modulo:
			return spv.OpFMod(builder, type_info.type, lhs, rhs)
		}
	case .Bool:
		#partial switch op {
		case .And:
			return spv.OpLogicalAnd(builder, type_info.type, lhs, rhs)
		case .Or:
			return spv.OpLogicalAnd(builder, type_info.type, lhs, rhs)
		}
		unimplemented()
	case .Matrix:
		unimplemented()
	case .Vector:
		if lhs_type.kind == .Vector && rhs_type.kind == .Vector {
			#partial switch op {
			case .Add:
				return spv.OpFAdd(builder, type_info.type, lhs, rhs)
			case .Subtract:
				return spv.OpFSub(builder, type_info.type, lhs, rhs)
			case .Multiply:
				return spv.OpFMul(builder, type_info.type, lhs, rhs)
			case .Divide:
				return spv.OpFDiv(builder, type_info.type, lhs, rhs)
			case .Modulo:
				return spv.OpFMod(builder, type_info.type, lhs, rhs)
			}
			panic("")
		}

		lhs := lhs
		rhs := rhs
		if rhs_type.kind == .Vector {
			lhs, rhs = rhs, lhs
		}

		#partial switch op {
		case .Add:
			// return spv.OpFAdd(builder, type_info.type, lhs, rhs)
		case .Subtract:
			// return spv.OpFSub(builder, type_info.type, lhs, rhs)
		case .Multiply:
			return spv.OpVectorTimesScalar(builder, type_info.type, lhs, rhs)
		case .Divide:
			// return spv.OpFDiv(builder, type_info.type, lhs, rhs)
		case .Modulo:
			// return spv.OpFMod(builder, type_info.type, lhs, rhs)
		}

	case:
		panic("")
	}

	unimplemented()
}

cg_expr :: proc(
	ctx:     ^CG_Context,
	builder: ^spv.Builder,
	expr:    ^ast.Expr,
	deref:   bool = true,
) -> (value: CG_Value) {
	assert(expr      != nil)
	assert(expr.type != nil)

	// TODO: implicit broadcasting / scalar -> matrix conversions
	value      = _cg_expr(ctx, builder, expr, deref)
	value.type = expr.type
	return
}

_cg_expr :: proc(
	ctx:     ^CG_Context,
	builder: ^spv.Builder,
	expr:    ^ast.Expr,
	deref:   bool,
) -> (value: CG_Value) {
	assert(expr      != nil)
	assert(expr.type != nil)

	if expr.const_value != nil {
		return cg_constant(ctx, expr.const_value)
	}

	when ODIN_DEBUG {
		start: int
		if builder != nil {
			start = len(builder.data)
		}
		defer if builder != nil && len(builder.data) != start {
			fmt.println(transmute([2]u16)builder.data[start], builder.data[start + 1:])
		}
	}

	switch v in expr.derived_expr {
	case ^ast.Expr_Constant:
		return cg_constant(ctx, expr.const_value)
	case ^ast.Expr_Binary:
		lhs, rhs := cg_expr(ctx, builder, v.lhs), cg_expr(ctx, builder, v.rhs)
		value.id  = cg_expr_binary(ctx, builder, v.op, lhs, rhs, v.type)
		return
	case ^ast.Expr_Ident:
		value := cg_lookup_entity(ctx, v.ident.text).value
		if value.storage_class == .Uniform || value.storage_class == .Global {
			ctx.referenced_globals[value.id] = {}
		}
		if !deref {
			assert(value.storage_class != nil)
			return value
		}
		if value.storage_class == nil {
			return value
		} else {
			return { id = spv.OpLoad(builder, cg_type(ctx, v.type).type, value.id), }
		}
	case ^ast.Expr_Proc_Lit:
		return cg_proc_lit(ctx, v)
	case ^ast.Expr_Proc_Sig:
	case ^ast.Expr_Paren:
		return cg_expr(ctx, builder, v.expr, deref)
	case ^ast.Expr_Selector:
		lhs := cg_expr(ctx, builder, v.lhs, false)

		lhs_type := v.lhs.type
		if lhs.type.kind == .Struct {
			field_index: int = -1
			for field, i in lhs_type.variant.(^types.Struct).fields {
				if field.name.text == v.selector.text {
					field_index = i
					break
				}
			}
			assert(field_index != -1)

			type_info := cg_type(ctx, v.type)
			ptr       := spv.OpAccessChain(builder, cg_type_ptr(ctx, type_info, lhs.storage_class), lhs.id, cg_constant(ctx, i64(field_index)).id)

			if deref {
				return { id = spv.OpLoad(builder, type_info.type, ptr), }
			} else {
				return { id = ptr, storage_class = lhs.storage_class, }
			}
		}

		assert(lhs.type.kind == .Vector)
		assert(deref)

		indices := make([dynamic]u32, 0, len(v.selector.text))
		for char in v.selector.text {
			switch char {
			case 'x', 'r':
				append(&indices, 0)
			case 'y', 'g':
				append(&indices, 1)
			case 'z', 'b':
				append(&indices, 2)
			case 'w', 'a':
				append(&indices, 3)
			}
		}

		value := cg_deref(ctx, builder, lhs)
		return { id = spv.OpVectorShuffle(builder, cg_type(ctx, v.type).type, value, value, ..indices[:]), }

	case ^ast.Expr_Call:
		if v.is_cast {
			
		} else {
			fn   := cg_expr(ctx, builder, v.lhs)
			args := make([]spv.Id, len(v.args))
			for &arg, i in args {
				arg = cg_expr(ctx, builder, v.args[i].value).id
			}

			return_type_info := cg_type(ctx, expr.type)
			ret              := spv.OpFunctionCall(builder, return_type_info.type, fn.id, ..args)
			proc_type        := v.lhs.type.variant.(^types.Proc)

			if len(proc_type.returns) == 1 {
				type_info := cg_type(ctx, proc_type.returns[0].type)
				copy      := spv.OpVariable(&ctx.functions, cg_type_ptr(ctx, return_type_info, .Function), .Function)
				spv.OpStore(builder, copy, ret)
				ptr       := spv.OpAccessChain(builder, cg_type_ptr(ctx, type_info, .Function), copy, cg_constant(ctx, i64(0)).id)
				return { id = spv.OpLoad(builder, type_info.type, ptr), }
			}
			return { id = ret, }
		}
	case ^ast.Expr_Compound:
	case ^ast.Expr_Index:
		lhs := cg_expr(ctx, builder, v.lhs, deref)
		rhs := cg_expr(ctx, builder, v.rhs)

		if lhs.storage_class != nil {
			id    := spv.OpAccessChain(builder, cg_type_ptr(ctx, v.type, lhs.storage_class), lhs.id, rhs.id)
			value := CG_Value { id = id, storage_class = lhs.storage_class, }
			if deref {
				return { id = cg_deref(ctx, builder, value), }
			} else {
				return value
			}
		}

		return { id = spv.OpVectorExtractDynamic(builder, cg_type(ctx, v.type).type, lhs.id, rhs.id), }
	case ^ast.Expr_Cast:
	case ^ast.Expr_Unary:
	
	case ^ast.Type_Struct, ^ast.Type_Array, ^ast.Type_Matrix:
		panic("")
	}

	fmt.panicf("unimplemented: %v", reflect.union_variant_typeid(expr.derived_expr))
}

cg_lookup_proc_scope :: proc(ctx: ^CG_Context) -> ^CG_Scope {
	#reverse for &scope in ctx.scopes {
		if scope.return_type != nil {
			return &scope
		}
	}
	panic("")
}

cg_lookup_return_value :: proc(ctx: ^CG_Context) -> spv.Id {
	return cg_lookup_proc_scope(ctx).return_value
}

cg_lookup_return_type :: proc(ctx: ^CG_Context) -> ^types.Type {
	return cg_lookup_proc_scope(ctx).return_type
}

cg_lookup_label :: proc(ctx: ^CG_Context, label: string) -> ^CG_Scope {
	#reverse for &scope in ctx.scopes {
		if scope.label == label {
			return &scope
		}
	}
	panic("")
}

cg_stmt :: proc(ctx: ^CG_Context, builder: ^spv.Builder, stmt: ^ast.Stmt, global := false) -> (returned: bool) {
	if stmt == nil {
		return
	}

	switch v in stmt.derived_stmt {
	case ^ast.Stmt_Return:
		if len(v.values) == 0 {
			value := cg_lookup_return_value(ctx)
			if value != 0 {
				spv.OpReturnValue(builder, value)
			} else {
				spv.OpReturn(builder)
			}
		} else {
			proc_scope   := cg_lookup_proc_scope(ctx)
			return_value := proc_scope.return_value
			type         := proc_scope.return_type.variant.(^types.Struct)

			if return_value == 0 {
				for value, i in v.values {
					v   := cg_expr(ctx, builder, value)
					ptr := proc_scope.outputs[i]
					spv.OpStore(builder, ptr, v.id)
				}
				spv.OpReturn(builder)
			} else {
				return_ti := cg_type(ctx, type)
				for value, i in v.values {
					v   := cg_expr(ctx, builder, value)
					ptr := spv.OpAccessChain(builder, cg_type_ptr(ctx, type.fields[i].type, .Function), return_value, cg_constant(ctx, i64(i)).id)
					spv.OpStore(builder, ptr, v.id)
				}
				spv.OpReturnValue(builder, spv.OpLoad(builder, return_ti.type, return_value))
			}
		}
		return true
	case ^ast.Stmt_Break:
		unimplemented()
	case ^ast.Stmt_Continue:
		unimplemented()
	case ^ast.Stmt_For_Range:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

	case ^ast.Stmt_For:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

		cg_stmt(ctx, builder, v.init)

		body_builder   := &spv.Builder{ current_id = &ctx.current_id, }
		header_builder := &spv.Builder{ current_id = &ctx.current_id, }
		end_builder    := &spv.Builder{ current_id = &ctx.current_id, }
		post_builder   := &spv.Builder{ current_id = &ctx.current_id, }

		spv.OpBranch(builder, ctx.current_id + 1)
		jump_back_target := spv.OpLabel(builder)

		header := spv.OpLabel(header_builder)
		spv.OpName(&ctx.debug_b, header, "header")
		end    := spv.OpLabel(end_builder)
		spv.OpName(&ctx.debug_b, end, "end")

		post_label := spv.OpLabel(post_builder)
		cg_stmt(ctx, post_builder, v.post)
		spv.OpBranch(post_builder, jump_back_target)

		body := cg_scope(ctx, body_builder, v.body, next = post_label)

		condition: spv.Id
		if v.cond != nil {
			condition = cg_expr(ctx, header_builder, v.cond).id
		} else {
			condition = cg_constant(ctx, true).id
		}

		spv.OpLoopMerge(builder, end, post_label, {})
		spv.OpBranch(builder, header)
		spv.OpBranchConditional(header_builder, condition, body, end)

		append(&builder.data, ..header_builder.data[:])
		append(&builder.data, ..body_builder.data[:])
		append(&builder.data, ..post_builder.data[:])
		append(&builder.data, ..end_builder.data[:])

	case ^ast.Stmt_Block:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

		cg_stmt_list(ctx, builder, v.body)

	case ^ast.Stmt_If:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

		cg_stmt(ctx, builder, v.init)
		cond := cg_expr(ctx, builder, v.cond)

		then_block := &spv.Builder{ current_id = &ctx.current_id, }
		else_block := &spv.Builder{ current_id = &ctx.current_id, }
		end_block  := &spv.Builder{ current_id = &ctx.current_id, }

		end_label := spv.OpLabel(end_block)

		then_label := cg_scope(ctx, then_block, v.then_block, next = end_label)
		else_label := cg_scope(ctx, else_block, v.else_block, next = end_label)

		spv.OpSelectionMerge(builder, end_label, {})
		spv.OpBranchConditional(builder, cond.id, then_label, else_label)

		append(&builder.data, ..then_block.data[:])
		append(&builder.data, ..else_block.data[:])
		append(&builder.data, ..end_block.data[:])
	case ^ast.Stmt_Switch:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

	case ^ast.Stmt_Assign:
		lhs_i := 0
		for value in v.rhs {
			lhs := cg_expr(ctx, builder, v.lhs[lhs_i], deref = false)
			rhs := cg_expr(ctx, builder, value)
			if v.op == nil {
				spv.OpStore(builder, lhs.id, rhs.id)
				continue
			}

			lhs_ti := cg_type(ctx, v.lhs[lhs_i].type)
			value  := cg_expr_binary(
				ctx,
				builder,
				v.op,
				{ id = spv.OpLoad(builder, lhs_ti.type, lhs.id), type = lhs.type, },
				rhs,
				v.lhs[lhs_i].type,
			)
			spv.OpStore(builder, lhs.id, value)
		}
	case ^ast.Stmt_Expr:
		cg_expr(ctx, builder, v.expr)

	case ^ast.Decl_Value:
		cg_decl(ctx, builder, v, global)
	}

	return false
}

cg_stmt_list :: proc(ctx: ^CG_Context, builder: ^spv.Builder, stmts: []^ast.Stmt, global := false) -> (returned: bool) {
	for stmt in stmts {
		if cg_stmt(ctx, builder, stmt, global) {
			returned = true
		}
	}
	return returned
}
