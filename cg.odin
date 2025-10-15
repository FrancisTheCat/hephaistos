package hephaistos

import "base:intrinsics"

import "core:fmt"
import "core:reflect"
import "core:slice"

import "ast"
import "types"

import spv "spirv-odin"

CG_Type_Info :: struct {
	type:      spv.Id,
	nil_value: spv.Id,
	ptr_type:  spv.Id,
}

Type_Hash_Entry :: struct {
	hash: u64,
	type: ^types.Type,
	info: CG_Type_Info,
}

Type_Registry :: [][dynamic]Type_Hash_Entry

CG_Context :: struct {
	constant_cache:   map[types.Const_Value]spv.Id,
	type_registry:    Type_Registry,

	meta:             spv.Builder,
	entry_points:     spv.Builder,
	exectution_modes: spv.Builder,
	debug_a:          spv.Builder,
	debug_b:          spv.Builder,
	annotations:      spv.Builder,
	types:            spv.Builder,
	functions:        spv.Builder,

	current_id:       spv.Id,
	checker:          ^Checker,
	scopes:           [dynamic]CG_Scope,
}

CG_Entity :: struct {
	kind:  Entity_Kind,
	type:  ^types.Type,
	value: CG_Value,
}

CG_Value :: struct {
	id:  spv.Id,
	ptr: bool,
}

CG_Scope :: struct {
	entities: map[string]CG_Entity,
	label:    string,
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

cg_insert_entity :: proc(ctx: ^CG_Context, name: string, kind: Entity_Kind, type: ^types.Type, id: spv.Id) {
	ctx.scopes[len(ctx.scopes) - 1].entities[name] = {
		kind  = kind,
		type  = type,
		value = {
			id  = id,
			ptr = kind == .Var,
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

cg_init :: proc(ctx: ^CG_Context, checker: ^Checker) {
	ctx.checker                     = checker
	ctx.meta.current_id             = &ctx.current_id
	ctx.entry_points.current_id     = &ctx.current_id
	ctx.exectution_modes.current_id = &ctx.current_id
	ctx.debug_a.current_id          = &ctx.current_id
	ctx.debug_b.current_id          = &ctx.current_id
	ctx.annotations.current_id      = &ctx.current_id
	ctx.types.current_id            = &ctx.current_id
	ctx.functions.current_id        = &ctx.current_id

	ctx.type_registry = make([][dynamic]Type_Hash_Entry, 1024)

	append(&ctx.meta.data, spv.MAGIC_NUMBER)
	append(&ctx.meta.data, spv.VERSION)
	append(&ctx.meta.data, 'H' << 0 | 'E' << 8 | 'P' << 16 | 'H' << 24)
	append(&ctx.meta.data, 4194303)
	append(&ctx.meta.data, 0)
	cg_scope_push(ctx)

	void := spv.OpTypeVoid(&ctx.types)
	assert(void == 1)
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

cg_decl :: proc(ctx: ^CG_Context, builder: ^spv.Builder, decl: ^ast.Decl) {
	switch v in decl.derived_decl {
	case ^ast.Decl_Value:
		if v.mutable {
			if len(v.values) == 0 {
				for type, i in v.types {
					type_info := cg_type(ctx, type)
					id        := spv.OpVariable(&ctx.functions, type_info.ptr_type, .Function, type_info.nil_value)
					name      := v.lhs[i].derived_expr.(^ast.Expr_Ident).ident.text
					cg_insert_entity(ctx, name, .Var, type, id)
				}
			} else {
				for value, i in v.values {
					type      := v.types[i]
					type_info := cg_type(ctx, type)
					id        := spv.OpVariable(&ctx.functions, type_info.ptr_type, .Function, type_info.nil_value)
					init      := cg_expr(ctx, builder, value)
					spv.OpStore(builder, id, init)
					name      := v.lhs[i].derived_expr.(^ast.Expr_Ident).ident.text
					cg_insert_entity(ctx, name, .Var, type, id)
				}
			}
		} else {
			for value, i in v.values {
				if value.type.kind != .Proc {
					continue
				}
				id   := cg_expr(ctx, builder, value)
				name := v.lhs[i].derived_expr.(^ast.Expr_Ident).ident.text
				cg_insert_entity(ctx, name, .Proc, value.type, id)
			}
		}
	}
}

@(require_results)
cg_generate :: proc(ctx: ^CG_Context, stmts: []^ast.Stmt) -> []u32 {
	// capabilities
	spv.OpCapability(&ctx.meta, .Shader)

	// These should only be emitted when neccessary prbly
	spv.OpCapability(&ctx.meta, .Int64)
	spv.OpCapability(&ctx.meta, .Int16)
	spv.OpCapability(&ctx.meta, .Int8)
	spv.OpCapability(&ctx.meta, .Float64)
	spv.OpCapability(&ctx.meta, .Float16)

	// memory model
	spv.OpMemoryModel(&ctx.meta, .Logical, .Simple)

	b: spv.Builder = { current_id = &ctx.current_id }
	cg_stmt_list(ctx, &b, stmts)

	spirv := slice.concatenate([][]u32{
		ctx.meta.data[:],
		ctx.entry_points.data[:],
		ctx.exectution_modes.data[:],
		ctx.debug_a.data[:],
		ctx.debug_b.data[:],
		ctx.annotations.data[:],
		ctx.types.data[:],
		ctx.functions.data[:],
		b.data[:],
	})
	spirv[spv.ID_BOUND_INDEX] = u32(ctx.current_id + 1)
	return spirv
}

cg_constant :: proc(ctx: ^CG_Context, value: types.Const_Value) -> spv.Id {
	if id, ok := ctx.constant_cache[value]; ok {
		return id
	}

	id: spv.Id
	switch v in value {
	case i64:
		id = spv.OpConstant(&ctx.types, cg_type(ctx, types.t_i32).type, u32(v))
	case f64:
		id = spv.OpConstant(&ctx.types, cg_type(ctx, types.t_f32).type, transmute(u32)f32(v))
	case bool:
		if v {
			id = spv.OpConstantTrue(&ctx.types, cg_type(ctx, types.t_bool).type)
		} else {
			id = spv.OpConstantFalse(&ctx.types, cg_type(ctx, types.t_bool).type)
		}
	}
	return id
}

cg_type :: proc(ctx: ^CG_Context, type: ^types.Type) -> CG_Type_Info {
	assert(type != nil)

	registry := &ctx.type_registry
	info, ok := register_type(registry, type)
	if ok {
		assert(info.type      != 0)
		assert(info.nil_value != 0 || info.type == 1)
		assert(info.ptr_type  != 0 || info.type == 1)

		return info^
	}

	VOID :: 1

	#partial switch type.kind {
	case .Uint:
		info.type = spv.OpTypeInt(&ctx.types, u32(type.size * 8), 0)
	case .Int:
		info.type = spv.OpTypeInt(&ctx.types, u32(type.size * 8), 1)
	case .Float:
		info.type = spv.OpTypeFloat(&ctx.types, u32(type.size * 8))
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
		} else {
			info.type = VOID
		}
	case .Vector:
		type_vector := type.variant.(^types.Vector)
		info.type = spv.OpTypeVector(&ctx.types, cg_type(ctx, type_vector.elem).type, u32(type_vector.count))
	case .Matrix:
		type_matrix := type.variant.(^types.Matrix)
		info.type = spv.OpTypeMatrix(&ctx.types, cg_type(ctx, type_matrix.col_type).type, u32(type_matrix.cols))
	case .Proc:
		type_proc := type.variant.(^types.Proc)
		args      := make([]spv.Id, len(type_proc.args))
		for &arg, i in args {
			arg = cg_type(ctx, type_proc.args[i].type).type
		}
		info.type = spv.OpTypeFunction(&ctx.types, cg_type(ctx, type_proc.return_type).type, ..args)
	}

	if info.type != VOID && type.kind != .Proc {
		info.nil_value = spv.OpConstantNull(&ctx.types, info.type)
		info.ptr_type  = spv.OpTypePointer(&ctx.types, .Function, info.type)
	}

	return info^
}

cg_proc_lit :: proc(ctx: ^CG_Context, p: ^ast.Expr_Proc_Lit) -> (id: spv.Id) {
	type := p.type.variant.(^types.Proc)
	id    = spv.OpFunction(&ctx.functions, cg_type(ctx, type.return_type).type, {}, cg_type(ctx, type).type)
	cg_scope_push(ctx)
	for arg, i in type.args {
		id := spv.OpFunctionParameter(&ctx.functions, cg_type(ctx, arg.type).type)
		cg_insert_entity(ctx, p.args[i].ident.text, .Param, arg.type, id)
	}
	spv.OpLabel(&ctx.functions)
	if len(type.args) == 0 {
		spv.OpEntryPoint(&ctx.entry_points, .Vertex, id, "main")
	}
	body     := spv.Builder { current_id = &ctx.current_id, }
	returned := cg_stmt_list(ctx, &body, p.body)
	if !returned {
		spv.OpReturn(&body)
	}
	append(&ctx.functions.data, ..body.data[:])
	spv.OpFunctionEnd(&ctx.functions)

	return
}

cg_expr :: proc(ctx: ^CG_Context, builder: ^spv.Builder, expr: ^ast.Expr, deref := true) -> spv.Id {
	assert(expr != nil)

	if expr.const_value != nil {
		return cg_constant(ctx, expr.const_value)
	}
	start := len(builder.data)
	defer if len(builder.data) != start {
		fmt.println(transmute([2]u16)builder.data[start], builder.data[start + 1:])
	}

	switch v in expr.derived_expr {
	case ^ast.Expr_Constant:
		return cg_constant(ctx, expr.const_value)
	case ^ast.Expr_Binary:
		lhs, rhs := cg_expr(ctx, builder, v.lhs), cg_expr(ctx, builder, v.rhs)
		type := cg_type(ctx, v.type).type

		if op_is_relation(v.op) {
			t := types.op_result_type(v.lhs.type, v.rhs.type)
			#partial switch t.kind {
			case .Int:
				#partial switch v.op {
				case .Equal:
					return spv.OpIEqual(builder, type, lhs, rhs)
				case .Not_Equal:
					return spv.OpINotEqual(builder, type, lhs, rhs)
				case .Less:
					return spv.OpSLessThan(builder, type, lhs, rhs)
				case .Less_Equal:
					return spv.OpSLessThanEqual(builder, type, lhs, rhs)
				case .Greater:
					return spv.OpSGreaterThan(builder, type, lhs, rhs)
				case .Greater_Equal:
					return spv.OpSGreaterThanEqual(builder, type, lhs, rhs)
				}
			case .Uint:
				#partial switch v.op {
				case .Equal:
					return spv.OpIEqual(builder, type, lhs, rhs)
				case .Not_Equal:
					return spv.OpINotEqual(builder, type, lhs, rhs)
				case .Less:
					return spv.OpULessThan(builder, type, lhs, rhs)
				case .Less_Equal:
					return spv.OpULessThanEqual(builder, type, lhs, rhs)
				case .Greater:
					return spv.OpUGreaterThan(builder, type, lhs, rhs)
				case .Greater_Equal:
					return spv.OpUGreaterThanEqual(builder, type, lhs, rhs)
				}
			case .Float:
				#partial switch v.op {
				case .Equal:
					return spv.OpFOrdEqual(builder, type, lhs, rhs)
				case .Not_Equal:
					return spv.OpFOrdNotEqual(builder, type, lhs, rhs)
				case .Less:
					return spv.OpFOrdLessThan(builder, type, lhs, rhs)
				case .Less_Equal:
					return spv.OpFOrdLessThanEqual(builder, type, lhs, rhs)
				case .Greater:
					return spv.OpFOrdGreaterThan(builder, type, lhs, rhs)
				case .Greater_Equal:
					return spv.OpFOrdGreaterThanEqual(builder, type, lhs, rhs)
				}
			case .Bool:
				#partial switch v.op {
				case .Equal:
					return spv.OpLogicalEqual(builder, type, lhs, rhs)
				case .Not_Equal:
					return spv.OpLogicalNotEqual(builder, type, lhs, rhs)
				}
			}
			panic("")
		}

		#partial switch v.type.kind {
		case .Int:
			#partial switch v.op {
			case .Add:
				return spv.OpIAdd(builder, type, lhs, rhs)
			case .Subtract:
				return spv.OpISub(builder, type, lhs, rhs)
			case .Multiply:
				return spv.OpIMul(builder, type, lhs, rhs)
			case .Divide:
				return spv.OpSDiv(builder, type, lhs, rhs)
			case .Modulo:
				return spv.OpSMod(builder, type, lhs, rhs)
			}
		case .Uint:
			#partial switch v.op {
			case .Add:
				return spv.OpIAdd(builder, type, lhs, rhs)
			case .Subtract:
				return spv.OpISub(builder, type, lhs, rhs)
			case .Multiply:
				return spv.OpIMul(builder, type, lhs, rhs)
			case .Divide:
				return spv.OpUDiv(builder, type, lhs, rhs)
			case .Modulo:
				return spv.OpUMod(builder, type, lhs, rhs)
			}
		case .Float:
			#partial switch v.op {
			case .Add:
				return spv.OpFAdd(builder, type, lhs, rhs)
			case .Subtract:
				return spv.OpFSub(builder, type, lhs, rhs)
			case .Multiply:
				return spv.OpFMul(builder, type, lhs, rhs)
			case .Divide:
				return spv.OpFDiv(builder, type, lhs, rhs)
			case .Modulo:
				return spv.OpFMod(builder, type, lhs, rhs)
			}
		case .Bool:
			#partial switch v.op {
			case .And:
				return spv.OpLogicalAnd(builder, type, lhs, rhs)
			case .Or:
				return spv.OpLogicalAnd(builder, type, lhs, rhs)
			}
			unimplemented()
		case .Matrix:
			unimplemented()
		case .Vector:
			if v.lhs.type.kind == .Vector && v.lhs.type.kind == .Vector {
				panic("")
			}

			if v.rhs.type.kind == .Vector {
				lhs, rhs = rhs, lhs
			}

			#partial switch v.op {
			case .Add:
				return spv.OpFAdd(builder, type, lhs, rhs)
			case .Subtract:
				return spv.OpFSub(builder, type, lhs, rhs)
			case .Multiply:
				return spv.OpFMul(builder, type, lhs, rhs)
			case .Divide:
				return spv.OpFDiv(builder, type, lhs, rhs)
			case .Modulo:
				return spv.OpFMod(builder, type, lhs, rhs)
			}

		case:
			panic("")
		}
	case ^ast.Expr_Ident:
		value := cg_lookup_entity(ctx, v.ident.text).value
		if !deref {
			assert(value.ptr)
			return value.id
		}
		if value.ptr {
			return spv.OpLoad(builder, cg_type(ctx, v.type).type, value.id)
		} else {
			return value.id
		}
	case ^ast.Expr_Proc_Lit:
		return cg_proc_lit(ctx, v)
	case ^ast.Expr_Proc_Sig:
	case ^ast.Expr_Paren:
	case ^ast.Expr_Selector:
		lhs := cg_expr(ctx, builder, v.lhs, false)

		lhs_type := v.lhs.type
		assert(lhs_type.kind == .Struct)

		field_index: int = -1
		for field, i in lhs_type.variant.(^types.Struct).fields {
			if field.name.text == v.selector.text {
				field_index = i
				break
			}
		}
		assert(field_index != -1)

		ptr := spv.OpAccessChain(builder, cg_type(ctx, v.type).ptr_type, lhs, cg_constant(ctx, i64(field_index)))

		if deref {
			return spv.OpLoad(builder, cg_type(ctx, v.type).type, ptr)
		} else {
			return ptr
		}

	case ^ast.Expr_Call:
		if v.is_cast {
			
		} else {
			fn   := cg_expr(ctx, builder, v.lhs)
			args := make([]spv.Id, len(v.args))
			for &arg, i in args {
				arg = cg_expr(ctx, builder, v.args[i].value)
			}
			return spv.OpFunctionCall(builder, cg_type(ctx, expr.type).type, fn, ..args)
		}
	case ^ast.Expr_Compound:
	case ^ast.Expr_Index:
		lhs := cg_expr(ctx, builder, v.lhs)
		rhs := cg_expr(ctx, builder, v.rhs)

		return spv.OpAccessChain(builder, cg_type(ctx, v.type).type, lhs, rhs)
	case ^ast.Expr_Cast:
	case ^ast.Expr_Unary:
	
	case ^ast.Type_Struct, ^ast.Type_Array, ^ast.Type_Matrix:
		panic("")
	}

	if true do fmt.panicf("unimplemented: %v", reflect.union_variant_typeid(expr.derived_expr))
	return 0
}

// @(require_results)
// cg_scope :: proc(ctx: ^CG_Context, stmts: []^ast.Stmt, label: string = "") -> (instructions: []u32, returned: bool) {
// 	cg_scope_push(ctx, label)
// 	defer cg_scope_pop(ctx)

// 	c := ctx.current
// 	defer ctx.current = c

// 	ctx.current.data       = make([dynamic]u32)
// 	ctx.current.current_id = &ctx.current_id

// 	returned     = cg_stmt_list(ctx, stmts)
// 	instructions = ctx.current.data[:]
// 	return
// }

cg_stmt :: proc(ctx: ^CG_Context, builder: ^spv.Builder, stmt: ^ast.Stmt) -> (returned: bool) {
	if stmt == nil {
		return
	}

	switch v in stmt.derived_stmt {
	case ^ast.Stmt_Return:
		spv.OpReturn(builder)
		return true
	case ^ast.Stmt_Break:
	case ^ast.Stmt_Continue:
	case ^ast.Stmt_For_Range:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

	case ^ast.Stmt_For:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

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

		cg_scope_push(ctx)
		true_label := spv.OpLabel(then_block)
		cg_stmt_list(ctx, then_block, v.then_block)
		spv.OpBranch(then_block, end_label)
		cg_scope_pop(ctx)
		
		cg_scope_push(ctx)
		false_label := spv.OpLabel(else_block)
		cg_stmt_list(ctx, else_block, v.else_block)
		spv.OpBranch(else_block, end_label)
		cg_scope_pop(ctx)

		spv.OpSelectionMerge(builder, end_label, {})
		spv.OpBranchConditional(builder, cond, true_label, false_label)

		append(&builder.data, ..then_block.data[:])
		append(&builder.data, ..else_block.data[:])
		append(&builder.data, ..end_block.data[:])
	case ^ast.Stmt_Switch:
		cg_scope_push(ctx, v.label.text)
		defer cg_scope_pop(ctx)

	case ^ast.Stmt_Assign:
		for value, i in v.rhs {
			id := cg_expr(ctx, builder, v.lhs[i], deref = false)
			v  := cg_expr(ctx, builder, value)
			spv.OpStore(builder, id, v)
		}
	case ^ast.Stmt_Expr:
		cg_expr(ctx, builder, v.expr)

	case ^ast.Decl_Value:
		cg_decl(ctx, builder, v)
	}

	return false
}

cg_stmt_list :: proc(ctx: ^CG_Context, builder: ^spv.Builder, stmts: []^ast.Stmt) -> (returned: bool) {
	for stmt in stmts {
		if cg_stmt(ctx, builder, stmt) {
			returned = true
		}
	}
	return returned
}
