package hephaistos

import "base:intrinsics"
import "base:runtime"

import "core:fmt"
import "core:mem"

import "ast"
import "tokenizer"
import "types"

Checker :: struct {
	scope:           ^Scope,
	ast:             ast.File,
	errors:          [dynamic]tokenizer.Error,
	error_allocator: runtime.Allocator,
}

Addressing_Mode :: enum {
	Invalid = 0,
	No_Value,
	RValue,
	LValue,
	Const,
	Type,
	Builtin,
}

@(rodata)
addressing_mode_string := [Addressing_Mode]string {
	.Invalid  = "<invalid>",
	.No_Value = "no value",
	.RValue   = "rvalue",
	.LValue   = "lvalue",
	.Const    = "const",
	.Type     = "type",
	.Builtin  = "builtin",
}

Operand :: struct {
	expr:   ^ast.Expr,
	type:   ^types.Type,
	mode:    Addressing_Mode,
	value:   types.Const_Value,
	is_call: bool,
}

Scope_Proc_Info :: struct {
	type: ^types.Proc,
	lit:  ^ast.Expr_Proc_Lit,
}

Scope :: struct {
	parent:    ^Scope,
	elements:  map[string]^Entity,
	procedure: Maybe(Scope_Proc_Info),
	label:     Maybe(tokenizer.Token),
	kind:      Scope_Kind,
}

Scope_Kind :: enum {
	Global,
	Proc,
	Block, // if or {}
	Loop,
	Switch,
}

@(require_results)
scope_new :: proc(parent: ^Scope, kind: Scope_Kind) -> ^Scope {
	s, _ := new(Scope)
	s.parent = parent
	s.kind   = kind
	return s
}

@(require_results)
scope_lookup_current :: proc(s: ^Scope, name: string) -> (e: ^Entity, ok: bool) {
	return s.elements[name]
}

@(require_results)
scope_lookup :: proc(checker: ^Checker, name: string) -> (e: ^Entity, ok: bool) {
	s := checker.scope
	for s != nil {
		e, ok = scope_lookup_current(s, name)
		if ok {
			return
		}
		s = s.parent
	}
	return
}

@(require_results)
scope_lookup_label :: proc(checker: ^Checker, name: string) -> (s: ^Scope, ok: bool) {
	s = checker.scope
	for s != nil {
		if label, ok := s.label.?; ok && label.text == name {
			return s, true
		}
		s = s.parent
	}

	return
}

@(require_results)
lookup_proc_type :: proc(checker: ^Checker) -> (e: ^types.Proc, ok: bool) {
	s := checker.scope
	for s != nil {
		p, ok := s.procedure.?
		if ok {
			return p.type, true
		}
		s = s.parent
	}

	return
}

@(require_results)
lookup_scope_by_kind :: proc(checker: ^Checker, mask: bit_set[Scope_Kind]) -> (s: ^Scope, ok: bool) {
	s = checker.scope
	for s != nil {
		if s.kind in mask {
			return s, true
		}
		s = s.parent
	}

	return
}

scope_push :: proc(c: ^Checker, kind: Scope_Kind) -> ^Scope {
	c.scope = scope_new(c.scope, kind)
	return c.scope
}

scope_pop :: proc(c: ^Checker) -> ^Scope {
	s := c.scope
	c.scope = s.parent
	return s
}

scope_insert_entity :: proc(checker: ^Checker, e: ^Entity) -> bool {
	if e == nil {
		return true
	}

	assert(e.name  != "")
	assert(checker.scope != nil)
	if e.name in checker.scope.elements {
		error(checker, e.ident, "'%s' has already been defined in this scope", e.name)
		return false
	}

	checker.scope.elements[e.name] = e
	return true
}

check_stmt :: proc(checker: ^Checker, stmt: ^ast.Stmt) -> (diverging: bool) {
	switch v in stmt.derived_stmt {
	case ^ast.Stmt_Return:
		proc_type, ok := lookup_proc_type(checker)
		if !ok {
			error(checker, v, "unexpected return statement outside of procedure body")
			return true
		}
		return_index := 0
		for e in v.values {
			value := check_expr(checker, e)
			if return_index >= len(proc_type.returns) {
				continue
			}

			if value.type.kind == .Tuple {
				for type in value.type.variant.(^types.Struct).fields {
					if types.op_result_type(type.type, proc_type.returns[return_index].type).kind == .Invalid {
						error(checker, value, "mismatched type in return statement: %v vs %v", proc_type.returns[return_index].type, type.type)
					}
					return_index += 1
				}
			} else {
				if types.op_result_type(value.type, proc_type.returns[return_index].type).kind == .Invalid {
					error(checker, value, "mismatched type in return statement: %v vs %v", proc_type.returns[return_index].type, value.type)
				}
				return_index += 1
			}
		}

		if return_index != 0 && return_index != len(proc_type.returns) {
			error(checker, v, "expected %d values for return statements but got %d", len(proc_type.returns), return_index)
		}

		return true
	case ^ast.Stmt_Break:
		if v.label.text != "" {
			_, ok := scope_lookup_label(checker, v.label.text)
			if !ok {
				error(checker, v, "unkown label: '%s'", v.label.text)
			}
		} else {
			_, ok := lookup_scope_by_kind(checker, { .Loop, .Switch, })
			if !ok {
				error(checker, v, "break can only be used in loops and switches")
			}
		}

		return true
	case ^ast.Stmt_Continue:
		if v.label.text != "" {
			scope, ok := scope_lookup_label(checker, v.label.text)
			if !ok {
				error(checker, v, "unkown label: '%s'", v.label.text)
			}
			if scope.kind != .Loop {
				error(checker, v, "continue can only be used in loops")
			}
		} else {
			_, ok := lookup_scope_by_kind(checker, { .Loop, })
			if !ok {
				error(checker, v, "continue can only be used in loops")
			}
		}

		return true
	case ^ast.Stmt_For_Range:
		scope_push(checker, .Loop).label = v.label
		defer scope_pop(checker)

		return check_stmt_list(checker, v.body)
	case ^ast.Stmt_For:
		scope_push(checker, .Loop).label = v.label
		defer scope_pop(checker)

		if v.init != nil {
			check_stmt(checker, v.init)
		}

		if v.cond != nil {
			cond := check_expr(checker, v.cond)
			if cond.type.kind != .Bool {
				error(checker, cond, "expected a boolean expression in if statement condition but got: %v", cond.type)
			}
		}

		if v.post != nil {
			check_stmt(checker, v.post)
		}

		return check_stmt_list(checker, v.body)

	case ^ast.Stmt_Block:
		scope_push(checker, .Block).label = v.label
		defer scope_pop(checker)

		return check_stmt_list(checker, v.body)
	case ^ast.Stmt_If:
		scope_push(checker, .Block).label = v.label
		defer scope_pop(checker)

		if v.init != nil {
			check_stmt(checker, v.init)
		}

		cond := check_expr(checker, v.cond)
		if cond.type.kind != .Bool {
			error(checker, cond, "expected a boolean expression in if statement condition but got: %v", cond.type)
		}

		then_diverging := check_stmt_list(checker, v.then_block)
		else_diverging := check_stmt_list(checker, v.else_block)
		return then_diverging && else_diverging
	case ^ast.Stmt_Switch:
		scope_push(checker, .Block).label = v.label
		defer scope_pop(checker)

		if v.init != nil {
			check_stmt(checker, v.init)
		}

		cond := check_expr(checker, v.cond)
		cond.type = types.default_type(cond.type)
		seen_default := false
		for c in v.cases {
			if c.value == nil {
				if seen_default {
					error(checker, c.token, "switch statement can only have one default case")
					seen_default = true
				}

				scope_push(checker, .Switch).label = v.label
				defer scope_pop(checker)

				check_stmt_list(checker, c.body)
				continue
			}

			scope_push(checker, .Switch).label = v.label
			defer scope_pop(checker)

			value := check_expr(checker, c.value)
			if types.op_result_type(value.type, cond.type).kind == .Invalid {
				error(checker, value, "type of case value does not match selector type: %v vs %v", cond.type, value.type)
			}
			check_stmt_list(checker, c.body)
		}

	case ^ast.Stmt_Assign:
		lhs := make([]Operand, len(v.lhs))
		rhs := make([]Operand, len(v.rhs))
		for &lhs, i in lhs {
			lhs = check_expr(checker, v.lhs[i])
		}
		for &rhs, i in rhs {
			rhs = check_expr(checker, v.rhs[i])
		}

		for &l in lhs {
			if l.mode != .LValue {
				error(checker, v, "cannot assign to %s expression", addressing_mode_string[l.mode])
			}
		}

		v.types = make([]^types.Type, len(lhs))

		lhs_i := 0
		check_assignment_types: for &r, rhs_i in rhs {
			if r.type.kind == .Tuple {
				for field in r.type.variant.(^types.Struct).fields {
					if lhs_i >= len(lhs) {
						lhs_i += 1
						continue
					}
					result_type := types.op_result_type(lhs[lhs_i].type, field.type)
					if result_type.kind == .Invalid {
						error(checker, v, "mismatched types in assign statement: %v vs %v", lhs[lhs_i].type, field.type)
					}
					v.types[lhs_i] = result_type
					lhs_i += 1
				}
			} else {
				if lhs_i >= len(lhs) {
					lhs_i += 1
					continue
				}
				result_type := types.op_result_type(lhs[lhs_i].type, r.type)
				if result_type.kind == .Invalid {
					error(checker, v, "mismatched types in assign statement: %v vs %v", lhs[lhs_i].type, r.type)
				} else if types.is_float(result_type) && types.is_integer(r.type) {
					v.rhs[rhs_i].const_value = f64(v.rhs[rhs_i].const_value.(i64))
				}
				v.types[lhs_i] = result_type
				lhs_i += 1
			}
		}
		if lhs_i != len(lhs) {
			error(checker, v, "assignment count mismatch: %v vs %v", len(lhs), lhs_i)
		}
		
	case ^ast.Stmt_Expr:
		operand := check_expr(checker, v.expr)
		if !operand.is_call {
			error(checker, v.expr, "expression is not used")
		}

	case ^ast.Decl_Value:
		names  := make([]tokenizer.Token, len(v.lhs))
		values := make([]Operand,         len(v.values))

		for &name, i in names {
			if ident, ok := v.lhs[i].derived_expr.(^ast.Expr_Ident); ok {
				name = ident.ident
			} else {
				error(checker, v.lhs[i], "variable declaration must be an identifier")
			}
		}
		for &values, i in values {
			values = check_expr_or_type(checker, v.values[i], stmt.attributes)
		}

		v.types = make([]^types.Type, len(v.lhs))

		if len(values) == 0 {
			type := check_type(checker, v.type_expr)
			for name in names {
				entity_kind := Entity_Kind.Var
				scope_insert_entity(checker, entity_new(entity_kind, name, type, decl = v))
			}
			for &t in v.types {
				t = type
			}
			return
		}

		explicit_type: ^types.Type
		if v.type_expr != nil {
			explicit_type = check_type(checker, v.type_expr)
		}

		name_i := 0
		check_decl_types: for &r in values {
			if r.type.kind == .Tuple {
				for field in r.type.variant.(^types.Struct).fields {
					if name_i >= len(names) {
						name_i += 1
						continue
					}
					name        := names[name_i]
					entity_kind := Entity_Kind.Var
					if !v.mutable {
						entity_kind = .Const
						if r.mode == .Type {
							entity_kind = .Type
						}
					}

					type := explicit_type
					if type == nil {
						type = field.type
						if entity_kind != .Const {
							type = types.default_type(type)
						}
					} else {
						if types.op_result_type(explicit_type, field.type).kind == .Invalid {
							error(checker, stmt, "mismatched types in value declaration: %v vs %v", explicit_type, field.type)
						}
					}
					v.types[name_i] = type

					scope_insert_entity(checker, entity_new(entity_kind, name, type, decl = v))
					name_i += 1
				}
			} else {
				if name_i >= len(names) {
					name_i += 1
					continue
				}
				name        := names[name_i]
				entity_kind := Entity_Kind.Var
				if !v.mutable {
					entity_kind = .Const
					if r.mode == .Type {
						entity_kind = .Type
					}
				}

				type := explicit_type
				if type == nil {
					type = r.type
					if entity_kind != .Const {
						type = types.default_type(type)
					}
				} else {
					if types.op_result_type(explicit_type, r.type).kind == .Invalid {
						error(checker, stmt, "mismatched types in value declaration: %v vs %v", explicit_type, r.type)
					}
					if types.is_float(explicit_type) {
						if types.is_integer(r.type) {
							r.expr.const_value = f64(r.expr.const_value.(i64))
						}
					}
				}
				v.types[name_i] = type

				scope_insert_entity(checker, entity_new(entity_kind, name, type, decl = v))
				name_i += 1
			}
		}
		if name_i != len(names) {
			error(checker, v, "assignment count mismatch: %v vs %v", len(names), name_i)
		}
	}

	diverging = false
	return
}

check_stmt_list :: proc(checker: ^Checker, stmts: []^ast.Stmt) -> (diverging: bool) {
	for stmt, i in stmts {
		d := check_stmt(checker, stmt)
		if d && !diverging && i != len(stmts) - 1 {
			error(checker, stmt, "statements after this statement are never executed")
			diverging = true
		}
	}

	return diverging
}

checker_init :: proc(checker: ^Checker) {
	scope_push(checker, .Global)

	scope_insert_entity(checker, entity_new(.Type, { text = "bool" }, types.t_bool))

	scope_insert_entity(checker, entity_new(.Type, { text = "i8"   }, types.t_i8 ))
	scope_insert_entity(checker, entity_new(.Type, { text = "i16"  }, types.t_i16))
	scope_insert_entity(checker, entity_new(.Type, { text = "i32"  }, types.t_i32))
	scope_insert_entity(checker, entity_new(.Type, { text = "i64"  }, types.t_i64))

	scope_insert_entity(checker, entity_new(.Type, { text = "u8"   }, types.t_u8 ))
	scope_insert_entity(checker, entity_new(.Type, { text = "u16"  }, types.t_u16))
	scope_insert_entity(checker, entity_new(.Type, { text = "u32"  }, types.t_u32))
	scope_insert_entity(checker, entity_new(.Type, { text = "u64"  }, types.t_u64))

	scope_insert_entity(checker, entity_new(.Type, { text = "f32"  }, types.t_f32))
	scope_insert_entity(checker, entity_new(.Type, { text = "f64"  }, types.t_f64))
}

op_is_relation :: proc(token_kind: tokenizer.Token_Kind) -> bool {
	#partial switch token_kind {
	case .Equal, .Not_Equal, .Less_Equal, .Greater_Equal, .Less, .Greater:
		return true
	}
	return false
}

evaluate_const_binary_op :: proc(checker: ^Checker, lhs, rhs: types.Const_Value, expr: ^ast.Expr_Binary) -> types.Const_Value {
	assert(lhs != nil)
	assert(rhs != nil)

	lhs := lhs
	rhs := rhs

	lhs_tag := (^intrinsics.type_union_tag_type(types.Const_Value))(uintptr(&lhs) + intrinsics.type_union_tag_offset(types.Const_Value))^
	rhs_tag := (^intrinsics.type_union_tag_type(types.Const_Value))(uintptr(&rhs) + intrinsics.type_union_tag_offset(types.Const_Value))^

	// Const_Value :: union {
	// 	i64,  tag = 1
	// 	f64,  tag = 2
	// 	bool, tag = 3
	// }
	if lhs_tag != rhs_tag {
		if lhs_tag < 3 && rhs_tag < 3 {
			if lhs_int, ok := lhs.(i64); ok {
				lhs = f64(lhs_int)
			} else if rhs_int, ok := rhs.(i64); ok {
				rhs = f64(rhs_int)
			}
		}
	}

	type_assert_2 :: proc(lhs, rhs: $V, $T: typeid) -> (T, T, bool) {
		x, x_ok := lhs.(T)
		y, y_ok := rhs.(T)
		return x, y, x_ok & y_ok
	}

	if l, r, ok := type_assert_2(lhs, rhs, i64); ok {
		#partial switch expr.op {
		case .Bit_And:
			return l & r
		case .Bit_Or:
			return l | r
		case .Xor:
			return l ~ r
		case .Add:
			return l + r
		case .Subtract:
			return l - r
		case .Multiply:
			return l * r
		case .Divide:
			if r == 0 {
				error(checker, expr, "division by zero")
				return nil
			}
			return l / r
		case .Modulo:
			if r == 0 {
				error(checker, expr, "modulo with zero")
				return nil
			}
			return l % r
		case .Exponent:
			unimplemented()
		case .Less:
			return l < r
		case .Greater:
			return l > r

		case .Equal:
			return l == r
		case .Not_Equal:
			return l != r
		case .Less_Equal:
			return l <= r
		case .Greater_Equal:
			return l >= r

		case .Shift_Left:
			if r < 0 {
				error(checker, expr, "shift by a negative amount: %v < 0", r)
			}
			return l << uint(r)
		case .Shift_Right:
			if r < 0 {
				error(checker, expr, "shift by a negative amount: %v < 0", r)
			}
			return l >> uint(r)

		case .Modulo_Floored:
			return l %% r
		}
	}

	if l, r, ok := type_assert_2(lhs, rhs, f64); ok {
		#partial switch expr.op {
		case .Add:
			return l + r
		case .Subtract:
			return l - r
		case .Multiply:
			return l * r
		case .Divide:
			return l / r
		case .Exponent:
			unimplemented()
		case .Less:
			return l < r
		case .Greater:
			return l > r

		case .Equal:
			return l == r
		case .Not_Equal:
			return l != r
		case .Less_Equal:
			return l <= r
		case .Greater_Equal:
			return l >= r
		}
	}

	if l, r, ok := type_assert_2(lhs, rhs, bool); ok {
		#partial switch expr.op {
		case .Bit_And:
			return l & r
		case .Bit_Or:
			return l | r
		case .Xor:
			return l ~ r
		case .Equal:
			return l == r
		case .Not_Equal:
			return l != r
		case .And:
			return l && r
		case .Or:
			return l || r
		}
	}

	error(checker, expr, "mismatched types in constant binary expression: %v vs %v", lhs, rhs)
	return nil
}

check_proc_type :: proc(checker: ^Checker, p: $T) -> ^types.Proc {
	args    := make([dynamic]types.Field, 0, len(p.args))
	returns := make([dynamic]types.Field, 0, len(p.returns))

	for arg in p.args {
		type := check_type(checker, arg.type)
		if arg.value != nil {
			value := check_expr(checker, arg.value)
			if types.op_result_type(type, value.type).kind == .Invalid {
				error(
					checker,
					arg.ident.location,
					arg.value.end,
					"default value type does not match declared type: %p vs %p",
					type,
					value.type,
				)
			}
		}

		append(&args, types.Field {
			name = arg.ident,
			type = type,
		})
	}

	for ret in p.returns {
		type := check_type(checker, ret.type)
		if ret.value != nil {
			value := check_expr(checker, ret.value)
			if types.op_result_type(type, value.type).kind == .Invalid {
				error(
					checker,
					ret.ident.location,
					ret.value.end,
					"default value type does not match declared type: %p vs %p",
					type,
					value.type,
				)
			}
		}

		append(&returns, types.Field {
			name = ret.ident,
			type = type,
		})
	}

	t        := types.type_new(.Proc, types.Proc)
	t.args    = args[:]
	t.returns = returns[:]

	return_type       := types.type_new(.Tuple, types.Struct)
	return_type.fields = returns[:]
	t.return_type      = return_type

	return t
}

check_expr_internal :: proc(checker: ^Checker, expr: ^ast.Expr, attributes: []ast.Field) -> (operand: Operand) {
	operand.expr = expr

	defer {
		expr.type        = operand.type
		expr.const_value = operand.value
	}

	switch v in expr.derived_expr {
	case ^ast.Expr_Constant:
		operand.mode = .Const

		switch val in v.value {
		case i64:
			operand.type  = types.t_int
			operand.value = val
		case f64:
			operand.type  = types.t_float
			operand.value = val
		case bool:
			operand.type  = types.t_bool
			operand.value = val
		}
		return

	case ^ast.Expr_Binary:
		// TODO: check applicability of operator
		lhs := check_expr(checker, v.lhs)
		rhs := check_expr(checker, v.rhs)

		operand.type = types.op_result_type(lhs.type, rhs.type)
		if operand.type.kind == .Invalid {
			error(checker, expr, "mismatched types in binary expression: %v vs %v", lhs.type, rhs.type)
			operand.mode = .Invalid
			return
		}
		if types.is_float(operand.type) {
			if types.is_integer(lhs.type) {
				lhs.expr.const_value = f64(lhs.value.(i64))
			}
			if types.is_integer(rhs.type) {
				rhs.expr.const_value = f64(rhs.value.(i64))
			}
		}
		if op_is_relation(v.op) {
			operand.type = types.t_bool
			operand.mode = .RValue
		} else if lhs.mode == .Const && rhs.mode == .Const {
			operand.mode  = .Const
			operand.value = evaluate_const_binary_op(checker, lhs.value, rhs.value, v)
		} else {
			operand.mode = .RValue
		}
		
	case ^ast.Expr_Ident:
		e, ok := scope_lookup(checker, v.ident.text)
		if !ok {
			error(checker, expr, "unkown identifier: '%s'", v.ident.text)
			operand.type = types.t_invalid
			operand.mode = .Invalid
			return
		}

		operand.type = e.type
		#partial switch e.kind {
		case .Const:
			operand.mode  = .Const
			operand.value = e.value
		case .Type:
			operand.mode  = .Type
		case .Var:
			operand.mode  = .LValue
		case .Param:
			operand.mode  = .RValue
		case .Proc:
		}

	case ^ast.Expr_Proc_Lit:
		shader_kind: ast.Shader_Kind
		for attribute in attributes {
			s: ast.Shader_Kind
			switch attribute.ident.text {
			case "vertex_shader":
				s = .Vertex 
			case "fragment_shader":
				s = .Fragment 
			case "geometry_shader":
				s = .Geometry 
			case "tesselation_shader":
				s = .Tesselation 
			case "compute_shader":
				s = .Compute 
			case:
				error(checker, attribute.ident, "unknown attribute: '%s'", attribute.ident.text)
			}
			if shader_kind != nil && s != nil {
				error(
					checker,
					attribute.ident,
					"the attributes %s and %s are mutually exclusive",
					ast.shader_kind_names[shader_kind],
					ast.shader_kind_names[s],
				)
			}
			shader_kind = s
		}
		v.shader_kind = shader_kind

		type := check_proc_type(checker, v)

		operand.type = type
		operand.mode = .RValue

		scope_push(checker, .Proc).procedure = Scope_Proc_Info { type = type, lit = v, }
		defer scope_pop(checker)

		for arg in type.args {
			if arg.name.text != "" {
				scope_insert_entity(checker, entity_new(.Param, arg.name, arg.type))
			}
		}

		scope_push(checker, .Block)
		defer scope_pop(checker)

		for ret in type.returns {
			if ret.name.text != "" {
				scope_insert_entity(checker, entity_new(.Var, ret.name, ret.type))
			}
		}
		check_stmt_list(checker, v.body)

	case ^ast.Expr_Proc_Sig:
		operand.type = check_proc_type(checker, v)
		operand.mode = .Type
	case ^ast.Expr_Paren:
		return check_expr(checker, v.expr)
	case ^ast.Expr_Selector:
		lhs := check_expr(checker, v.lhs)
		if lhs.type.kind != .Struct {
			error(checker, v, "expression of type %v has no field called '%s'", lhs.type, v.selector.text)
			return
		}

		found := false
		for field in lhs.type.variant.(^types.Struct).fields {
			if field.name.text == v.selector.text {
				operand.type = field.type
				operand.mode = lhs.mode
				found        = true
				break
			}
		}
		if !found {
			error(checker, v, "expression of type %v has no field called '%s'", lhs.type, v.selector.text)
		}

	case ^ast.Expr_Call:
		operand.is_call = true

		fn := check_expr_or_type(checker, v.lhs)
		if fn.mode == .Type {
			v.is_cast = true
			if len(v.args) != 1 {
				error(checker, v, "expected exactly one argument to function call syntax type cast, but got %d", len(v.args))
				return
			}
			value := check_expr(checker, v.args[0].value)
			if !types.castable(value.type, fn.type) {
				error(checker, v, "can not cast expression from type %v to %v", value.type, fn.type)
			}
			operand.type = fn.type
			operand.mode = .RValue
		} else {
			if fn.type.kind != .Proc {
				error(checker, v, "expected a function in call expression")
				return
			}

			proc_type := fn.type.variant.(^types.Proc)

			arg_index := 0
			for e in v.args {
				value := check_expr(checker, e.value)
				if arg_index >= len(proc_type.args) {
					continue
				}

				if value.type.kind == .Tuple {
					for type in value.type.variant.(^types.Struct).fields {
						if types.op_result_type(type.type, proc_type.args[arg_index].type).kind == .Invalid {
							error(checker, value, "mismatched type in at argument %d: %v vs %v", arg_index, proc_type.args[arg_index].type, value.type)
						}
						arg_index += 1
					}
				} else {
					if types.op_result_type(value.type, proc_type.args[arg_index].type).kind == .Invalid {
						error(checker, value, "mismatched type in at argument %d: %v vs %v", arg_index, proc_type.args[arg_index].type, value.type)
					}
					arg_index += 1
				}
			}

			if arg_index != len(proc_type.args) {
				error(checker, v, "expected %d arguments but got %d", len(proc_type.args), arg_index)
			}

			operand.type    = proc_type.return_type
			operand.mode    = .RValue
			operand.is_call = true
		}
	case ^ast.Expr_Compound:
		// TODO
	case ^ast.Expr_Index:
		lhs := check_expr(checker, v.lhs)
		rhs := check_expr(checker, v.rhs)
		if rhs.type.kind != .Int && rhs.type.kind != .Uint {
			error(checker, rhs, "expected an integer as the index, but got %v", rhs.type)
		}

		result_type := types.t_invalid
		#partial switch lhs.type.kind {
		case .Matrix:
			result_type = lhs.type.variant.(^types.Matrix).col_type
		case .Vector:
			result_type = lhs.type.variant.(^types.Vector).elem
		}

		if result_type.kind == .Invalid {
			error(checker, v, "expression of type %v can not be indexed", lhs.type)
			return
		}

		operand.type = result_type
		operand.mode = lhs.mode
		
	case ^ast.Expr_Cast:
		value       := check_expr(checker, v.value)
		operand.type = check_type(checker, v.type_expr)
		if !types.castable(value.type, operand.type) {
			error(checker, v, "can not cast expression from type %v to %v", value.type, operand.type)
		}
		operand.mode = .RValue
	case ^ast.Expr_Unary:
		// TODO: check applicability of operator
		expr := check_expr(checker, v.expr)
		operand.mode = .RValue
		operand.type = expr.type

	case ^ast.Type_Matrix:
		type := types.type_new(.Matrix, types.Matrix)

		rows := check_expr(checker, v.rows)
		if rows.mode != .Const || (rows.type.kind != .Int && rows.type.kind != .Uint) {
			error(checker, rows, "expected a constant integer")
		}
		cols: int
		if v.cols == nil {
			cols = int(rows.value.(i64) or_else 0)
		} else {
			cols_expr := check_expr(checker, v.cols)
			if cols_expr.mode != .Const || (cols_expr.type.kind != .Int && cols_expr.type.kind != .Uint) {
				error(checker, cols_expr, "expected a constant integer")
			} else {
				cols = int(cols_expr.value.(i64))
			}
		}

		col_type      := types.type_new(.Vector, types.Vector)
		col_type.count = int(rows.value.(i64) or_else 0)
		col_type.elem  = types.default_type(check_type(checker, v.elem))
		col_type.size  = col_type.elem.size * col_type.count
		col_type.align = col_type.elem.align

		type.col_type = col_type
		type.col_type = col_type

		type.cols  = cols
		type.size  = col_type.size * type.cols
		type.align = col_type.align

		operand.type = type
		operand.mode = .Type
	case ^ast.Type_Array:
		type := types.type_new(.Vector, types.Vector)

		count := check_expr(checker, v.count)
		if count.mode != .Const || (count.type.kind != .Int && count.type.kind != .Uint) {
			error(checker, count, "expected a constant integer as the count of an array")
		}

		type.count = int(count.value.(i64) or_else 0)
		type.elem  = types.default_type(check_type(checker, v.elem))
		type.size  = type.elem.size * type.count
		type.align = type.elem.align

		operand.type = type
		operand.mode = .Type
	case ^ast.Type_Struct:
		operand.mode = .Type

		type        := types.type_new(.Struct, types.Struct)
		fields      := make([dynamic]types.Field, 0, len(v.fields))
		fields_seen := make(map[string]struct{})
		offset      := 0
		align       := 1
		for field in v.fields {
			if field.ident.text in fields_seen {
				error(checker, field.ident, "duplicate field name: '%s'", field.ident.text)
			}
			fields_seen[field.ident.text] = {}

			type := check_type(checker, field.type)

			if type.align != 0 {
				offset = mem.align_forward_int(offset, type.align)
			}

			append(&fields, types.Field {
				name   = field.ident,
				type   = type,
				offset = offset,
			})

			offset += type.size
			align   = max(align, type.align)
		}

		offset = mem.align_forward_int(offset, align)

		type.fields = fields[:]
		type.size   = offset
		type.align  = align

		operand.type = type
		operand.mode = .Type
	}

	return
}

check_expr_or_type :: proc(checker: ^Checker, expr: ^ast.Expr, attributes: []ast.Field = {}) -> (operand: Operand) {
	operand = check_expr_internal(checker, expr, attributes)
	switch operand.mode {
	case .RValue, .LValue, .Const, .Type:
		return
	case .No_Value:
		error(checker, operand, "expected an expression, got no value")
	case .Builtin:
		error(checker, operand, "expected an expression, got builtin")
	case .Invalid:
	}

	
	operand.mode = .Invalid
	operand.type = types.t_invalid
	return
}

check_expr :: proc(checker: ^Checker, expr: ^ast.Expr, attributes: []ast.Field = {}) -> (operand: Operand) {
	operand = check_expr_internal(checker, expr, attributes)
	switch operand.mode {
	case .RValue, .LValue, .Const:
		return
	case .No_Value:
		error(checker, operand, "expected an expression, got no value")
	case .Builtin:
		error(checker, operand, "expected an expression, got builtin")
	case .Type:
		error(checker, operand, "expected an expression, got type")
	case .Invalid:
	}

	operand.mode = .Invalid
	operand.type = types.t_invalid
	return
}

check_type :: proc(checker: ^Checker, expr: ^ast.Expr, attributes: []ast.Field = {}) -> ^types.Type {
	operand := check_expr_internal(checker, expr, attributes)
	switch operand.mode {
	case .RValue, .LValue, .Const:
		error(checker, operand, "expected a type, got expression")
	case .No_Value:
		error(checker, operand, "expected a type, got no value")
	case .Builtin:
		error(checker, operand, "expected a type, got builtin")
	case .Type:
		return operand.type
	case .Invalid:
	}

	return types.t_invalid
}

error_checker_operand :: proc(checker: ^Checker, operand: Operand, message: string, args: ..any) {
	append(&checker.errors, tokenizer.Error {
		location = operand.expr.start,
		end     = operand.expr.end,
		message = fmt.aprintf(message, ..args, allocator = checker.error_allocator),
	})
}

error_checker_location :: proc(checker: ^Checker, location: tokenizer.Location, message: string, args: ..any) {
	append(&checker.errors, tokenizer.Error {
		location = location,
		end      = location,
		message  = fmt.aprintf(message, ..args, allocator = checker.error_allocator),
	})
}

error_checker_start_end :: proc(checker: ^Checker, start, end: tokenizer.Location, message: string, args: ..any) {
	append(&checker.errors, tokenizer.Error {
		location = start,
		end      = end,
		message  = fmt.aprintf(message, ..args, allocator = checker.error_allocator),
	})
}

error_checker_token :: proc(checker: ^Checker, token: tokenizer.Token, message: string, args: ..any) {
	end := token.location
	end.offset += len(token.text)
	end.column += len(token.text)
	append(&checker.errors, tokenizer.Error {
		location = token.location,
		end      = end,
		message  = fmt.aprintf(message, ..args, allocator = checker.error_allocator),
	})
}

error_checker_ast_node :: proc(checker: ^Checker, ast_node: ^ast.Node, message: string, args: ..any) {
	append(&checker.errors, tokenizer.Error {
		location = ast_node.start,
		end      = ast_node.end,
		message  = fmt.aprintf(message, ..args, allocator = checker.error_allocator),
	})
}

error :: proc {
	error_checker_operand,
	error_checker_location,
	error_checker_token,
	error_checker_ast_node,
	error_checker_start_end,
	error_parser_start_end,
	error_parser_single_token,
}
