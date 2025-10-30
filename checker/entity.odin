package hephaistos_checker

import "core:mem"

import "../ast"
import "../tokenizer"
import "../types"

Entity_Kind :: enum u32 {
	Invalid = 0,

	Const,
	Type,
	Var,
	Proc,
	Param,
	Builtin,
}

@(rodata)
entity_kind_string := [Entity_Kind]string{
	.Invalid = "invalid",
	.Const   = "const",
	.Type    = "type",
	.Var     = "var",
	.Proc    = "proc",
	.Param   = "param",
	.Builtin = "builtin",
}

Entity :: struct {
	kind:       Entity_Kind,
	ident:      tokenizer.Token,
	name:       string,
	type:       ^types.Type,
	decl:       ^ast.Decl,
	value:      types.Const_Value,
	builtin_id: ast.Builtin_Id,
}

@(require_results)
entity_new :: proc(
	kind:       Entity_Kind,
	ident:      tokenizer.Token,
	type:       ^types.Type,
	value:      types.Const_Value = nil,
	decl:       ^ast.Decl         = nil,
	builtin_id: ast.Builtin_Id    = nil,
	allocator:  mem.Allocator,
) -> ^Entity {
	e := new(Entity, allocator)
	e.kind       = kind
	e.type       = type
	e.ident      = ident
	e.name       = ident.text
	e.value      = value
	e.decl       = decl
	e.builtin_id = builtin_id
	return e
}
