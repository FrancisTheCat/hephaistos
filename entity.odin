package hephaistos

import "ast"
import "tokenizer"
import "types"

Entity_Kind :: enum u32 {
	Invalid = 0,

	Nil,

	Const,
	Type,
	Var,
	Proc,
	Param,
}

@(rodata)
entity_kind_string := [Entity_Kind]string{
	.Invalid = "invalid",
	.Nil     = "nil",
	.Const   = "const",
	.Type    = "type",
	.Var     = "var",
	.Proc    = "proc",
	.Param   = "param",
}

Entity_Flags :: distinct bit_set[Entity_Flag; u32]
Entity_Flag :: enum {
	Parameter,
}

Entity :: struct {
	kind:  Entity_Kind,
	flags: Entity_Flags,
	ident: tokenizer.Token,
	name:  string,
	type: ^types.Type,
	decl: ^ast.Decl,
	value: types.Const_Value,
}

@(require_results)
entity_new :: proc(
	kind:  Entity_Kind,
	ident: tokenizer.Token,
	type:  ^types.Type,
	value: types.Const_Value = nil,
	decl:  ^ast.Decl         = nil,
) -> ^Entity {
	e := new(Entity)
	e.kind  = kind
	e.type  = type
	e.ident = ident
	e.name  = ident.text
	e.value = value
	e.decl  = decl
	return e
}
