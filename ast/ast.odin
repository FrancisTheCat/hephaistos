package ast

import "base:intrinsics"

@(require)
import "core:mem"

import "../tokenizer"
import "../types"

Node :: struct {
	start, end: tokenizer.Location,
	derived:    Any_Node,
}

Expr :: struct {
	using expr_base: Node,
	derived_expr:    Any_Expr,
	type:           ^types.Type,
	const_value:     types.Const_Value,
}

Stmt :: struct {
	using stmt_base: Node,
	derived_stmt:    Any_Stmt,
}

Decl :: struct {
	using decl_base: Stmt,
	derived_decl:    Any_Decl,
}


Expr_Binary :: struct {
	using node: Expr,
	op:         tokenizer.Token_Kind,
	lhs, rhs:  ^Expr,
}

Expr_Unary :: struct {
	using node: Expr,
	op:         tokenizer.Token_Kind,
	expr:      ^Expr,
}

Const_Value :: union {
		bool,
		i64,
		f64,
	}

Expr_Constant :: struct {
	using node: Expr,
	value:      types.Const_Value,
}

Expr_Ident :: struct {
	using node: Expr,
	ident:      tokenizer.Token,
}

Expr_Proc_Lit :: struct {
	using node: Expr,
	args:       []Field,
	returns:    []Field,
	body:       []^Stmt,
}

Expr_Proc_Sig :: struct {
	using node: Expr,
	args:       []Field,
	returns:    []Field,
}

Expr_Call :: struct {
	using node: Expr,
	lhs:       ^Expr,
	args:     []Field,
	is_cast:    bool,
}

Expr_Paren :: struct {
	using node: Expr,
	expr:      ^Expr,
}

Expr_Selector :: struct {
	using node: Expr,
	lhs:       ^Expr,
	selector:   tokenizer.Token,
}

Expr_Compound :: struct {
	using node: Expr,
	fields:   []Field,
}

Expr_Index :: struct {
	using node: Expr,
	lhs, rhs:  ^Expr,
}

Expr_Cast :: struct {
	using node: Expr,
	value:     ^Expr,
	type_expr: ^Expr,
}


Type_Struct :: struct {
	using node: Expr,
	fields:     []Field,
}

Type_Array :: struct {
	using node: Expr,
	count:     ^Expr,
	elem:      ^Expr,
}

Type_Matrix :: struct {
	using node: Expr,
	rows:      ^Expr,
	cols:      ^Expr,
	elem:      ^Expr,
}


Decl_Value :: struct {
	using node: Decl,
	lhs:        []^Expr,
	type_expr:  ^Expr,
	values:     []^Expr,
	mutable:    bool,
	types:      []^types.Type,
}

Stmt_Return :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
	values:  []^Expr,
}

Stmt_Break :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
}

Stmt_Continue :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
}

Stmt_For_Range :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
	start, end: Expr,
	ident:      tokenizer.Token,
	body:       []^Stmt,
}

Stmt_For :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
	init:       ^Stmt,
	cond:       ^Expr,
	post:       ^Stmt,
	body:       []^Stmt,
}

Stmt_Block :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
	body:       []^Stmt,
}

Stmt_If :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
	init:       ^Stmt,
	cond:       ^Expr,
	then_block: []^Stmt,
	else_block: []^Stmt,
}

Switch_Case :: struct {
	token: tokenizer.Token,
	value: ^Expr,
	body:  []^Stmt,
}

Stmt_Switch :: struct {
	using node: Stmt,
	label:      tokenizer.Token,
	init:       ^Stmt,
	cond:       ^Expr,
	cases:      []Switch_Case,
}

Stmt_Assign :: struct {
	using node: Stmt,
	lhs, rhs:   []^Expr,
	op:         tokenizer.Token_Kind,
	types:      []^types.Type,
}

Stmt_Expr :: struct {
	using node: Stmt,
	expr:      ^Expr,
}


Any_Node :: union {
	^Expr_Constant,
	^Expr_Binary,
	^Expr_Ident,
	^Expr_Proc_Lit,
	^Expr_Proc_Sig,
	^Expr_Paren,
	^Expr_Selector,
	^Expr_Call,
	^Expr_Compound,
	^Expr_Index,
	^Expr_Cast,
	^Expr_Unary,

	^Type_Struct,
	^Type_Array,
	^Type_Matrix,

	^Stmt_Return,
	^Stmt_Break,
	^Stmt_Continue,
	^Stmt_For_Range,
	^Stmt_For,
	^Stmt_Block,
	^Stmt_If,
	^Stmt_Switch,
	^Stmt_Assign,
	^Stmt_Expr,

	^Decl_Value,
}

Any_Expr :: union {
	^Expr_Constant,
	^Expr_Binary,
	^Expr_Ident,
	^Expr_Proc_Lit,
	^Expr_Proc_Sig,
	^Expr_Paren,
	^Expr_Selector,
	^Expr_Call,
	^Expr_Compound,
	^Expr_Index,
	^Expr_Cast,
	^Expr_Unary,
	
	^Type_Struct,
	^Type_Array,
	^Type_Matrix,
}

Any_Decl :: union {
	^Decl_Value,
}

Any_Stmt :: union {
	^Stmt_Return,
	^Stmt_Break,
	^Stmt_Continue,
	^Stmt_For_Range,
	^Stmt_For,
	^Stmt_Block,
	^Stmt_If,
	^Stmt_Switch,
	^Stmt_Assign,
	^Stmt_Expr,

	^Decl_Value,
}

new :: proc($T: typeid, start, end: tokenizer.Location) -> ^T {
	n, _ := mem.new(T)
	n.start   = start
	n.end     = end
	n.derived = n
	base: ^Node = n // dummy check
	_ = base // "Use" type to make -vet happy
	when intrinsics.type_has_field(T, "derived_expr") {
		n.derived_expr = n
	}
	when intrinsics.type_has_field(T, "derived_stmt") {
		n.derived_stmt = n
	}
	when intrinsics.type_has_field(T, "derived_decl") {
		n.derived_decl = n
	}
	return n
}

Field :: struct {
	ident:  tokenizer.Token,
	type:  ^Expr,
	value: ^Expr,
}

A_Context, a_init, a_destroy :: struct {
	data: []byte,
}, proc(a: ^A_Context, n: int) {
	a.data = make([]byte, n)
}, proc(a: ^A_Context) {
	delete(a.data)
}

File :: struct {
	stmts: []Stmt,
}
