package types

import "core:io"
import "core:fmt"
import "core:strings"
import "core:hash"
import "core:mem"

import "../tokenizer"

Field :: struct {
	name:   tokenizer.Token,
	type:  ^Type,
	value:  Const_Value,
	offset: int,
}

Const_Value :: union {
	i64,
	f64,
	bool,
	string,
}

Struct :: struct {
	using base: Type,
	fields:   []Field,
}

Vector :: struct {
	using base: Type,
	count:      int,
	elem:      ^Type,
}

Matrix :: struct {
	using base: Type,
	cols:       int,
	col_type:  ^Vector,
}

Proc :: struct {
	using base:   Type,
	args:       []Field,
	returns:    []Field,
	return_type: ^Type,
}

Kind :: enum {
	Invalid,

	Uint,
	Int,
	Bool,
	Float,

	Struct,
	Matrix,
	Vector,
	Proc,

	Tuple,
}

Type :: struct {
	kind:    Kind,
	size:    int,
	align:   int,
	variant: union {
		^Struct,
		^Matrix,
		^Vector,
		^Proc,
	},
}

new :: proc(kind: Kind, $T: typeid, allocator: mem.Allocator) -> ^T {
	t, _ := mem.new(T, allocator)
	t.kind    = kind
	t.variant = t
	return t
}

t_invalid := &Type{kind = .Invalid, size = 0, align = 1}
t_bool    := &Type{kind = .Bool,    size = 1, align = 1}
t_int     := &Type{kind = .Int,     size = 0, align = 0}
t_uint    := &Type{kind = .Uint,    size = 0, align = 0}
t_float   := &Type{kind = .Float,   size = 0, align = 0}

t_i8      := &Type{kind = .Int,     size = 1, align = 1}
t_i16     := &Type{kind = .Int,     size = 2, align = 2}
t_i32     := &Type{kind = .Int,     size = 4, align = 4}
t_i64     := &Type{kind = .Int,     size = 8, align = 8}

t_u8      := &Type{kind = .Uint,    size = 1, align = 1}
t_u16     := &Type{kind = .Uint,    size = 2, align = 2}
t_u32     := &Type{kind = .Uint,    size = 4, align = 4}
t_u64     := &Type{kind = .Uint,    size = 8, align = 8}

t_f32     := &Type{kind = .Float,   size = 4, align = 4}
t_f64     := &Type{kind = .Float,   size = 8, align = 8}

print_writer :: proc(w: io.Writer, type: ^Type) {
	if type == nil {
		fmt.wprint(w, "<nil>")
		return
	}

	switch type.kind {
	case .Invalid:
		fmt.wprint(w, "invalid type")
	case .Struct:
		s := type.variant.(^Struct)
		fmt.wprint(w, "struct {")
		for field, i in s.fields {
			if i > 0 {
				fmt.wprint(w, ", ")
			}
			fmt.wprint(w, field.name.text)
			fmt.wprint(w, ": ")
			print_writer(w, field.type)
		}
		fmt.wprint(w, "}")
	case .Matrix:
		m := type.variant.(^Matrix)
		c := m.col_type
		fmt.wprintf(w, "matrix[%d, %d]", m.cols, c.count)
		print_writer(w, c.elem)
	case .Vector:
		v := type.variant.(^Vector)
		fmt.wprintf(w, "vector[%d]", v.count)
		print_writer(w, v.elem)
	case .Proc:
		b := type.variant.(^Proc)
		fmt.wprint(w, "proc(")
		for arg, i in b.args {
			if i > 0 {
				fmt.wprint(w, ", ")
			}
			fmt.wprint(w, arg.name.text)
			fmt.wprint(w, ": ")
			print_writer(w, arg.type)
		}
		fmt.wprint(w, ") -> (")
		for ret, i in b.returns {
			if i > 0 {
				fmt.wprint(w, ", ")
			}
			if len(ret.name.text) != 0 {
				fmt.wprint(w, ret.name.text)
				fmt.wprint(w, ": ")
			}
			print_writer(w, ret.type)
		}
		fmt.wprint(w, ")")
	case .Int:
		if type.size == 0 {
			fmt.wprintf(w, "int")
		} else {
			fmt.wprintf(w, "i%d", type.size * 8)
		}
	case .Uint:
		fmt.wprintf(w, "u%d", type.size * 8)
	case .Bool:
		fmt.wprintf(w, "bool")
	case .Float:
		if type.size == 0 {
			fmt.wprintf(w, "float")
		} else {
			fmt.wprintf(w, "f%d", type.size * 8)
		}
	case .Tuple:
		fmt.wprint(w, "(")
		for type, i in type.variant.(^Struct).fields {
			if i > 0 {
				fmt.wprint(w, ", ")
			}
			print_writer(w, type.type)
		}
		fmt.wprint(w, ")")
	}
}

@(require_results)
print_string :: proc(type: ^Type, allocator := context.allocator) -> string {
	b := strings.builder_make(allocator)
	print_writer(strings.to_writer(&b), type)
	return strings.to_string(b)
}

@(require_results)
equal :: proc(a, b: ^Type) -> bool {
	if a == b {
		return true
	}

	if a == nil || b == nil {
		return false
	}

	if a.kind != b.kind {
		return false
	}

	#partial switch a.kind {
	case .Int, .Bool, .Float:
		return a.size == b.size && a.align == b.align

	case .Struct:
		unimplemented()
	case .Matrix:
		a := a.variant.(^Matrix)
		b := b.variant.(^Matrix)

		if a == b {
			return true
		}

		if a.cols != b.cols {
			return false
		}

		return equal(a.col_type, b.col_type)
		
	case .Vector:
		a := a.variant.(^Vector)
		b := b.variant.(^Vector)

		if a == b {
			return true
		}

		if a.count != b.count {
			return false
		}

		return equal(a.elem, b.elem)
	case .Proc:
		unimplemented()
	}

	return true
}

implicity_castable :: proc(from, to: ^Type) -> bool {
	return false
}

@(require_results)
op_result_type :: proc(a, b: ^Type) -> (result: ^Type) {
	a := a
	b := b

	if a == b {
		return a
	}

	if a == nil || b == nil {
		return t_invalid
	}

	if a.kind == .Tuple {
		t := a.variant.(^Struct)
		if len(t.fields) == 1 {
			a = t.fields[0].type
		}
	}

	if b.kind == .Tuple {
		t := b.variant.(^Struct)
		if len(t.fields) == 1 {
			b = t.fields[0].type
		}
	}

	if a.kind == .Invalid || b.kind == .Invalid {
		return t_invalid
	}

	if a.size == 0 && b.size == 0 {
		if a.kind == .Float || b.kind == .Float {
			return t_float
		} else {
			return t_int
		}
	}
	
	if a.size == 0 {
		if b.kind == .Int && a.kind == .Float {
			return t_invalid
		}
		return b
	}

	if b.size == 0 {
		if a.kind == .Int && b.kind == .Float {
			return t_invalid
		}
		return a
	}

	if equal(a, b) {
		return a
	}

	if a.kind == .Matrix && b.kind == .Matrix {
		return t_invalid
	}
	if a.kind == .Vector && b.kind == .Vector {
		return t_invalid
	}

	if a.kind == .Matrix || b.kind == .Matrix {
		m: ^Type
		s: ^Type
		if a.kind == .Matrix {
			m = a
			s = b
		} else {
			m = b
			s = a
		}

		if op_result_type(m.variant.(^Matrix).col_type.elem, s).kind != .Invalid {
			return m
		} else {
			return t_invalid
		}
	}

	if a.kind == .Vector || b.kind == .Vector {
		v: ^Type
		s: ^Type
		if a.kind == .Vector {
			v = a
			s = b
		} else {
			v = b
			s = a
		}

		if op_result_type(v.variant.(^Vector).elem, s).kind != .Invalid {
			return v
		} else {
			return t_invalid
		}
	}

	return t_invalid
}

@(require_results)
default_type :: proc(type: ^Type) -> ^Type {
	if type == nil || type.size != 0 {
		return type
	}

	#partial switch type.kind {
	case .Uint:
		return t_u32
	case .Int:
		return t_i32
	case .Bool:
		return t_bool
	case .Float:
		return t_f32

	case .Proc, .Invalid, .Struct, .Matrix, .Vector:
		return type
	}

	unreachable()
}

@(require_results)
castable :: proc(from, to: ^Type) -> bool {
	if equal(from, to) {
		return true
	}

	if is_numeric(from) && is_numeric(to) {
		return true
	}

	if from.kind == .Bool && is_integer(to) {
		return true
	}

	return false
}

@(require_results)
is_numeric :: proc(type: ^Type) -> bool {
	#partial switch type.kind {
	case .Float, .Int, .Uint:
		return true
	}
	return false
}

@(require_results)
is_integer :: proc(type: ^Type) -> bool {
	#partial switch type.kind {
	case .Int, .Uint:
		return true
	}
	return false
}

@(require_results)
is_float :: proc(type: ^Type) -> bool {
	#partial switch type.kind {
	case .Float:
		return true
	}
	return false
}

@(private="file")
to_bytes :: proc(v: $P/^$T) -> []byte {
	return ([^]byte)(v)[:size_of(T)]
}

@(require_results)
type_hash :: proc(type: ^Type) -> u64 {
	h := hash.fnv64a(to_bytes(type)[:offset_of(Type, variant)])

	switch v in type.variant {
	case ^Struct:
		for field in v.fields {
			field_hash := type_hash(field.type)
			h           = hash.fnv64a(to_bytes(&field_hash), h)
		}
	case ^Matrix:
		col_hash := type_hash(v.col_type)
		h         = hash.fnv64a(to_bytes(&col_hash), h)
		h         = hash.fnv64a(to_bytes(&v.cols), h)
	case ^Vector:
		elem_hash := type_hash(v.elem)
		h          = hash.fnv64a(to_bytes(&elem_hash), h)
		h          = hash.fnv64a(to_bytes(&v.count), h)
	case ^Proc:
		for field in v.args {
			field_hash := type_hash(field.type)
			h           = hash.fnv64a(to_bytes(&field_hash), h)
		}
		for field in v.returns {
			field_hash := type_hash(field.type)
			h           = hash.fnv64a(to_bytes(&field_hash), h)
		}
	}
	
	return h
}
