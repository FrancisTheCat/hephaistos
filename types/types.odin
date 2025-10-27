package types

import "core:io"
import "core:fmt"
import "core:strings"
import "core:hash"
import "core:mem"

import "../tokenizer"

Field :: struct {
	name:     tokenizer.Token,
	type:     ^Type,
	value:    Const_Value,
	offset:   int,
	location: int,
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

@(require_results)
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
		a := a.variant.(^Struct)
		b := b.variant.(^Struct)
		if len(a.fields) != len(b.fields) {
			return false
		}

		for i in 0 ..< len(a.fields) {
			if a.fields[i].offset != b.fields[i].offset {
				return false
			}
			if a.fields[i].location != b.fields[i].location {
				return false
			}
			if !equal(a.fields[i].type, b.fields[i].type) {
				return false
			}
		}

		return true

	case .Matrix:
		a := a.variant.(^Matrix)
		b := b.variant.(^Matrix)

		if a.cols != b.cols {
			return false
		}

		return equal(a.col_type, b.col_type)
	case .Vector:
		a := a.variant.(^Vector)
		b := b.variant.(^Vector)

		if a.count != b.count {
			return false
		}

		return equal(a.elem, b.elem)
	case .Proc:
		unimplemented()
	}

	return true
}

@(require_results)
base_type :: proc(type: ^Type) -> ^Type {
	type := type
	for type.kind == .Tuple {
		t := type.variant.(^Struct)
		if len(t.fields) == 1 {
			type = t.fields[0].type
		} else {
			return type
		}
	}
	return type
}

@(require_results)
implicitly_castable :: proc(from, to: ^Type) -> bool {
	to   := base_type(to)
	from := base_type(from)

	if equal(from, to) {
		return true
	}

	if from.size == 0 {
		if to.kind == .Int && from.kind == .Float {
			return false
		}
		return true
	}

	if is_numeric(from) && to.kind == .Vector {
		return implicitly_castable(from, to.variant.(^Vector).elem)
	}

	if is_numeric(from) && to.kind == .Matrix {
		return implicitly_castable(from, to.variant.(^Matrix).col_type.elem)
	}

	return false
}

@(require_results)
op_result_type :: proc(a, b: ^Type, is_multiply: bool, allocator: mem.Allocator) -> ^Type {
	if is_multiply && (is_matrix(a) || is_matrix(b)) {
		return matrix_multiply_type(a, b, allocator)
	}

	if implicitly_castable(a, b) {
		return b
	}

	if implicitly_castable(b, a) {
		return a
	}

	return t_invalid
}

@(require_results)
default_type :: proc(type: ^Type) -> ^Type {
	type := base_type(type)

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

	if implicitly_castable(from, to) {
		return true
	}

	if is_numeric(from) && is_numeric(to) {
		return true
	}

	if is_bool(from) && is_integer(to) {
		return true
	}

	if is_numeric(from) && is_vector(to) {
		return true
	}

	return false
}

@(require_results)
is_vector :: proc(type: ^Type) -> bool {
	return type.kind == .Vector
}

@(require_results)
is_matrix :: proc(type: ^Type) -> bool {
	return type.kind == .Matrix
}

@(require_results)
is_struct :: proc(type: ^Type) -> bool {
	return type.kind == .Struct
}

@(require_results)
is_bool :: proc(type: ^Type) -> bool {
	return type.kind == .Bool
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

@(require_results)
matrix_multiply_type :: proc(a, b: ^Type, allocator: mem.Allocator) -> ^Type {
	if a == nil || b == nil {
		return t_invalid
	}

	assert(a.kind == .Matrix || b.kind == .Matrix)

	if a.kind == .Matrix && b.kind == .Matrix {
		a := a.variant.(^Matrix)
		b := b.variant.(^Matrix)

		if a.cols != b.col_type.count {
			return t_invalid
		}

		if !equal(a.col_type.elem, b.col_type.elem) {
			return t_invalid
		}

		col := vector_new(a.col_type.elem, a.col_type.count, allocator)
		return matrix_new(col, b.cols, allocator)
	}

	if a.kind == .Matrix && b.kind == .Vector {
		a := a.variant.(^Matrix)
		v := b.variant.(^Vector)

		if a.cols != v.count {
			return t_invalid
		}

		if !equal(v.elem, matrix_elem_type(a)) {
			return t_invalid
		}

		return vector_new(v.elem, a.col_type.count, allocator)
	}

	if a.kind == .Vector && b.kind == .Matrix {
		v := a.variant.(^Vector)
		b := b.variant.(^Matrix)

		if v.count != b.col_type.count {
			return t_invalid
		}

		if !equal(v.elem, matrix_elem_type(b)) {
			return t_invalid
		}

		return vector_new(v.elem, b.cols, allocator)
	}

	if a.kind == .Float {
		if op_result_type(a, matrix_elem_type(b), false, {}) == t_invalid {
			return t_invalid
		}
		return b
	}

	if b.kind == .Float {
		if op_result_type(b, matrix_elem_type(a), false, {}) == t_invalid {
			return t_invalid
		}
		return a
	}

	return t_invalid
}

@(require_results)
matrix_elem_type :: proc(t: ^Type) -> ^Type {
	return t.variant.(^Matrix).col_type.elem
}

@(require_results)
vector_new :: proc(elem: ^Type, count: int, allocator: mem.Allocator) -> ^Vector {
	assert(elem      != nil)
	// assert(elem.size != 0)

	type := new(.Vector, Vector, allocator)
	type.elem  = elem
	type.count = count
	type.size  = count * elem.size
	type.align = elem.align

	return type
}

@(require_results)
matrix_new :: proc(col_type: ^Vector, cols: int, allocator: mem.Allocator) -> ^Matrix {
	assert(col_type      != nil)
	assert(col_type.size != 0)

	type         := new(.Matrix, Matrix, allocator)
	type.col_type = col_type
	type.cols     = cols
	type.size     = cols * col_type.size
	type.align    = col_type.align

	return type
}

@(require_results)
is_comparable :: proc(type: ^Type) -> bool{
	#partial switch type.kind {
	case .Proc, .Tuple:
		return false
	}
	return true
}

@(require_results)
operator_applicable :: proc(type: ^Type, op: tokenizer.Token_Kind) -> bool {
	if is_comparable(type) && (op == .Equal || op == .Not_Equal) {
		return true
	}

	if is_numeric(type) {
		#partial switch op {
		case .Less, .Less_Equal, .Greater, .Greater_Equal:
			return true
		case .Add, .Subtract, .Multiply, .Divide:
			return true
		}
	}

	#partial switch type.kind {
	case .Int, .Uint:
		#partial switch op {
		case .Modulo, .Modulo_Floored:
			return true
		case .Bit_Or, .Bit_And, .Xor, .Shift_Left, .Shift_Right:
			return true
		}
	case .Bool:
		#partial switch op {
		case .Not, .And, .Or:
			return true
		}
	case .Vector:
		return operator_applicable(type.variant.(^Vector).elem, op)
	case .Matrix:
		return operator_applicable(type.variant.(^Matrix).col_type.elem, op)
	}

	return false
}
