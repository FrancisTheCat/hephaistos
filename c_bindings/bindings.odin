package hephaistos_c_bindings

import "base:runtime"

import "core:mem"

import hep ".."

Result :: struct {
	errors:       []hep.Error,
	instructions: []u32,
	_error_arena: ^mem.Dynamic_Arena,
}

@(export)
hep_compile_shader :: proc "c" (
	source: cstring,
	path:   cstring,
) -> (r: Result) {
	context = runtime.default_context()
	defer free_all(context.temp_allocator)

	error_arena := new(mem.Dynamic_Arena)
	mem.dynamic_arena_init(error_arena)
	r.instructions, r.errors = hep.compile_shader(
		string(source),
		string(path),
		error_allocator = mem.dynamic_arena_allocator(error_arena),
	)
	if len(r.errors) == 0 {
		mem.dynamic_arena_destroy(error_arena)
		free(error_arena)
	} else {
		r._error_arena = error_arena
	}
	return
}

@(export)
hep_result_free :: proc "c" (r: Result) {
	context = runtime.default_context()
	if len(r.errors) != 0 {
		mem.dynamic_arena_destroy(r._error_arena)
		free(r._error_arena)
	} else {
		delete(r.instructions)
	}
}
