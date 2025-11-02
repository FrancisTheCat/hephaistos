package hephaistos_c_bindings

import "base:runtime"

import "core:io"
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
hep_result_free :: proc "c" (#by_ptr r: Result) {
	context = runtime.default_context()
	if len(r.errors) != 0 {
		mem.dynamic_arena_destroy(r._error_arena)
		free(r._error_arena)
	} else {
		delete(r.instructions)
	}
}

@(export)
hep_error_print :: proc "c" (#by_ptr error: hep.Error, file_name: cstring, lines: [^]string, buf: [^]u8) -> int {
	context = runtime.default_context()
	buf := buf
	w: io.Writer
	if buf == nil {
		w = {
			procedure = proc(
				stream_data: rawptr,
				mode:        io.Stream_Mode,
				p:           []byte,
				offset:      i64,
				whence:      io.Seek_From,
			) -> (n: i64, err: io.Error) {
				return i64(len(p)), nil
			},
			data = nil,
		}
	} else {
		w = {
			procedure = proc(
				stream_data: rawptr,
				mode:        io.Stream_Mode,
				p:           []byte,
				offset:      i64,
				whence:      io.Seek_From,
			) -> (n: i64, err: io.Error) {
				if mode != .Write {
					return
				}
				buf := (^[^]u8)(stream_data)
				copy(buf[:len(p)], p)
				buf^ = buf[len(p):]
				return i64(len(p)), nil
			},
			data = &buf,
		}
	}
	defer if buf != nil {
		buf[0] = 0
	}
	return hep.print_error(w, string(file_name), lines[:max(int)], error) + 1
}
