package example

import "core:slice"
import "core:strings"
import "core:mem"
import "core:fmt"
import "core:os"
import "core:prof/spall"
import glm "core:math/linalg/glsl"

import hep ".."

ENABLE_SPALL :: #config(ENABLE_SPALL, false)

when ENABLE_SPALL {
	spall_ctx:    spall.Context
	spall_buffer: spall.Buffer

	@(instrumentation_enter)
	spall_enter :: proc "contextless" (proc_address, call_site_return_address: rawptr, loc: runtime.Source_Code_Location) {
		spall._buffer_begin(&spall_ctx, &spall_buffer, "", "", loc)
	}

	@(instrumentation_exit)
	spall_exit :: proc "contextless" (proc_address, call_site_return_address: rawptr, loc: runtime.Source_Code_Location) {
		spall._buffer_end(&spall_ctx, &spall_buffer)
	}
}

main :: proc() {
	when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		defer mem.tracking_allocator_destroy(&track)
		context.allocator = mem.tracking_allocator(&track)

		defer for _, leak in track.allocation_map {
			fmt.printf("%v leaked %m\n", leak.location, leak.size)
		}
	}

	when ENABLE_SPALL {
		spall_ctx = spall.context_create("trace.spall")
		defer spall.context_destroy(&spall_ctx)

		buffer_backing := make([]u8, spall.BUFFER_DEFAULT_SIZE)
		defer delete(buffer_backing)

		spall_buffer = spall.buffer_create(buffer_backing)
		defer spall.buffer_destroy(&spall_ctx, &spall_buffer)
	}

	Vertex_Shader_Uniforms :: struct {
		view, proj, model: glm.mat4,
	}

	Shadow_Uniforms :: struct {
		shadow_matrix:   glm.mat4,
		light_direction: glm.vec3,
	}

	Some_Enum :: enum u64 {
		A, B, C,
	}

	defines: map[string]hep.Const_Value
	defines["SOME_CONFIG_VAR"] = true
	defer delete(defines)

	FILE_NAME :: "example.hep"
	source := #load(FILE_NAME, string)
	code, errors := hep.compile_shader(
		source,
		FILE_NAME,
		defines         = defines,
		shared_types    = { Vertex_Shader_Uniforms, Shadow_Uniforms, Some_Enum, },
		error_allocator = context.temp_allocator,
	)
	defer delete(code)

	if len(errors) != 0 {
		lines := strings.split_lines(source, context.temp_allocator)
		for error in errors {
			hep.print_error(os.stream_from_handle(os.stderr), FILE_NAME, lines, error)
		}
		return
	}

	os.write_entire_file("a.spv", slice.to_bytes(code))
}
