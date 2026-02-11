package gl_cube

import glm "core:math/linalg/glsl"
import "core:os"
import "core:strings"
import "core:time"

import gl "vendor:OpenGL"
import "vendor:glfw"

import hep "../.."

WIDTH  :: 900
HEIGHT :: 600

@(require_results)
compile_graphics_program :: proc($path: string, shared_types: []typeid = {}) -> (program: u32, ok: bool) {
	source := #load(path, string)
	spirv, errors := hep.compile_shader(
		string(source),
		path,
		shared_types    = shared_types,
		spirv_version   = hep.SPIR_V_VERSION_1_0,
		allocator       = context.temp_allocator,
		error_allocator = context.temp_allocator,
	)
	if len(errors) != 0 {
		lines := strings.split_lines(string(source))
		for e in errors {
			hep.print_error(os.to_stream(os.stderr), path, lines, e)
		}
		return
	}

	program = gl.CreateProgram()
	shaders: [2]u32 = {
		0 = gl.CreateShader(gl.VERTEX_SHADER),
		1 = gl.CreateShader(gl.FRAGMENT_SHADER),
	}
	gl.ShaderBinary(len(shaders), raw_data(&shaders), gl.SHADER_BINARY_FORMAT_SPIR_V, raw_data(spirv), i32(len(spirv) * size_of(u32)))
	gl.SpecializeShader(shaders[0], "vertex_main",   0, nil, nil)
	gl.SpecializeShader(shaders[1], "fragment_main", 0, nil, nil)

	gl.AttachShader(program, shaders[0])
	gl.AttachShader(program, shaders[1])

	gl.LinkProgram(program)

	ok = true
	return
}

Shader_Uniforms :: struct {
	view, proj, model: glm.mat4,
	normal_matrix:     glm.mat3,

	albedo:          glm.vec3,
	light_direction: glm.vec3,
}

shader_uniforms: Shader_Uniforms = {
	view            = glm.mat4LookAt({0, 0, -4}, {}, {0, 1, 0}),
	proj            = glm.mat4Perspective(glm.PI / 2, WIDTH / HEIGHT, 0.001, 100),
	model           = 1,
	normal_matrix   = 1,
	albedo          = { 1, 0, 0, },
	light_direction = { 0, 1, 0, },
}

main :: proc() {
	glfw.Init()
	defer glfw.Terminate()

	glfw.WindowHint(glfw.SAMPLES, 8)
	window := glfw.CreateWindow(WIDTH, HEIGHT, "Hello Cube", nil, nil)
	defer glfw.DestroyWindow(window)

	glfw.MakeContextCurrent(window)
	gl.load_up_to(4, 6, glfw.gl_set_proc_address)

	glfw.SetFramebufferSizeCallback(window, proc "c" (window: glfw.WindowHandle, width, height: i32) {
		shader_uniforms.proj = glm.mat4Perspective(glm.PI / 2, f32(width) / f32(height), 0.001, 100)
		gl.Viewport(0, 0, width, height)
	})

	shader := compile_graphics_program("shader.hep", { Shader_Uniforms, }) or_else panic("Failed to compile shader")
	gl.UseProgram(shader)

	Vertex :: struct {
		position: glm.vec3,
		normal:   glm.vec3,
	}

	vertices: []Vertex = {
		// Bottom face
		{ position = { -1, -1, -1 }, normal = { 0, -1,  0 } },
		{ position = {  1, -1, -1 }, normal = { 0, -1,  0 } },
		{ position = {  1, -1,  1 }, normal = { 0, -1,  0 } },
		{ position = { -1, -1,  1 }, normal = { 0, -1,  0 } },

		// Top face
		{ position = { -1,  1, -1 }, normal = { 0,  1,  0 } },
		{ position = {  1,  1, -1 }, normal = { 0,  1,  0 } },
		{ position = {  1,  1,  1 }, normal = { 0,  1,  0 } },
		{ position = { -1,  1,  1 }, normal = { 0,  1,  0 } },

		// Left face
		{ position = { -1, -1, -1 }, normal = { -1, 0,  0 } },
		{ position = { -1,  1, -1 }, normal = { -1, 0,  0 } },
		{ position = { -1,  1,  1 }, normal = { -1, 0,  0 } },
		{ position = { -1, -1,  1 }, normal = { -1, 0,  0 } },

		// Right face
		{ position = {  1, -1, -1 }, normal = { 1,  0,  0 } },
		{ position = {  1,  1, -1 }, normal = { 1,  0,  0 } },
		{ position = {  1,  1,  1 }, normal = { 1,  0,  0 } },
		{ position = {  1, -1,  1 }, normal = { 1,  0,  0 } },

		// Front face
		{ position = { -1, -1, -1 }, normal = { 0,  0, -1 } },
		{ position = {  1, -1, -1 }, normal = { 0,  0, -1 } },
		{ position = {  1,  1, -1 }, normal = { 0,  0, -1 } },
		{ position = { -1,  1, -1 }, normal = { 0,  0, -1 } },

		// Back face
		{ position = { -1, -1,  1 }, normal = { 0,  0,  1 } },
		{ position = {  1, -1,  1 }, normal = { 0,  0,  1 } },
		{ position = {  1,  1,  1 }, normal = { 0,  0,  1 } },
		{ position = { -1,  1,  1 }, normal = { 0,  0,  1 } },
	}

	indices: []u32 = {
	    // Bottom face
	    0, 1, 2,
	    0, 2, 3,

	    // Top face
	    4, 5, 6,
	    4, 6, 7,

	    // Left face
	    8, 9, 10,
	    8, 10, 11,

	    // Right face
	    12, 13, 14,
	    12, 14, 15,

	    // Front face
	    16, 17, 18,
	    16, 18, 19,

	    // Back face
	    20, 21, 22,
	    20, 22, 23,
	}

	vbo, ibo: u32
	gl.CreateBuffers(1, &vbo)
	gl.CreateBuffers(1, &ibo)
	gl.NamedBufferStorage(vbo, size_of(Vertex) * len(vertices), &vertices[0], 0)
	gl.NamedBufferStorage(ibo, size_of(Vertex) * len(indices),  &indices[0],  0)

	vao: u32
	gl.CreateVertexArrays(1, &vao)

	gl.VertexArrayVertexBuffer(vao, 0, vbo, 0, size_of(Vertex))
	gl.VertexArrayElementBuffer(vao, ibo)

	gl.EnableVertexArrayAttrib(vao, 0)
	gl.EnableVertexArrayAttrib(vao, 1)

	gl.VertexArrayAttribFormat(vao, 0, 3, gl.FLOAT, false, u32(offset_of(Vertex, position)))
	gl.VertexArrayAttribFormat(vao, 1, 3, gl.FLOAT, false, u32(offset_of(Vertex, normal)))

	gl.VertexArrayAttribBinding(vao, 0, 0)
	gl.VertexArrayAttribBinding(vao, 1, 0)

	ubo: u32
	gl.CreateBuffers(1, &ubo)
	gl.NamedBufferStorage(ubo, size_of(Shader_Uniforms), nil, gl.DYNAMIC_STORAGE_BIT)
	gl.BindBufferRange(gl.UNIFORM_BUFFER, 0, ubo, 0, size_of(Shader_Uniforms))

	gl.Enable(gl.DEPTH_TEST)
	gl.ClearColor(0.15, 0.15, 0.15, 1)

	start_time := time.now()
	for !glfw.WindowShouldClose(window) {
		gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		current_time := f32(f64(time.since(start_time)) / f64(time.Second))

		shader_uniforms.model         = glm.mat4Rotate({1, 2, 1}, current_time)
		shader_uniforms.normal_matrix = glm.mat3(shader_uniforms.model)

		shader_uniforms.albedo = {
			0.5 * glm.sin(current_time + 0 * glm.PI / 3) + 0.5,
			0.5 * glm.sin(current_time + 1 * glm.PI / 3) + 0.5,
			0.5 * glm.sin(current_time + 2 * glm.PI / 3) + 0.5,
		}

		gl.NamedBufferSubData(ubo, 0, size_of(Shader_Uniforms), &shader_uniforms)
		gl.BindVertexArray(vao)
		gl.DrawElements(gl.TRIANGLES, i32(len(indices)), gl.UNSIGNED_INT, nil)

		glfw.SwapBuffers(window)
		glfw.PollEvents()
		free_all(context.temp_allocator)
	}
}
