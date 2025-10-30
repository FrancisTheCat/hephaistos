#+feature dynamic-literals
package hephaistos_checker

import "base:runtime"

import spv "../spirv-odin"
import "../types"
import "../ast"

Interface_Usage :: enum {
	In = 1,
	Out,
}

Interface_Info :: struct {
	type: ^types.Type,
	usage: [ast.Shader_Stage]Interface_Usage,
}

interface_infos: map[spv.BuiltIn]Interface_Info

@(init)
_builtin_infos_init :: proc "contextless" () {
	context = runtime.default_context()

	vec4  := types.vector_new(types.t_f32, 4, context.allocator)
	ivec3 := types.vector_new(types.t_i32, 3, context.allocator)
	interface_infos[.Position          ] = { type = vec4,        usage = #partial { .Vertex   = .Out, }, }
	interface_infos[.PointSize         ] = { type = types.t_f32, usage = #partial { .Vertex   = .Out, }, }
	interface_infos[.VertexIndex       ] = { type = types.t_i32, usage = #partial { .Vertex   = .In,  }, }
	interface_infos[.InstanceId        ] = { type = types.t_i32, usage = #partial { .Geometry = .In,  }, }
	interface_infos[.PrimitiveId       ] = { type = types.t_i32, usage = #partial { .Geometry = .In,  }, }
	interface_infos[.InvocationId      ] = { type = types.t_i32, usage = #partial { .Geometry = .In,  }, }
	interface_infos[.Layer             ] = { type = types.t_i32, usage = #partial { .Geometry = .Out, }, }
	interface_infos[.ViewportIndex     ] = { type = types.t_i32, usage = #partial { .Geometry = .Out, }, }
	interface_infos[.FragCoord         ] = { type = vec4,        usage = #partial { .Fragment = .In,  }, }
	interface_infos[.FragDepth         ] = { type = types.t_f32, usage = #partial { .Fragment = .In,  }, }
	interface_infos[.GlobalInvocationId] = { type = ivec3,       usage = #partial { .Compute  = .In,  }, }
}
