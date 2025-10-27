#+feature dynamic-literals
package hephaistos

import "base:runtime"

import spv "spirv-odin"
import "types"
import "ast"

Builtin_Usage :: enum {
	In = 1,
	Out,
}

Builtin_Info :: struct {
	type: ^types.Type,
	usage: [ast.Shader_Stage]Builtin_Usage,
}

builtin_infos: map[spv.BuiltIn]Builtin_Info

@(init)
_builtin_infos_init :: proc "contextless" () {
	context = runtime.default_context()

	vec4 := types.vector_new(types.t_f32, 4, context.allocator)
	builtin_infos[.Position     ] = { type = vec4,        usage = #partial { .Vertex   = .Out, }, }
	builtin_infos[.PointSize    ] = { type = types.t_f32, usage = #partial { .Vertex   = .Out, }, }
	builtin_infos[.VertexIndex  ] = { type = types.t_i32, usage = #partial { .Vertex   = .In,  }, }
	builtin_infos[.InstanceId   ] = { type = types.t_i32, usage = #partial { .Geometry = .In,  }, }
	builtin_infos[.PrimitiveId  ] = { type = types.t_i32, usage = #partial { .Geometry = .In,  }, }
	builtin_infos[.InvocationId ] = { type = types.t_i32, usage = #partial { .Geometry = .In,  }, }
	builtin_infos[.Layer        ] = { type = types.t_i32, usage = #partial { .Geometry = .Out, }, }
	builtin_infos[.ViewportIndex] = { type = types.t_i32, usage = #partial { .Geometry = .Out, }, }
	builtin_infos[.FragCoord    ] = { type = vec4,        usage = #partial { .Fragment = .In,  }, }
	builtin_infos[.FragDepth    ] = { type = types.t_f32, usage = #partial { .Fragment = .In,  }, }
}
