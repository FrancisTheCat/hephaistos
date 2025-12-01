# Hephaistos
Hephaistos is a shading language heavily inspired by the [Odin](https://odin-lang.org/) programming language.

```odin
// VERTEX SHADER

@(push_constant)
vs_constants: #import(Vertex_Shader_Constants)

@(vertex_shader)
vertex_main :: proc(
    a_position:   [3]f32 @ 0,
    a_normal:     [3]f32 @ 1,
    a_tex_coords: [2]f32 @ 2,
) -> (
    v_position:   [3]f32 @ 0,
    v_normal:     [3]f32 @ 1,
    v_tex_coords: [2]f32 @ 2,
) {
    v_position   = (vs_constants.model * [4]f32{a_position, 1}).xyz
    v_normal     = vs_constants.normal_matrix * a_normal
    v_tex_coords = a_tex_coords

    $Position = vs_constants.proj * vs_constants.view * [4]f32{v_position, 1}
    return
}

// FRAGMENT SHADER

@(uniform_buffer, binding = 0)
fs_uniforms: #import(Fragment_Shader_Uniforms)

@(uniform, binding = 1)
texture: sampler[2][3]f32

@(fragment_shader)
fragment_main :: proc(
    v_position:   [3]f32 @ 0,
    v_normal:     [3]f32 @ 1,
    v_tex_coords: [2]f32 @ 2,
) -> (
    f_color: [3]f32 @ 0,
) {
    color  := texture[v_tex_coords]
    f_color = max(dot(fs_uniforms.light_direction, normalize(v_normal)), 0) * color
}

```

```odin
spirv_code, errors := hephaistos.compile_shader(
    "shader.hep",
    #load("shader.hep"),
    shared_types = { Vertex_Shader_Constants, Fragment_Shader_Uniforms, },
)
```

### Attributes
| Attribute | Description |
| --- | --- |
| `uniform` | used for OpenGL style uniforms of type scalar, vector or matrix and for images/samplers, requires `location`/`binding` attribute for OpenGL style uniforms/textures respectively |
| `uniform_buffer` | Uniform buffer of type struct or array, requires `binding` attribute |
| `storage_buffer` | Storage buffer of type struct or array, requires `binding` attribute |
| `push_constant` | Vulkan push constant of type struct or array, requires `binding` attribute |
| `binding` | Binding point of interface variable |
| `location` | Uniform location for interface variables with `uniform` attribute |
| `descriptor_set` | Descriptor set of interface variable, defaults to 0 |
| `fragment_shader` | Fragment Shader |
| `vertex_shader` | Vertex Shader |
| `compute_shader` | Compute Shader |
| `geometry_shader` | Geometry Shader |
| `local_size` | Compute shader local size, defaults to `{1, 1, 1}` |
| `read_only` | Prohibit writes to variable |
| `link_name` | Set link name of shader entry point, defaults to name of procedure |

## OpenGL
When using Hephaistos with OpenGL you will have to specify the SPIR-V version as 1.0:
```odin
hephaistos.compile_shader(..., spirv_version = hephaistos.SPIR_V_VERSION_1_0, ...)
```

## Vulkan
When using Hephaistos with Vulkan the "scalarBlockLayout" feature needs to be enabled for the physical device (standardized in 1.2, available as an extension otherwise; As of 1.4 support for scalarBlockLayout is required).

# Special Thanks
Without [Ginger Bill](https://github.com/gingerBill/) there is no world where this compiler would exist. A huge thank you for writing the Odin compiler and also for writing [titania](https://github.com/gingerBill/titania), the most readable compiler I have ever seen.
