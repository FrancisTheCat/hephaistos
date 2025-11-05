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

@(uniform, binding = 0)
fs_uniforms: #import(Fragment_Shader_Uniforms)

@(binding = 1)
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

```
spirv_code, errors := hephaistos.compile_shader(
    "shader.hep",
    #load("shader.hep"),
    shared_types = { Vertex_Shader_Constants, Fragment_Shader_Uniforms, },
)
```

# Special Thanks
Without [Ginger Bill](https://github.com/gingerBill/) there is no world where this compiler would exist. A huge thank you for writing the Odin compiler and also for writing [titania](https://github.com/gingerBill/titania), the most readable compiler I have ever seen.
