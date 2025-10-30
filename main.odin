package hephaistos

import "base:runtime"

import "core:io"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:prof/spall"
import "core:strings"
import "core:slice"
import "core:terminal/ansi"
import glm "core:math/linalg/glsl"

import "tokenizer"
import "parser"
import "ast"
import "checker"
import "types"
import "cg"

Token    :: tokenizer.Token
Error    :: tokenizer.Error
tokenize :: tokenizer.tokenize

parse :: parser.parse

check   :: checker.check
Checker :: checker.Checker

Const_Value :: types.Const_Value
Type        :: types.Type

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

compile_shader :: proc(
	source:       string,
	path:         string,
	defines:      map[string]Const_Value = {},
	shared_types: []typeid = {},
	allocator       := context.allocator,
	error_allocator := context.allocator,
) -> (code: []u32, errors: []Error) {
	tokens: []Token
	tokens, errors = tokenize(source, false, context.temp_allocator, error_allocator)
	if len(errors) != 0 {
		return
	}

	stmts: []^Ast_Stmt
	stmts, errors = parse(tokens, context.temp_allocator, error_allocator)
	if len(errors) != 0 {
		return
	}

	checker: Checker
	checker, errors = check(stmts, defines, shared_types, context.temp_allocator, error_allocator)
	if len(errors) != 0 {
		return
	}

	code = cg.generate(&checker, stmts, path, source, allocator)

	return
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

	user_formatters: map[typeid]fmt.User_Formatter
	fmt.set_user_formatters(&user_formatters)
	fmt.register_user_formatter(^Type, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		types.print_writer(fi.writer, arg.(^Type))
		return true
	})
	defer delete(user_formatters)

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

	defines: map[string]Const_Value
	defines["SOME_CONFIG_VAR"] = true
	defer delete(defines)

	file_name := "example.hep"
	source    := string(os.read_entire_file(file_name) or_else panic(""))
	defer delete(source)
	code, errors := compile_shader(
		source,
		file_name,
		defines         = defines,
		shared_types    = { Vertex_Shader_Uniforms, Shadow_Uniforms, Some_Enum, },
		error_allocator = context.temp_allocator,
	)
	defer delete(code)

	if len(errors) != 0 {
		lines := strings.split_lines(source, context.temp_allocator)
		for error in errors {
			print_error(os.stream_from_handle(os.stderr), file_name, lines, error)
		}
		return
	}

	os.write_entire_file("a.spv", slice.to_bytes(code))
}

print_error :: proc(w: io.Writer, file_name: string, lines: []string, error: tokenizer.Error) {
	if error.line == 0 {
		fmt.wprintf(w, ansi.CSI + ansi.FG_RED + ansi.SGR + "Error:" + ansi.CSI + ansi.RESET + ansi.SGR + " %s\n", error.message)
		return
	}
	fmt.wprintf(w, ansi.CSI + ansi.BOLD   + ansi.SGR + "%s(%v:%v) ", file_name, error.line, error.column)
	fmt.wprintf(w, ansi.CSI + ansi.FG_RED + ansi.SGR + "Error:" + ansi.CSI + ansi.RESET + ansi.SGR + " %s\n", error.message)
	fmt.wprintln(w, lines[error.line - 1])
	for i in 1 ..< error.column {
		if lines[error.line - 1][i - 1] == '\t' {
			fmt.wprint(w, "\t")
		} else {
			fmt.wprint(w, " ")
		}
	}
	fmt.wprint(w, ansi.CSI + ansi.FG_GREEN + ansi.SGR + "^")
	for _ in error.column ..< error.end.column - 2 {
		fmt.wprint(w, "~")
	}
	if error.column < error.end.column - 1 {
		fmt.wprint(w, "^")
	}
	fmt.wprintln(w, ansi.CSI + ansi.RESET + ansi.SGR)
}

Ast_Node           :: ast.Node
Ast_Expr           :: ast.Expr
Ast_Stmt           :: ast.Stmt
Ast_Decl           :: ast.Decl

Ast_Field          :: ast.Field
Ast_Switch_Case    :: ast.Switch_Case

Ast_Shader_Stage   :: ast.Shader_Stage
Ast_Builtin_Id     :: ast.Builtin_Id

Ast_Expr_Binary    :: ast.Expr_Binary
Ast_Expr_Unary     :: ast.Expr_Unary
Ast_Expr_Constant  :: ast.Expr_Constant
Ast_Expr_Ident     :: ast.Expr_Ident
Ast_Expr_Interface :: ast.Expr_Interface
Ast_Expr_Config    :: ast.Expr_Config
Ast_Expr_Proc_Lit  :: ast.Expr_Proc_Lit
Ast_Expr_Proc_Sig  :: ast.Expr_Proc_Sig
Ast_Expr_Call      :: ast.Expr_Call
Ast_Expr_Paren     :: ast.Expr_Paren
Ast_Expr_Selector  :: ast.Expr_Selector
Ast_Expr_Compound  :: ast.Expr_Compound
Ast_Expr_Index     :: ast.Expr_Index
Ast_Expr_Cast      :: ast.Expr_Cast

Ast_Type_Struct    :: ast.Type_Struct
Ast_Type_Array     :: ast.Type_Array
Ast_Type_Matrix    :: ast.Type_Matrix
Ast_Type_Import    :: ast.Type_Import
Ast_Type_Sampler   :: ast.Type_Sampler
Ast_Type_Enum      :: ast.Type_Enum

Ast_Decl_Value     :: ast.Decl_Value

Ast_Stmt_Return    :: ast.Stmt_Return
Ast_Stmt_Break     :: ast.Stmt_Break
Ast_Stmt_Continue  :: ast.Stmt_Continue
Ast_Stmt_For_Range :: ast.Stmt_For_Range
Ast_Stmt_For       :: ast.Stmt_For
Ast_Stmt_Block     :: ast.Stmt_Block
Ast_Stmt_If        :: ast.Stmt_If
Ast_Stmt_When      :: ast.Stmt_When
Ast_Stmt_Switch    :: ast.Stmt_Switch
Ast_Stmt_Assign    :: ast.Stmt_Assign
Ast_Stmt_Expr      :: ast.Stmt_Expr

Ast_Any_Node       :: ast.Any_Node
Ast_Any_Expr       :: ast.Any_Expr
Ast_Any_Decl       :: ast.Any_Decl
Ast_Any_Stmt       :: ast.Any_Stmt
