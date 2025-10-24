package hephaistos

import "core:io"
import "core:fmt"
import "core:os"
import "core:strings"
import "core:slice"
import "core:terminal/ansi"

import "ast"
import "tokenizer"
import "types"

compile_shader :: proc(
	source: string,
	allocator       := context.allocator,
	error_allocator := context.allocator,
) -> (code: []u32, errors: []tokenizer.Error) {
	tokens: []tokenizer.Token
	tokens, errors = tokenizer.tokenize(source, false, context.allocator, error_allocator)
	if len(errors) != 0 {
		return
	}

	stmts: []^ast.Stmt
	stmts, errors = parse(tokens, context.temp_allocator, error_allocator)
	if len(errors) != 0 {
		return
	}

	checker: Checker
	errors = check(&checker, stmts, context.temp_allocator, error_allocator)
	if len(errors) != 0 {
		return
	}

	code = cg_generate(&checker, stmts, allocator = context.allocator)

	return
}

main :: proc() {
	user_formatters: map[typeid]fmt.User_Formatter
	fmt.set_user_formatters(&user_formatters)
	fmt.register_user_formatter(^types.Type, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		types.print_writer(fi.writer, arg.(^types.Type))
		return true
	})

	file_name    := "test.hep"
	source       := string(os.read_entire_file(file_name) or_else panic(""))
	code, errors := compile_shader(source)

	if len(errors) != 0 {
		lines := strings.split_lines(source, context.temp_allocator)
		for error in errors {
			print_error(os.stream_from_handle(os.stderr), file_name, lines, error)
		}
		return
	}

	os.write_entire_file("a.spv", slice.to_bytes(code))
	fmt.println(code)
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
