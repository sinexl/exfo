use bumpalo::Bump;
use crate::analysis::resolver::Resolver;
use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::common::CompilerError;
use crate::compiler_io::util::get_line;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;

pub fn dev_repl() {
    let mut exit = false;
    while !exit {
        let ast_alloc = Bump::new();
        let ir_alloc = Bump::new();
        let mut static_errors: Vec<Box<dyn CompilerError>> = vec![];
        let input = get_line("> ");

        if input == ":quit" {
            exit = true;
        }

        let (tokens, errors) = Lexer::new(&input, "<REPL>").accumulate();
        crate::push_errors!(static_errors, errors);

        let mut parser = Parser::new(tokens.into(), &ast_alloc);
        let (statements, errors) = parser.parse_program();
        crate::push_errors!(static_errors, errors);

        let mut resolver = Resolver::new();
        let errors = resolver.resolve_statements(statements);
        crate::push_errors!(static_errors, errors);

        for statement in statements {
            println!("{}", statement);
        }
        for statement in statements {
            println!("{}", PrefixPrintStatement(statement));
        }

        if static_errors.is_empty() {
            let mut comp = Compiler::new(&ir_alloc, &ast_alloc, resolver.resolutions);
            comp.compile_statements(statements);
            println!("{ir}", ir = comp.ir);
        } else {
            for e in static_errors {
                println!("{e}");
            }
        }
    }
}


