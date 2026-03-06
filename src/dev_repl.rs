use crate::analysis::resolver::Resolver;
use crate::analysis::type_system::type_context::TypeCtx;
use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::ast::tree_printer::DisplayStatement;
use crate::common::errors_warnings::CompilerError;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::ptr::addr_of_mut;
use exfo::compiler_io::util::get_line;
use crate::analysis::type_system::typechecker::Typechecker;
use crate::common::symbol_table::Transform;
use crate::push_errors;

pub fn dev_repl() {
    let mut exit = false;
    while !exit {
        let ir_alloc = Bump::new();
        let ast_alloc = Bump::new();
        let type_alloc = Bump::new();
        let mut static_errors: Vec<Box<dyn CompilerError>> = vec![];
        let input = get_line("> ");

        let mut types = TypeCtx::new(&type_alloc);
        let types_ptr = addr_of_mut!(types);

        if input == ":quit" {
            exit = true;
        }

        let (tokens, errors) = Lexer::new(&input, "<REPL>").accumulate();
        push_errors!(static_errors, errors);

        let mut parser = Parser::new(tokens.into(), &ast_alloc, types_ptr);
        let (statements, errors) = parser.parse_program();
        push_errors!(static_errors, errors);

        let mut resolver = Resolver::new();
        let errors = resolver.resolve_statements(statements);
        push_errors!(static_errors, errors);

        let mut type_checker = Typechecker::new(types_ptr, parser.count_symbols());
        let errors = type_checker.typecheck_statements(statements);
        if let Err(e) = errors {

            push_errors!(static_errors, e);
        }

        for statement in statements {
            println!("{}", DisplayStatement(statement, &types));
        }
        for statement in statements {
            println!("{}", PrefixPrintStatement(statement, &types));
        }

        if static_errors.is_empty() {
            let mut comp = Compiler::new(&ir_alloc, types_ptr, type_checker.symbols.transform());
            comp.compile_statements(statements);
            println!("{ir}", ir = comp.ir);
        } else {
            for e in static_errors {
                println!("{e}");
            }
        }
    }
}
