use crate::analysis::resolver::Resolver;
use crate::analysis::type_system::type_context::TypeCtx;
use crate::analysis::type_system::typechecker::Typechecker;
use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::ast::tree_printer::DisplayStatement;
use crate::common::static_errors::StaticErrors;
use crate::common::symbol_table::Transform;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use exfo::compiler_io::util::get_line;
use std::ptr::addr_of_mut;

pub fn dev_repl() {
    let mut exit = false;
    while !exit {
        let ir_alloc = Bump::new();
        let ast_alloc = Bump::new();
        let type_alloc = Bump::new();
        let errors_alloc = Bump::new();
        let mut static_errors = StaticErrors::new(&errors_alloc);
        let input = get_line("> ");

        let mut types = TypeCtx::new(&type_alloc);
        let types_ptr = addr_of_mut!(types);

        if input == ":quit" {
            exit = true;
        }

        let (tokens, errors) = Lexer::new(&input, "<REPL>").accumulate();
        static_errors.lexer(errors);

        let mut parser = Parser::new(tokens.into(), &ast_alloc, &errors_alloc, types_ptr);
        let (statements, errors) = parser.parse_program();
        static_errors.parser(errors);

        let mut resolver = Resolver::new(&errors_alloc);
        let errors = resolver.resolve_statements(statements);
        static_errors.resolver(errors);

        let mut type_checker = Typechecker::new(&type_alloc, &errors_alloc, types_ptr, parser.count_symbols());
        let errors = type_checker.typecheck_statements(statements);
        if let Err(e) = errors {

            static_errors.typechecker(e);
        }

        for statement in statements {
            println!("{}", DisplayStatement(statement, &types));
        }
        for statement in statements {
            println!("{}", PrefixPrintStatement(statement, &types));
        }

        if static_errors.len() == 0 {
            let mut comp = Compiler::new(&ir_alloc, types_ptr, type_checker.symbols.transform());
            comp.compile_statements(statements);
            println!("{ir}", ir = comp.ir);
        } else {

            let mut e = String::new();
            static_errors.print(&mut e, &types).unwrap();
            eprintln!("{}", e);
            std::process::exit(-1);
        }
    }
}
