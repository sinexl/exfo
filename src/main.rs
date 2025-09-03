use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::common::CompilerError;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::io::Write;
use std::{env, fs};

mod ast;
pub mod common;
mod compiling;
pub mod lexing;
mod parsing;
mod simple_interpreter;

fn get_line(msg: &str) -> String {
    let mut res = String::new();
    print!("{msg}");
    let _ = std::io::stdout().flush();
    std::io::stdin().read_line(&mut res).unwrap();
    if let Some('\n') = res.chars().next_back() {
        res.pop();
    };
    if let Some('\r') = res.chars().next_back() {
        res.pop();
    };
    res
}
fn repl() {
    let mut exit = false;
    while !exit {
        let mut compilation_errors: Vec<&dyn CompilerError> = vec![];
        let input = get_line("> ");

        if input == ":quit" {
            exit = true;
        }

        let (tokens, errors) = Lexer::new(&input, "<REPL>").accumulate();
        compilation_errors.reserve(errors.len());
        errors
            .iter()
            .for_each(|e| compilation_errors.push(e as &dyn CompilerError));

        let bump = Bump::new();
        let mut parser = Parser::new(tokens.into(), &bump);
        let (statements, errors) = parser.parse_program();
        errors
            .iter()
            .for_each(|e| compilation_errors.push(e as &dyn CompilerError));

        if compilation_errors.is_empty() {
            for statement in statements {
                println!("{}", statement);
            }
            for statement in statements {
                println!("{}", PrefixPrintStatement(statement));
            }
        } else {
            for e in compilation_errors {
                println!("{e}");
            }
        }
    }
}

fn main() -> Result<(), ()> {
    if env::args().next_back() == Some("--repl".to_owned()) {
        repl();
        return Ok(());
    }
    let path = if let Some(filename) = env::args().nth(1) {
        filename
    } else {
        return Err(());
    };

    let file = fs::read_to_string(&path).map_err(|e| eprintln!("{}", e))?;

    let (tokens, errors) = Lexer::new(&file, &path).accumulate();

    for token in &tokens {
        println!("{}", token);
    }
    for error in &errors {
        println!("{}", error)
    }
    let ast_alloc = Bump::new();
    let mut parser = Parser::new(tokens.into(), &ast_alloc);
    let (ast, errors) = parser.parse_program();
    for error in &errors {
        println!("{}", error as &dyn CompilerError);
    }
    assert_eq!(errors.len(), 0);

    for node in ast {
        println!("{}", node);
    }
    let ir_bump = Bump::new();
    let mut compiler = Compiler::new(&ir_bump);

    compiler.compile_statements(ast);
    let ir = compiler.ir;

    println!("{ir:#?}");

    Ok(())
}
