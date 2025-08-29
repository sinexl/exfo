use crate::ast::prefix_printer::PrefixPrint;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::io::Write;
use std::{env, fs};

mod ast;
pub mod common;
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
        let input = get_line("> ");
        let (tokens, errors) = Lexer::new(&input, "<REPL>").accumulate();
        if errors.len() > 0 {
            errors.iter().for_each(|err| {
                println!("{}", err);
                exit = true
            });
        }
        let bump = Bump::new();
        let mut parser = Parser::new(tokens.into(), &bump);
        let expr = parser.parse_expression().unwrap();
        println!("{}", expr);
        println!("{}", PrefixPrint(expr));


    }
}

fn main() -> Result<(), ()> {
    if env::args().next_back() == Some("--repl".to_owned()) {
        repl();
    }

    let path = "./src/QuickTests/main.exfo";
    let file = fs::read_to_string(path).map_err(|e| eprintln!("{}", e))?;

    let (tokens, errors) = Lexer::new(&file, path).accumulate();

    for token in &tokens {
        println!("{}", token);
    }
    for error in &errors {
        println!("{}", error)
    }
    let ast_alloc = Bump::new();
    let mut parser = Parser::new(tokens.into(), &ast_alloc);
    let ast = parser
        .parse_expression()
        .map_err(|e| eprintln!("{:?}", e))?;

    println!("{}", ast);

    Ok(())
}
