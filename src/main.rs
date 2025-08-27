use crate::lexer::Lexer;
use crate::parser::Parser;
use bumpalo::Bump;
use std::fs;

mod ast;
pub mod common;
pub mod lexer;
mod parser;
mod simple_interpreter;

fn main() -> Result<(), ()> {
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

    Ok(())
}
