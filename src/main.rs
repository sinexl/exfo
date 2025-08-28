use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::fs;

mod ast;
pub mod common;
pub mod lexing;
mod parsing;
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

    println!("{}", ast);


    Ok(())
}
