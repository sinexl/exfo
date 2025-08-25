use crate::lexer::Lexer;
use std::fs;

pub mod common;
pub mod lexer;
mod ast;
mod simple_interpreter;

fn main() -> Result<(), ()> {
    let path = "test.exfo";
    let file = fs::read_to_string(path).map_err(|e| eprintln!("{}", e))?;

    let (tokens, errors) = Lexer::new(&file, path).accumulate();

    for token in tokens {
        println!("{}", token);
    }
    for error in errors {
        println!("{}", error)
    }



    Ok(())
}
