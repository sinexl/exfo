use crate::lexer::Lexer;
use std::fs;

pub mod common;
pub mod lexer;
fn main() -> Result<(), ()> {
    let path = "test.exfo";
    let file = fs::read_to_string(path).map_err(|e| eprintln!("{}", e))?;

    let mut lexer = Lexer::new(&file, path);

    while let Some(tk) = lexer.next_token() {
        println!("{tk}");
    }

    Ok(())
}
