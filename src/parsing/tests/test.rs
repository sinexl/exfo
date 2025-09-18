use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;

#[test]
pub fn parser() {
    let ir = Bump::new();
    let mut lexer = Lexer::file("./src/QuickTests/parser.exfo");
    let (tokens, errors) = lexer.accumulate();
    assert!(errors.is_empty());
    let mut this = Parser::new(tokens.into(), &ir);
    let (statements, errors) = this.parse_program();

    for e in &errors {
        println!("{:?}", e);
    }
    if !errors.is_empty() {
        panic!(); 
    }

    for statement in statements {
        println!("{}", statement);
    }
}
