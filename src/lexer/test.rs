use crate::lexer::Lexer;

#[test]
pub fn test_lexer() {
    let path = "./src/QuickTests/lexer.exfo";
    let (tokens, errors) = Lexer::file(path).accumulate();
    for error in errors {
        println!("{}", error);
    }

    for token in tokens {
        println!("{}", token);
    }
}