use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use crate::analysis::r#type::TypeCtx;
use crate::ast::tree_printer::DisplayStatement;

#[test]
pub fn parser() {
    let ast_bump = Bump::new();
    let type_bump = Bump::new();
    let mut lexer = Lexer::file("./src/QuickTests/parser.exfo");
    let (tokens, errors) = lexer.accumulate();
    assert!(errors.is_empty());
    let mut this = Parser::new(tokens.into(), &ast_bump, &type_bump);
    let (statements, errors) = this.parse_program();

    for e in &errors {
        println!("{:?}", e);
    }
    if !errors.is_empty() {
        panic!();
    }

    let type_ctx = TypeCtx::new(&type_bump);
    for statement in statements {
        println!("{}", DisplayStatement(statement, &type_ctx));
    }
}
