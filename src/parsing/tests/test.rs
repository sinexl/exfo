use crate::analysis::type_system::type_context::TypeCtx;
use crate::ast::tree_printer::DisplayStatement;
use crate::common::errors_warnings::display::DisplayErrorExtension;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::ptr::addr_of_mut;

#[test]
pub fn parser() {
    let ast_bump = Bump::new();
    let type_bump = Bump::new();
    let err_bump = Bump::new();
    let mut type_ctx = TypeCtx::new(&type_bump);
    let mut lexer = Lexer::file("./src/dev/parser.exfo");
    let (tokens, errors) = lexer.accumulate();
    assert!(errors.is_empty());
    let mut this = Parser::new(tokens.into(), &ast_bump, &err_bump, addr_of_mut!(type_ctx));
    let (statements, errors) = this.parse_program();

    for e in &errors {
        println!("{}", e.display(()));
    }
    if !errors.is_empty() {
        panic!();
    }

    let type_ctx = TypeCtx::new(&type_bump);
    for statement in statements {
        println!("{}", DisplayStatement(statement, &type_ctx));
    }
}
