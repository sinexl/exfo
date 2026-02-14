use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use crate::parsing::tests::test_precedence::PATH;
use bumpalo::Bump;
use crate::analysis::r#type::TypeCtx;

#[test]
pub fn simple_expression_statements() {
    assert_eq!(single("a = 10;"), "(= a 10);");
}

pub fn single(input: &str) -> String {
    let (t, e) = Lexer::new(input, PATH).accumulate();
    assert_eq!(e.len(), 0);

    let ast_bump = Bump::new();
    let type_bump = Bump::new();
    let mut p = Parser::new(t.into(), &ast_bump, &type_bump);
    let (expr, e) = p.parse_program();
    assert_eq!(e.len(), 0);
    assert_eq!(expr.len(), 1);

    let type_ctx = TypeCtx::new(&type_bump);
    format!("{}", PrefixPrintStatement(expr[0], &type_ctx))
}
