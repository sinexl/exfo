use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use crate::parsing::tests::test_precedence::PATH;
use bumpalo::Bump;

#[test]
pub fn simple_expression_statements() {
    assert_eq!(single("a = 10;"), "(= a 10);");
}

pub fn single(input: &str) -> String {
    let (t, e) = Lexer::new(input, PATH).accumulate();
    assert_eq!(e.len(), 0);

    let mut bump = Bump::new();
    let mut p = Parser::new(t.into(), &mut bump);
    let (expr, e) = p.parse_program();
    assert_eq!(e.len(), 0);
    assert_eq!(expr.len(), 1);

    format!("{}", PrefixPrintStatement(expr[0]))
}
