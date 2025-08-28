use crate::ast::prefix_printer::PrefixPrint;
use crate::common::SourceLocation;
use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser, ParserErrorKind};
use bumpalo::Bump;
use std::rc::Rc;

#[test]
pub fn simple() {
    assert_eq!(success("1"), "1");
    assert_eq!(success("1 + 2 + 3"), "(+ (+ 1 2) 3)");
}

#[test]
pub fn parens() {
    assert_eq!(success("(1 +  2)"), "(+ 1 2)");
    assert_eq!(success("(1)"), "1");
    assert_eq!(fail("(1"), ParseError {
        kind: ParserErrorKind::UnbalancedParens,
        location: SourceLocation::new(Rc::from("<testcase>"), 1, 1),
    })
}

#[test]
pub fn precedence_simple() {
    assert_eq!(success("1 + 2 * 3"), "(+ 1 (* 2 3))");
}

#[test]
pub fn precedence_multiple() {
    assert_eq!(success("1 + 2 * 3 - 4 / 5"), "(- (+ 1 (* 2 3)) (/ 4 5))");
    assert_eq!(success("1 * 2 / 3 + 4 - 5"), "(- (+ (/ (* 1 2) 3) 4) 5)");
}

#[test]
pub fn left_associativity() {
    assert_eq!(success("1 + 2 + 3"), "(+ (+ 1 2) 3)");
    assert_eq!(success("1 * 2 * 3"), "(* (* 1 2) 3)");
    assert_eq!(success("1 == 2 == 3"), "(== (== 1 2) 3)");
}

#[test]
pub fn precedence_comparison() {
    assert_eq!(success("1 + 2 < 3"), "(< (+ 1 2) 3)");
    assert_eq!(success("1 == 2 > 3"), "(== 1 (> 2 3))");
    assert_eq!(success("1 < 2 == 3 + 4"), "(== (< 1 2) (+ 3 4))");
    assert_eq!(success("1 + 2 != 3 / 4"), "(!= (+ 1 2) (/ 3 4))");
}

pub fn success(expr: &str) -> String {
    let (tokens, errors) = Lexer::new(expr, "<testcase>").accumulate();
    assert_eq!(errors.len(), 0);
    let test_alloc = Bump::new();
    let v = Parser::new(tokens.into(), &test_alloc)
        .parse_expression()
        .unwrap();
    println!("{}", v);
    format!("{}", PrefixPrint(v))
}

pub fn fail(expr: &str) -> ParseError {
    let (tokens, errors) = Lexer::new(expr, "<testcase>").accumulate();
    assert_eq!(errors.len(), 0);
    let test_alloc = Bump::new();
    Parser::new(tokens.into(), &test_alloc)
        .parse_expression().expect_err("should fail")
}
