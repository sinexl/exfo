use std::ptr::addr_of_mut;
use crate::ast::expression::AstLiteral::Integral;
use crate::ast::expression::ExpressionKind;
use crate::ast::prefix_printer::PrefixPrint;
use crate::common::SourceLocation;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::{ParseError, ParseErrorKind, Parser};
use bumpalo::Bump;
use std::rc::Rc;
use crate::analysis::r#type::TypeCtx;

pub const PATH: &str = "<testcase>";

#[test]
pub fn simple() {
    assert_eq!(success("1"), "1");
    assert_eq!(success("1 + 2 + 3"), "(+ (+ 1 2) 3)");
    assert_eq!(success("-1 + 2"), "(+ (- 1) 2)");
}

#[test]
pub fn parens() {
    assert_eq!(success("(1 +  2)"), "(+ 1 2)");
    assert_eq!(success("(1)"), "1");
    assert_eq!(success("(1 + 2) * 3"), "(* (+ 1 2) 3)");
    assert_eq!(success("1 * (2 + 3)"), "(* 1 (+ 2 3))");
    assert_eq!(success("(1 + (2 * 3))"), "(+ 1 (* 2 3))");
    assert_eq!(success("((1) + 2)"), "(+ 1 2)");
    assert_eq!(success("((1 + 2) * 3) / 4"), "(/ (* (+ 1 2) 3) 4)");
}

#[test]
pub fn parens_unbalanced() {
    assert_eq!(
        fail("(1"),
        ParseError {
            kind: ParseErrorKind::UnbalancedParens,
            location: SourceLocation::new(Rc::from(PATH), 1, 1),
        }
    );
    assert_eq!(
        fail(" ((1 + 2)"),
        ParseError {
            kind: ParseErrorKind::UnbalancedParens,
            location: SourceLocation {
                line: 1,
                offset: 2,
                file: Rc::from(PATH)
            }
        }
    );
}

#[test]
pub fn precedence_simple() {
    assert_eq!(success("1 + 2 * 3"), "(+ 1 (* 2 3))");
}

#[test]
pub fn precedence_multiple() {
    assert_eq!(success("1 + 2 * 3 - 4 / 5"), "(- (+ 1 (* 2 3)) (/ 4 5))");
    assert_eq!(success("1 * 2 / 3 + 4 - 5"), "(- (+ (/ (* 1 2) 3) 4) 5)");

    assert_eq!(
        success("a = 1 + 2 * 3 - 4 / 5"),
        "(= a (- (+ 1 (* 2 3)) (/ 4 5)))"
    );

    assert_eq!(
        success("b = 1 * 2 / 3 + 4 - 5"),
        "(= b (- (+ (/ (* 1 2) 3) 4) 5))"
    );

    assert_eq!(success("a = b = 1 + 2"), "(= a (= b (+ 1 2)))");
}

#[test]
pub fn left_associativity() {
    assert_eq!(success("1 + 2 + 3"), "(+ (+ 1 2) 3)");
    assert_eq!(success("1 * 2 * 3"), "(* (* 1 2) 3)");
    assert_eq!(success("1 == 2 == 3"), "(== (== 1 2) 3)");
}

#[test]
pub fn assignment_and_right_associativity() {
    assert_eq!(success("a = b = c"), "(= a (= b c))");
    assert_eq!(success("(a = b) = c"), "(= (= a b) c)");
    assert_eq!(success("(a = 1) + 2"), "(+ (= a 1) 2)");
    assert_eq!(success("a = 1 + 2"), "(= a (+ 1 2))");
}

#[test]
pub fn invalid_assignment() {
    let msg = ExpressionKind::Literal(Integral(0)).humanize();
    assert_eq!(
        fail("1 = 2"),
        ParseError {
            kind: ParseErrorKind::InvalidAssignment(msg.clone()),
            location: SourceLocation::new(Rc::from(PATH), 1, 1),
        }
    );
    assert_eq!(
        fail("(1 + a = 5) = 3").kind,
        ParseErrorKind::InvalidAssignment("binary operation".into())
    );
}

#[test]
pub fn precedence_comparison() {
    assert_eq!(success("1 + 2 < 3"), "(< (+ 1 2) 3)");
    assert_eq!(success("1 == 2 > 3"), "(== 1 (> 2 3))");
    assert_eq!(success("1 < 2 == 3 + 4"), "(== (< 1 2) (+ 3 4))");
    assert_eq!(success("1 + 2 != 3 / 4"), "(!= (+ 1 2) (/ 3 4))");
}

#[test]
pub fn unary() {
    assert_eq!(success("-1 * 2"), "(* (- 1) 2)");
    assert_eq!(success("-1 * -2"), "(* (- 1) (- 2))");
}

#[test]
pub fn call() {
    assert_eq!(success("a()"), "(call a)");
    assert_eq!(success("a(1)(2)"), "(call (call a 1) 2)");
    assert_eq!(
        success("a(1 + 2, 3 / 4 * 5, c = d = 10)"),
        "(call a (+ 1 2) (* (/ 3 4) 5) (= c (= d 10)))"
    )
}

pub fn success(expr: &str) -> String {
    let (tokens, errors) = Lexer::new(expr, PATH).accumulate();
    assert_eq!(errors.len(), 0);
    let test_alloc = Bump::new();
    let type_alloc = Bump::new();
    let mut type_ctx = TypeCtx::new(&type_alloc);
    let v = Parser::new(tokens.into(), &test_alloc, addr_of_mut!(type_ctx))
        .parse_expression()
        .unwrap();
    println!("{}", PrefixPrint(v));
    format!("{}", PrefixPrint(v))
}

pub fn fail(expr: &str) -> ParseError {
    let (tokens, errors) = Lexer::new(expr, PATH).accumulate();
    assert_eq!(errors.len(), 0);
    let test_alloc = Bump::new();
    let mut type_ctx = TypeCtx::new(&test_alloc);
    Parser::new(tokens.into(), &test_alloc, addr_of_mut!(type_ctx))
        .parse_expression()
        .expect_err("should fail")
}
