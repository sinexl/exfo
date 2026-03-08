use crate::ast::expression::AstLiteral::Integral;
use crate::ast::expression::ExpressionKind;
use crate::common::SourceLocation;
use crate::parsing::tests::machinery::expression::*;
use bumpalo::Bump;
use std::rc::Rc;
use crate::parsing::errors::{ParseError, ParseErrorKind};

pub const PATH: &str = "<testcase>";

#[test]
pub fn simple() {
    assert_eq!(single("1"), "1");
    assert_eq!(single("1 + 2 + 3"), "(+ (+ 1 2) 3)");
    assert_eq!(single("-1 + 2"), "(+ (- 1) 2)");
}

#[test]
pub fn parens() {
    assert_eq!(single("(1 +  2)"), "(+ 1 2)");
    assert_eq!(single("(1)"), "1");
    assert_eq!(single("(1 + 2) * 3"), "(* (+ 1 2) 3)");
    assert_eq!(single("1 * (2 + 3)"), "(* 1 (+ 2 3))");
    assert_eq!(single("(1 + (2 * 3))"), "(+ 1 (* 2 3))");
    assert_eq!(single("((1) + 2)"), "(+ 1 2)");
    assert_eq!(single("((1 + 2) * 3) / 4"), "(/ (* (+ 1 2) 3) 4)");
}

#[test]
pub fn parens_unbalanced() {
    let alloc = Bump::new();
    let e = &alloc;
    assert_eq!(
        fail(e, "(1"),
        ParseError {
            kind: ParseErrorKind::UnbalancedParens,
            loc: SourceLocation::new(Rc::from(PATH), 1, 1),
        }
    );
    assert_eq!(
        fail(e, " ((1 + 2)"),
        ParseError {
            kind: ParseErrorKind::UnbalancedParens,
            loc: SourceLocation {
                line: 1,
                offset: 2,
                file: Rc::from(PATH)
            }
        }
    );
}

#[test]
pub fn precedence_simple() {
    assert_eq!(single("1 + 2 * 3"), "(+ 1 (* 2 3))");
}

#[test]
pub fn precedence_multiple() {
    assert_eq!(single("1 + 2 * 3 - 4 / 5"), "(- (+ 1 (* 2 3)) (/ 4 5))");
    assert_eq!(single("1 * 2 / 3 + 4 - 5"), "(- (+ (/ (* 1 2) 3) 4) 5)");

    assert_eq!(
        single("a = 1 + 2 * 3 - 4 / 5"),
        "(= (a<#?>) (- (+ 1 (* 2 3)) (/ 4 5)))"
    );

    assert_eq!(
        single("b = 1 * 2 / 3 + 4 - 5"),
        "(= (b<#?>) (- (+ (/ (* 1 2) 3) 4) 5))"
    );

    assert_eq!(single("a = b = 1 + 2"), "(= (a<#?>) (= (b<#?>) (+ 1 2)))");
}

#[test]
pub fn left_associativity() {
    assert_eq!(single("1 + 2 + 3"), "(+ (+ 1 2) 3)");
    assert_eq!(single("1 * 2 * 3"), "(* (* 1 2) 3)");
    assert_eq!(single("1 == 2 == 3"), "(== (== 1 2) 3)");
}

#[test]
pub fn assignment_and_right_associativity() {
    assert_eq!(single("a = b = c"), "(= (a<#?>) (= (b<#?>) (c<#?>)))");
    assert_eq!(single("(a = b) = c"), "(= (= (a<#?>) (b<#?>)) (c<#?>))");
    assert_eq!(single("(a = 1) + 2"), "(+ (= (a<#?>) 1) 2)");
    assert_eq!(single("a = 1 + 2"), "(= (a<#?>) (+ 1 2))");
}

#[test]
pub fn invalid_assignment() {
    let e = Bump::new();
    let e = &e;
    let msg = ExpressionKind::Literal(Integral(0)).humanize();
    assert_eq!(
        fail(e, "1 = 2"),
        ParseError {
            kind: ParseErrorKind::InvalidAssignment(msg),
            loc: SourceLocation::new(Rc::from(PATH), 1, 1),
        }
    );
    assert_eq!(
        fail(e, "(1 + a = 5) = 3").kind,
        ParseErrorKind::InvalidAssignment("binary operation")
    );
}

#[test]
pub fn invalid_address_of() {
    let msg = ExpressionKind::Literal(Integral(0)).humanize();
    let e = Bump::new();
    let e = &e;
    assert_eq!(
        fail(e, "a = &2"),
        ParseError {
            kind: ParseErrorKind::InvalidAddressOf(msg),
            loc: SourceLocation::new(Rc::from(PATH), 1, 5),
        }
    );
    assert_eq!(
        fail(e, "a = &(b + 1)").kind,
        ParseErrorKind::InvalidAddressOf("binary operation")
    );
}

#[test]
pub fn precedence_comparison() {
    assert_eq!(single("1 + 2 < 3"), "(< (+ 1 2) 3)");
    assert_eq!(single("1 == 2 > 3"), "(== 1 (> 2 3))");
    assert_eq!(single("1 < 2 == 3 + 4"), "(== (< 1 2) (+ 3 4))");
    assert_eq!(single("1 + 2 != 3 / 4"), "(!= (+ 1 2) (/ 3 4))");
}

#[test]
pub fn unary() {
    assert_eq!(single("-1 * 2"), "(* (- 1) 2)");
    assert_eq!(single("-1 * -2"), "(* (- 1) (- 2))");
    assert_eq!(single("!1 || !1"), "(|| (! 1) (! 1))");
    assert_eq!(single("!2 || 1 && !1"), "(|| (! 2) (&& 1 (! 1)))");
}

#[test]
pub fn logical() {
    assert_eq!(single("!1 && 2 || 3"), "(|| (&& (! 1) 2) 3)");
    assert_eq!(single("!1 == 2"), "(== (! 1) 2)");
    // XNOR
    assert_eq!(
        single("1 && 2 || !1 && !2"),
        "(|| (&& 1 2) (&& (! 1) (! 2)))"
    );
    // XOR
    assert_eq!(
        single("1 && !2 || !1 && 2"),
        "(|| (&& 1 (! 2)) (&& (! 1) 2))"
    );
}

#[test]
pub fn call() {
    assert_eq!(single("a()"), "(call (a<#?>))");
    assert_eq!(single("a(1)(2)"), "(call (call (a<#?>) 1) 2)");
    assert_eq!(
        single("a(1 + 2, 3 / 4 * 5, c = d = 10)"),
        "(call (a<#?>) (+ 1 2) (* (/ 3 4) 5) (= (c<#?>) (= (d<#?>) 10)))"
    )
}
