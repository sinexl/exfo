use crate::parsing::tests::machinery::statement::*;

#[test]
pub fn simple_expression_statements() {
    assert_eq!(single("a = 10;"), "((= a<#?> 10))");
}

