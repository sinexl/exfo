use crate::ast::expression::Expression;
use crate::common::SourceLocation;

pub struct Statement<'a> {
    kind: StatementKind<'a>,
    loc: SourceLocation,
}

pub enum StatementKind<'a> {
    ExpressionStatement(Expression<'a>),
    Print(Expression<'a>),
}