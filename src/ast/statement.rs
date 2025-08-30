use crate::ast::expression::Expression;
use crate::common::SourceLocation;

pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub loc: SourceLocation,
}

pub enum StatementKind<'a> {
    ExpressionStatement(&'a Expression<'a>),
}
