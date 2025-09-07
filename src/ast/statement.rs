use crate::ast::expression::Expression;
use crate::common::{Identifier, SourceLocation};

pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub loc: SourceLocation,
}

pub enum StatementKind<'a> {
    ExpressionStatement(&'a Expression<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    Block(&'a [&'a Statement<'a>]),
}

pub struct FunctionDeclaration<'a> {
    pub name: Identifier<'a>,
    // todo: parameters
    pub body: &'a [&'a Statement<'a>],
}
