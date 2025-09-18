use crate::analysis::r#type::Type;
use crate::ast::expression::Expression;
use crate::common::{Identifier, SourceLocation};
use std::fmt::{Display, Formatter};

pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub loc: SourceLocation,
}

pub enum StatementKind<'a> {
    ExpressionStatement(&'a Expression<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    Block(&'a [&'a Statement<'a>]),
}

pub struct FunctionDeclaration<'ast> {
    pub name: Identifier<'ast>,
    pub parameters: &'ast [FunctionParameter<'ast>],
    pub body: &'ast [&'ast Statement<'ast>],
}

pub struct FunctionParameter<'ast> {
    pub name: Identifier<'ast>,
    pub ty: Type<'ast>,
}

impl<'ast> Display for FunctionParameter<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.name, self.ty)
    }
}

pub struct VariableDeclaration<'a> {
    pub name: Identifier<'a>,
    pub initializer: Option<&'a Expression<'a>>,
}
