use std::cell::Cell;
use crate::analysis::r#type::Type;
use crate::ast::expression::Expression;
use crate::common::{Identifier, SourceLocation};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub loc: SourceLocation,
}

#[derive(Debug)]
pub enum StatementKind<'a> {
    ExpressionStatement(&'a Expression<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    Block(&'a [&'a Statement<'a>]),
    Return(Option<&'a Expression<'a>>),
}

#[derive(Debug)]
pub struct FunctionDeclaration<'ast> {
    pub name: Identifier<'ast>,
    pub parameters: &'ast [FunctionParameter<'ast>],
    pub body: &'ast [&'ast Statement<'ast>],
    pub return_type: Cell<Type<'ast>>, 
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub name: Identifier<'a>,
    pub initializer: Option<&'a Expression<'a>>,
    pub ty: Cell<Type<'a>>, 
}

#[derive(Debug)]
pub struct FunctionParameter<'ast> {
    pub name: Identifier<'ast>,
    pub ty: Type<'ast>,
}

impl<'ast> Display for FunctionParameter<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.name, self.ty)
    }
}
