use crate::analysis::r#type::{DisplayType, TypeIdCell};
use crate::analysis::type_context::TypeCtx;
use crate::ast::expression::{Expression, SymId};
use crate::common::identifier::Identifier;
use crate::common::SourceLocation;
use std::cell::Cell;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Statement<'ast, 'types> {
    pub kind: StatementKind<'ast, 'types>,
    pub loc: SourceLocation,
}

#[derive(Debug)]
pub enum StatementKind<'ast, 'types> {
    ExpressionStatement(&'ast Expression<'ast>),
    FunctionDeclaration(FunctionDeclaration<'ast, 'types>, SymId),
    VariableDeclaration(VariableDeclaration<'ast>, SymId),
    Block(&'ast [&'ast Statement<'ast, 'types>]),
    Return(Option<&'ast Expression<'ast>>),
    Extern(ExternalFunction<'ast, 'types>, SymId),
    If {
        condition: &'ast Expression<'ast>,
        then: &'ast Statement<'ast, 'types>,
        r#else: Option<&'ast Statement<'ast, 'types>>,
    },

    /// Fields of while, break and continue nodes:
    /// name: label in syntax.
    /// id: unique identifier.
    /// Name is only needed for resolver. All later passes should use id.
    While {
        condition: &'ast Expression<'ast>,
        body: &'ast Statement<'ast, 'types>,
        name: Option<Identifier<'ast>>,
        id: Cell<usize>,
    },
    Break {
        name: Option<Identifier<'ast>>,
        id: Cell<usize>,
    },
    Continue {
        name: Option<Identifier<'ast>>,
        id: Cell<usize>,
    },
}

#[derive(Debug)]
pub struct FunctionDeclaration<'ast, 'types> {
    pub name: Identifier<'ast>,
    pub parameters: &'ast [FunctionParameter<'ast>],
    pub body: &'ast [&'ast Statement<'ast, 'types>],
    pub return_type: TypeIdCell,
}

#[derive(Debug)]
pub struct ExternalFunction<'ast, 'types> {
    pub name: Identifier<'ast>,
    pub kind: ExternKind,
    pub parameters: &'types [TypeIdCell],
    pub return_type: TypeIdCell,
    pub is_variadic: bool,
}

#[derive(Debug)]
pub enum ExternKind {
    C,
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub name: Identifier<'a>,
    pub initializer: Option<&'a Expression<'a>>,
    pub ty: TypeIdCell,
}

#[derive(Debug)]
pub struct FunctionParameter<'ast> {
    pub name: Identifier<'ast>,
    pub ty: TypeIdCell,
    pub id: SymId
}

pub struct DisplayFunctionParameter<'ast, 'types>(
    pub &'ast FunctionParameter<'ast>,
    pub &'types TypeCtx<'types>,
);

impl<'ast, 'types> Display for DisplayFunctionParameter<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self(param, types) = self;
        write!(
            f,
            "{}({}): {}",
            param.name.name,
            param.id,
            DisplayType(param.ty.clone().inner(), types)
        )
    }
}
