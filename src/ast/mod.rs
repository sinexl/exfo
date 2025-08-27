pub mod binop;
pub mod printer;

use crate::ast::binop::BinopKind;
use crate::common::SourceLocation;

pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub loc: SourceLocation,
}

pub trait ExpressionVisitor<R> {
    fn visit(&mut self, expression: &Expression) -> R;
}

impl Expression<'_> {
    pub fn accept<R>(&self, mut visitor: impl ExpressionVisitor<R>) -> R {
        visitor.visit(self)
    }
}

pub enum ExpressionKind<'a> {
    Binop {
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        kind: BinopKind,
    },
    Unary {
        item: &'a Expression<'a>,
        operator: UnaryKind,
    },
    Grouping(&'a Expression<'a>),
    Literal(f32),
}


#[derive(Hash)]
pub enum UnaryKind {
    Negation,
}

impl UnaryKind {
    pub(crate) fn name(&self) -> &'static str {
        match self { 
            UnaryKind::Negation => "Negation",
        }
    }
}
