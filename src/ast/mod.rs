use crate::common::SourceLocation;

pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub loc: SourceLocation,
}

pub trait ExpressionVisitor<R>
{
    fn visit(&mut self, expression: &Expression) -> R;
}

impl Expression<'_> {
    pub fn accept<R>(&self, mut visitor: impl ExpressionVisitor<R>) -> R {
        visitor.visit(self)
    }
}

pub enum ExpressionKind<'a> {
    Binop(Binop<'a>),
    Unary{item: &'a Expression<'a>, operator: UnaryKind},
    Grouping(&'a Expression<'a>),
    Literal(f32),
}

pub struct Binop<'a> {
    pub left: &'a Expression<'a>,
    pub right: &'a Expression<'a>,
    pub kind: BinopKind,
}
#[derive(Hash)]
pub enum BinopKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Hash)]
pub enum UnaryKind {
    Negation,
}