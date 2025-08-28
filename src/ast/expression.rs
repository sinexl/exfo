use crate::ast::binop::BinopKind;
use crate::common::{Identifier, SourceLocation};

#[derive(Debug)]
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

#[derive(Debug)]
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
    Assignment { target: &'a Expression<'a>, value: &'a Expression<'a> },
    Literal(f32),
    VariableAccess(Identifier<'a>),
}

impl ExpressionKind<'_> {
    pub fn is_assignable(&self) -> bool {
        match self {
            ExpressionKind::Assignment { .. } => true,
            ExpressionKind::VariableAccess(_) => true,
            ExpressionKind::Binop { .. } => false,
            ExpressionKind::Unary { .. } => false,
            ExpressionKind::Literal(_) => false,
        }
    }
    pub fn humanize(&self) -> String {
        match self {
            ExpressionKind::Binop { .. } => "binary operation".to_string(),
            ExpressionKind::Unary { .. } => "unary operation".to_string(),
            ExpressionKind::Assignment { .. } => "assignment".to_string(),
            ExpressionKind::Literal(_) => "literal".to_string(),
            ExpressionKind::VariableAccess(_) => "variable access".to_string(),
        }
    }
}


#[derive(Hash, Debug)]
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
