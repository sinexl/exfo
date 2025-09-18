use crate::analysis::r#type::Type;
use crate::ast::binop::BinopKind;
use crate::common::{Identifier, SourceLocation};
use std::cell::Cell;
use std::fmt::Display;
use std::hash::{Hash, Hasher};

#[derive(Debug, Eq, PartialEq)]
pub struct Expression<'ast> {
    pub kind: ExpressionKind<'ast>,
    pub loc: SourceLocation,
    pub id: usize,
    pub ty: Cell<Type<'ast>>,
}

impl<'a> Hash for Expression<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ExpressionKind<'ast> {
    Binop {
        left: &'ast Expression<'ast>,
        right: &'ast Expression<'ast>,
        kind: BinopKind,
    },
    Unary {
        item: &'ast Expression<'ast>,
        operator: UnaryKind,
    },
    Assignment {
        target: &'ast Expression<'ast>,
        value: &'ast Expression<'ast>,
    },
    Literal(AstLiteral),
    VariableAccess(Identifier<'ast>),
    FunctionCall {
        callee: &'ast Expression<'ast>,
        arguments: &'ast [&'ast Expression<'ast>],
    },
}

impl ExpressionKind<'_> {
    pub fn is_assignable(&self) -> bool {
        match self {
            ExpressionKind::Assignment { .. } => true,
            ExpressionKind::VariableAccess(_) => true,
            ExpressionKind::Binop { .. } => false,
            ExpressionKind::Unary { .. } => false,
            ExpressionKind::Literal(_) => false,
            ExpressionKind::FunctionCall { .. } => false,
        }
    }
    pub fn humanize(&self) -> String {
        match self {
            ExpressionKind::Binop { .. } => "binary operation".to_string(),
            ExpressionKind::Unary { .. } => "unary operation".to_string(),
            ExpressionKind::Assignment { .. } => "assignment".to_string(),
            ExpressionKind::Literal(_) => "literal".to_string(),
            ExpressionKind::VariableAccess(_) => "variable access".to_string(),
            ExpressionKind::FunctionCall { .. } => "function call".to_string(),
        }
    }
}

#[derive(Hash, Debug, Eq, PartialEq)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AstLiteral {
    Integral(i64),
    FloatingPoint(f64),
}
impl Eq for AstLiteral {}

impl Hash for AstLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            AstLiteral::Integral(i) => i.hash(state),
            //  NOTE. It's unclear how to approach this.
            // I guess, we will only care for structural identity, not mathematical one.
            // So -0.0 and 0.0 will be considered "distinct", and NaN will be equal to NaN
            AstLiteral::FloatingPoint(f) => f.to_bits().hash(state),
        }
    }
}

impl Display for AstLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstLiteral::Integral(i) => write!(f, "{}", i),
            AstLiteral::FloatingPoint(fl) => write!(f, "{}", fl),
        }
    }
}
