use std::cell::Cell;
use crate::analysis::r#type::TypeIdCell;
use crate::ast::binop::BinopKind;
use crate::common::{Identifier, SourceLocation};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Eq, PartialEq)]
pub struct Expression<'ast> {
    pub kind: ExpressionKind<'ast>,
    pub loc: SourceLocation,
    pub id: usize,
    pub ty: TypeIdCell,
}

impl<'a> Hash for Expression<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}


#[derive(Debug, Eq, PartialEq, Copy, Clone)]
// Symbol ID.
// Each symbol (i.e function declaration, variable declaration, etc)
// has a unique ID which is assigned by parser.
pub struct SymId(pub usize);
impl Display for SymId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

// Reference ID
// Encapsulates an internally mutable SymbolId, which initially is unknown (None).
// On later compiler passes, Resolver assigns such id for each variable.
#[derive(Debug, Eq, PartialEq)]
pub struct RefId {
    inner: Cell<Option<SymId>>,
}

impl RefId {
    pub fn unknown() -> Self {
        Self {
            inner: Cell::new(None),
        }
    }

    pub fn from_id(id: SymId) -> Self {
        Self {
            inner: Cell::new(Some(id)),
        }
    }
}

impl Display for RefId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.inner.get() {
            Some(id) => write!(f, "{}", id),
            None => write!(f, "#?"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionKind<'ast> {
    Binop {
        left: &'ast mut Expression<'ast>,
        right: &'ast mut Expression<'ast>,
        kind: BinopKind,
    },
    Unary {
        item: &'ast mut Expression<'ast>,
        operator: UnaryKind,
    },
    Assignment {
        target: &'ast mut Expression<'ast>,
        value: &'ast mut Expression<'ast>,
    },
    Literal(AstLiteral<'ast>),
    VariableAccess(Identifier<'ast>, RefId),
    FunctionCall {
        callee: &'ast mut Expression<'ast>,
        arguments: &'ast [*mut Expression<'ast>],
    },
}

impl ExpressionKind<'_> {
    pub fn lvalue(&self) -> bool {
        match self {
            ExpressionKind::Assignment { .. } => true,
            ExpressionKind::VariableAccess(_, ..) => true,
            ExpressionKind::Binop { .. } => false,
            ExpressionKind::Unary { operator, ..} =>
                match operator {
                    UnaryKind::Negation | UnaryKind::AddressOf => false,
                    UnaryKind::Dereferencing => true,
                },
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
            ExpressionKind::VariableAccess(_, ..) => "variable access".to_string(),
            ExpressionKind::FunctionCall { .. } => "function call".to_string(),
        }
    }
}

#[derive(Hash, Debug, Eq, PartialEq)]
pub enum UnaryKind {
    Negation,
    Dereferencing,
    AddressOf,
}

impl UnaryKind {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            UnaryKind::Negation => "Negation",
            UnaryKind::Dereferencing => "Dereferencing",
            UnaryKind::AddressOf => "AddressOf"
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AstLiteral<'a> {
    Integral(i64),
    FloatingPoint(f64),
    Boolean(bool),
    String(&'a str),
}
impl Eq for AstLiteral<'_> {}

impl Hash for AstLiteral<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            AstLiteral::Integral(i) => i.hash(state),
            //  NOTE. It's unclear how to approach this.
            // I guess, we will only care for structural identity, not mathematical one.
            // So -0.0 and 0.0 will be considered "distinct", and NaN will be equal to NaN
            AstLiteral::FloatingPoint(f) => f.to_bits().hash(state),
            AstLiteral::Boolean(b) => b.hash(state),
            AstLiteral::String(s) => s.hash(state),
        }
    }
}

impl<'a> Display for AstLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            AstLiteral::Integral(i) => write!(f, "{}", i),
            AstLiteral::FloatingPoint(fl) => write!(f, "{}", fl),
            AstLiteral::Boolean(b) => write!(f, "{}", b),
            AstLiteral::String(s) => write!(f, "{}", s),
        }
    }
}
