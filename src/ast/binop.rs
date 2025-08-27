use crate::lexer::token::TokenType;

#[derive(Hash)]
pub(crate) enum BinopKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl BinopKind {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            BinopKind::Addition => "Addition",
            BinopKind::Subtraction => "Subtraction",
            BinopKind::Multiplication => "Multiplication",
            BinopKind::Division => "Division",
        }
    }
}

impl BinopKind {
    fn from_operator(token: TokenType) -> Option<Self> {
        use crate::lexer::token::TokenType::*;
        use BinopKind::*;
        match token {
            Plus => Some(Addition),
            Minus => Some(Subtraction),
            Star => Some(Multiplication),
            Slash => Some(Division),
            _ => None,
        }
    }
}
