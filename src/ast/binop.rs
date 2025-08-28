use crate::lexer::token::TokenType;
use crate::lexer::token::TokenType::{Minus, Plus, Slash, Star};

#[derive(Hash, Eq, PartialEq, Debug)]
pub(crate) enum BinopKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

/// higher index = higher precedence
pub const PRECEDENCE: &[&[BinopKind]] = &[
    &[BinopKind::Addition, BinopKind::Subtraction],
    &[BinopKind::Multiplication, BinopKind::Division],

];

pub const MAX_PRECEDENCE: i32 = PRECEDENCE.len() as i32;

impl BinopKind {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            BinopKind::Addition => "Addition",
            BinopKind::Subtraction => "Subtraction",
            BinopKind::Multiplication => "Multiplication",
            BinopKind::Division => "Division",
        }
    }
    pub fn from_operator(token: TokenType) -> Option<Self> {
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

    pub fn precedence(&self) -> i32 {
        // level = precedence level
        // group = operations with the same precedence
        for (level, group) in PRECEDENCE.iter().enumerate() {
            for binop in *group {
                if *self == *binop {
                    return level as i32;
                }
            }
        }
        unreachable!()
    }

}
