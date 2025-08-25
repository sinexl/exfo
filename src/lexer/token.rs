use crate::common::SourceLocation;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,

    pub integer: i32,
    pub double: f32,
    pub string: Rc<str>,

    pub loc: SourceLocation,
}

impl Token {
    pub fn integer(value: i32, loc: SourceLocation) -> Self {
        Self {
            kind: TokenType::Integer,
            loc,
            integer: value,
            double: 0f32,
            string: Default::default(),
        }
    }

    pub fn double(value: f32, loc: SourceLocation) -> Self {
        Token {
            kind: TokenType::Double,
            loc,
            integer: 0,
            double: value,
            string: Default::default(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TokenType {
    EOF,

    // Keywords
    Func,

    // Punctuation
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Dot,
    Semicolon,

    // Logical operators
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Other operators
    Plus,
    Minus,
    Star,
    Slash,

    // Other terminals
    Identifier,
    Integer,
    Double,
    String,
}

impl TokenType {
    pub fn from_punct(p: &str) -> Self {
        PUNCTS.with(|c| c[&p])
    }
}

thread_local! {
pub static PUNCTS: HashMap<&'static str, TokenType> = HashMap::from([
    ("(", TokenType::OpenParen),
    (")", TokenType::CloseParen),
    ("{", TokenType::OpenBrace),
    ("}", TokenType::CloseBrace),
]);
}
