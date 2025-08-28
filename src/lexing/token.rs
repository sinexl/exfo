use crate::common::SourceLocation;
use crate::lexing::token::TokenType::{Bang, Equal};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub kind: TokenType,

    pub integer: i32,
    pub double: f32,
    pub string: Rc<str>,

    pub loc: SourceLocation,
}

impl Token {
    pub fn integer(value: i32, loc: SourceLocation) -> Self {
        Self {
            kind: Integer,
            loc,
            integer: value,
            double: 0f32,
            string: Default::default(),
        }
    }

    pub fn double(value: f32, loc: SourceLocation) -> Self {
        Token {
            kind: Double,
            loc,
            integer: 0,
            double: value,
            string: Default::default(),
        }
    }

    pub fn eof(loc: SourceLocation) -> Self {
        Token {
            kind: Eof,
            integer: 0,
            double: 0f32,
            string: Default::default(),
            loc,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{loc}: {kind:?}", loc = self.loc, kind = self.kind)?;
        match self.kind {
            String => write!(f, " \"{}\"", self.string),
            Integer => write!(f, " ({})", self.integer),
            Double => write!(f, " ({})", self.double),
            Id => write!(f, " `{}`", self.string),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum TokenType {
    Eof,

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
    Id,
    Integer,
    Double,
    String,
}

impl TokenType {
    pub fn from_punct(p: &str) -> Self {
        PUNCTS.with(|c| c[&p])
    }
}

use TokenType::*;
thread_local! {
    pub static PUNCTS: HashMap<&'static str, TokenType> = HashMap::from([
        ("(", OpenParen),
        (")", CloseParen),
        ("{", OpenBrace),
        ("}", CloseBrace),
        (".", Dot),
        (";", Semicolon),
        ("+", Plus),
        ("-", Minus),
        ("*", Star),
        ("/", Slash),
        (">", Greater), (">=", GreaterEqual),
        ("<", Less), ("<=", LessEqual),
        ("=", Equal), ("==", EqualEqual),
        ("!", Bang), ("!=", BangEqual),
    ]);

    pub static SINGLE_PUNCTS: HashMap<char, TokenType> = HashMap::from([
        ('(', OpenParen),
        (')', CloseParen),
        ('{', OpenBrace),
        ('}', CloseBrace),
        ('.', Dot),
        (';', Semicolon),
        ('+', Plus),
        ('-', Minus),
        ('*', Star),
        ('/', Slash),
        ('>', Greater),
        ('<', Less),
        ('=', Equal),
        ('!', Bang),
    ]);
}
pub fn is_punct(p: char) -> bool {
    SINGLE_PUNCTS.with(|c| c.contains_key(&p))
}
