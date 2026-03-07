use crate::common::SourceLocation;
use crate::lexing::token::TokenType::{Bang, Equal};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub kind: TokenType,

    pub integer: i64,
    pub double: f64,
    pub string: Rc<str>,

    pub loc: SourceLocation,
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        const EPSILON: f64 = 1e-6;

        self.kind == other.kind
            && self.integer == other.integer
            && (self.double - other.double).abs() < EPSILON
            && self.string == other.string
            && self.loc == other.loc
    }
}

impl Eq for Token {}

impl Token {
    pub fn integer(value: i64, loc: SourceLocation) -> Self {
        Self {
            kind: Integer,
            loc,
            integer: value,
            double: 0f64,
            string: Default::default(),
        }
    }

    pub fn double(value: f64, loc: SourceLocation) -> Self {
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
            double: 0f64,
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


macro_rules! token_type {
    ($name:ident
        { $($variant:ident => $str:expr),* $(,)?}
    ) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub(crate) enum $name {
            $($variant),*
        }

        impl $name {
            pub fn humanize(self) -> &'static str {
                match self {
                    $($variant => $str),*,
                }
            }
        }
    };
}
token_type! {
    TokenType {
        Eof => "end of file",

        // Keywords
        Func => "`func` keyword",
        Extern => "`extern` keyword",
        Return => "`return` keyword",
        True => "`true` keyword",
        False => "`false` keyword",
        If => "`if` keyword",
        Else => "`else` keyword",
        While => "`while` keyword",
        Break => "`break` keyword",
        Continue => "`continue` keyword",

        // Punctuation
        OpenParen => "opening parenthesis",
        CloseParen => "closing parenthesis",
        OpenBrace => "opening brace",
        CloseBrace => "closing brace",
        Dot => "`.`",
        Comma => "`,`",
        Semicolon => "semicolon",
        Colon => "colon",
        TripleDot => "`...`",

        // Ordering operators
        Equal => "`=`",
        EqualEqual => "`==`",
        Bang => "`!`",
        BangEqual => "`!=`",
        Less => "`<`",
        LessEqual => "`<=`",
        Greater => "`>`",
        GreaterEqual => "`>=`",

        // Logical Operators
        DoubleAmpersand => "`&&`",
        DoubleBar => "`||`",

        // Binary Operators
        Ampersand => "`&`",
        Bar => "`|`",

        // Other operators
        Plus => "`+`",
        Minus => "`-`",
        Star => "`*`",
        Slash => "`/`",

        // Other terminals
        Id => "identifier",
        Integer => "integer literal",
        Double => "double literal",
        String => "string literal",
    }
}
impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.humanize())
    }
}

impl TokenType {
    #[allow(dead_code)]
    pub fn from_punct(p: &str) -> Self {
        PUNCTS.with(|c| c[&p])
    }

    pub fn is_statement_beginning(&self) -> bool {
        match self {
            Func | Extern | Return | If | Else | While | Break | Continue => true,

            Eof | True | False | OpenParen | CloseParen | OpenBrace | CloseBrace | Dot | Comma
            | Semicolon | Colon | Equal | EqualEqual | Bang | BangEqual | Less | LessEqual
            | Greater | GreaterEqual | Plus | Minus | Star | Slash | Id | Integer | Double
            | String | DoubleAmpersand | DoubleBar | Ampersand | Bar => false,
            TripleDot => false,

        }
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
        ("...", TripleDot),
        (";", Semicolon),
        (":", Colon),
        ("+", Plus),
        ("-", Minus),
        ("*", Star),
        ("/", Slash),
        (">", Greater), (">=", GreaterEqual),
        ("<", Less), ("<=", LessEqual),
        ("=", Equal), ("==", EqualEqual),
        ("!", Bang), ("!=", BangEqual),
        ("||", DoubleBar), ("&&", DoubleAmpersand)
    ]);

    pub static SINGLE_PUNCTS: HashMap<char, TokenType> = HashMap::from([
        ('(', OpenParen),
        (')', CloseParen),
        ('{', OpenBrace),
        ('}', CloseBrace),
        ('.', Dot),
        (',', Comma),
        (';', Semicolon),
        (':', Colon),
        ('+', Plus),
        ('-', Minus),
        ('*', Star),
        ('/', Slash),
        ('>', Greater),
        ('<', Less),
        ('=', Equal),
        ('!', Bang),
        ('|', Bar),
        ('&', Ampersand),
    ]);
}
pub fn is_punct(p: char) -> bool {
    SINGLE_PUNCTS.with(|c| c.contains_key(&p))
}
