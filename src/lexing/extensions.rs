use crate::lexing::token::TokenType;

pub trait OptionCharExtensions {
    fn is(&self, c: char) -> Option<char>;
    fn is_not(&self, c: char) -> Option<char>;
}

impl OptionCharExtensions for Option<char> {
    fn is(&self, c: char) -> Option<char> {
        self.filter(|&ch| ch == c)
    }
    fn is_not(&self, c: char) -> Option<char> {
        self.filter(|&ch| ch != c)
    }
}

pub trait CharExtensions {
    fn is_id_start(&self) -> bool;
    fn is_id_continue(&self) -> bool;
}

impl CharExtensions for char {
    fn is_id_start(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }

    fn is_id_continue(&self) -> bool {
        self.is_alphanumeric() || *self == '_'
    }
}

pub trait StringExtensions {
    fn to_token_type(&self) -> TokenType;
}

impl StringExtensions for str {
    fn to_token_type(&self) -> TokenType {
        use crate::lexing::token::TokenType::*;
        match self {
            "func" => Func,
            "extern" => Extern,
            "return" => Return,
            "true" => True,
            "false" => False,
            "if" => If,
            "else" => Else,
            "while" => While,
            "break" => Break,
            "continue" => Continue,
            _ => Id,
        }
    }
}
