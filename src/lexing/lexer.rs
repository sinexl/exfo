use crate::common::{CompilerError, SourceLocation};
use crate::lexing::extensions::{CharExtensions, OptionCharExtensions, StringExtensions};
use crate::lexing::lexer::LexerErrorKind::{
    UnexpectedCharacter, UnterminatedComment, UnterminatedString,
};
use crate::lexing::token::{SINGLE_PUNCTS, Token, TokenType, is_punct};
use std::fmt::{Display, Formatter};
use std::fs;
use std::rc::Rc;
use std::str::FromStr;

pub(crate) struct Lexer {
    filepath: Rc<str>,
    src: Rc<str>,
    state: LexerState,
    token_start: LexerState,
}

impl Lexer {
    pub fn new(src: &str, path: &str) -> Self {
        Lexer {
            filepath: Rc::from(path),
            src: Rc::from(src),
            state: Default::default(),
            token_start: LexerState::default(),
        }
    }
    #[allow(dead_code)]
    pub fn file(path: &str) -> Self {
        let contents = fs::read_to_string(path).expect("Could not read file");
        Self::new(contents.as_str(), path)
    }

    pub fn accumulate(&mut self) -> (Vec<Token>, Vec<LexerError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        loop {
            let x = self.next_token();
            if let Ok(x) = x {
                let kind = x.kind;
                tokens.push(x);
                if kind == TokenType::Eof {
                    break;
                }
            } else if let Err(x) = x {
                errors.push(x);
            }
        }
        (tokens, errors)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        use TokenType::*;

        self.skip_whitespaces_and_comments()?;
        let saved = self.save();
        let c = if let Some(c) = self.next_char() {
            c
        } else {
            return Ok(Token::eof(self.source_loc(self.state)));
        };
        self.token_start = self.state;

        let tk = match c {
            '!' => self.create_punct_with_equal(BangEqual, Bang),
            '=' => self.create_punct_with_equal(EqualEqual, Equal),
            '>' => self.create_punct_with_equal(GreaterEqual, Greater),
            '<' => self.create_punct_with_equal(LessEqual, Less),
            punct if is_punct(punct) => {
                let kind = SINGLE_PUNCTS.with(|c| c[&punct]);
                self.create_punct(kind)
            }

            '"' => {
                self.restore(saved);
                self.scan_string()?
            }
            digit if digit.is_ascii_digit() => {
                self.restore(saved);
                self.scan_number()
            }
            ch if ch.is_id_start() => {
                self.restore(saved);
                self.scan_identifier_or_keyword()
            }
            c => {
                return Err(LexerError {
                    kind: UnexpectedCharacter(c),
                    location: self.source_loc(self.state),
                });
            }
        };

        Ok(tk)
    }

    // this function is mainly needed to counter borrow checker which forbids inlining
    fn create_punct_with_equal(
        &mut self,
        if_has_equal: TokenType,
        if_doesnt_have: TokenType,
    ) -> Token {
        let x = self.match_type('=', if_has_equal, if_doesnt_have);
        self.create_punct(x)
    }

    pub fn skip_whitespaces_and_comments(&mut self) -> Result<(), LexerError> {
        'comments: loop {
            if self.peek_char().filter(|c| c.is_whitespace()).is_some() {
                while self.peek_char().filter(|c| c.is_whitespace()).is_some() {
                    self.skip_char();
                }
            } else if self.starts_with("/*") {
                let loc = self.source_loc(self.state);
                let mut nested = 1;
                while nested > 0 {
                    while !self.starts_with("*/") {
                        if self.starts_with_peek("/*") {
                            nested += 1;
                        }

                        if self.is_eof() {
                            return Err(LexerError::new(loc, UnterminatedComment));
                        }
                        self.skip_char();
                    }
                    nested -= 1;
                }
            } else if self.starts_with("//") {
                while self.peek_char().is_not('\n').is_some() {
                    self.skip_char();
                }
                if !self.is_eof() {
                    self.skip_char()
                }
            } else {
                break 'comments;
            }
        }
        Ok(())
    }

    pub fn save(&self) -> LexerState {
        self.state
    }
    pub fn restore(&mut self, state: LexerState) {
        self.state = state;
    }

    pub fn source_loc(&self, state: LexerState) -> SourceLocation {
        state.source_loc(self.filepath.clone())
    }

    fn scan_string(&mut self) -> Result<Token, LexerError> {
        assert_eq!(self.peek_char().unwrap(), '"');
        let start_loc = self.source_loc(self.state);
        self.skip_char();
        let start = self.idx();

        // TODO: Escape sequences
        while self.peek_char().is_not('"').is_some() {
            self.skip_char();
        }

        if self.is_eof() {
            return Err(LexerError::new(start_loc, UnterminatedString));
        }

        self.skip_char(); // "
        let val: Rc<str> = Rc::from(&self.src[start..self.idx() - 1]);

        Ok(Token {
            loc: self.source_loc(self.token_start),
            kind: TokenType::String,
            integer: 0,
            double: 0f64,
            string: val,
        })
    }

    fn scan_number(&mut self) -> Token {
        let start = self.idx();

        while self.peek_char().filter(|c| c.is_ascii_digit()).is_some() {
            self.skip_char()
        }
        let mut is_floating = false;

        if self.peek_char().is('.').is_some() && self.peek_next().is_ascii_digit() {
            is_floating = true;
            self.skip_char(); // skip .
            while self.peek_char().filter(|c| c.is_ascii_digit()).is_some() {
                self.skip_char();
            }
        }

        let str: Rc<str> = Rc::from(&self.src[start..self.idx()]);

        let location = self.source_loc(self.token_start);
        if is_floating {
            let value = f64::from_str(&str).unwrap();
            return Token::double(value, location);
        }
        let value = i64::from_str(&str).unwrap();
        Token::integer(value, location)
    }

    fn scan_identifier_or_keyword(&mut self) -> Token {
        let start = self.idx();
        while self.peek_char().filter(|c| c.is_id_continue()).is_some() {
            self.skip_char();
        }

        let word: Rc<str> = Rc::from(&self.src[start..self.idx()]);
        let kind = word.to_token_type();
        Token {
            kind,
            integer: 0,
            double: 0f64,
            string: word,
            loc: self.source_loc(self.token_start),
        }
    }
}

// helpers
impl Lexer {
    fn peek_char(&self) -> Option<char> {
        self.src.chars().nth(self.idx()) // nice language you've got here
    }

    fn peek_next(&self) -> char {
        let next = self.idx() + 1;
        if self.is_eof_at(next) {
            return '\0';
        }
        self.src.chars().nth(next).unwrap()
    }

    pub fn is_eof(&self) -> bool {
        self.idx() >= self.src.len()
    }

    pub fn is_eof_at(&self, idx: usize) -> bool {
        idx >= self.src.len()
    }

    fn skip_char(&mut self) {
        let _ = self.next_char().unwrap();
    }

    #[must_use]
    fn next_char(&mut self) -> Option<char> {
        let char = self.src.chars().nth(self.state.current)?;
        if let '\n' = char {
            self.state.line_number += 1;
            self.state.line_start = self.state.current;
        };

        self.state.current += 1;
        Some(char)
    }

    fn idx(&self) -> usize {
        self.state.current
    }

    fn starts_with(&mut self, s: &str) -> bool {
        let res = self.starts_with_peek(s);
        if res {
            self.state.current += s.len();
        }
        res
    }

    pub fn starts_with_peek(&self, s: &str) -> bool {
        assert!(!s.is_empty());
        let current = self.idx();
        if self.is_eof_at(current + s.len() - 1) {
            return false;
        }
        if self.src[current..].starts_with(s) {
            return true;
        }
        false
    }

    /// Peeks a character.
    /// If character == `if_true`, consumes it and returns if_true,
    /// Otherwise, returns `if_false` without consuming
    fn match_type(&mut self, expected: char, if_true: TokenType, if_not: TokenType) -> TokenType {
        let actual = self.peek_char().unwrap();
        if actual != expected {
            return if_not;
        }
        self.skip_char();

        if_true
    }

    fn create_punct(&self, kind: TokenType) -> Token {
        Token {
            kind,
            loc: self.source_loc(self.token_start),
            integer: 0,
            double: 0f64,
            string: Default::default(), // TODO: the character itself
        }
    }
}

#[derive(Copy, Clone)]
pub struct LexerState {
    current: usize,
    line_start: usize,
    line_number: usize,
}

impl LexerState {
    pub(crate) fn source_loc(&self, path: Rc<str>) -> SourceLocation {
        let mut offset = self.current - self.line_start;

        if self.line_number > 1 {
            offset -= 1;
        }
        SourceLocation::new(path, self.line_number, offset)
    }
}

impl Default for LexerState {
    fn default() -> LexerState {
        LexerState {
            current: 0,
            line_start: 0,
            line_number: 1,
        }
    }
}

pub struct LexerError {
    location: SourceLocation,
    kind: LexerErrorKind,
}

impl LexerError {
    pub fn new(location: SourceLocation, kind: LexerErrorKind) -> LexerError {
        LexerError { location, kind }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LexerErrorKind {
    UnterminatedString,
    UnterminatedComment,
    UnexpectedCharacter(char),
}

impl CompilerError for LexerError {
    fn location(&self) -> SourceLocation {
        self.location.clone()
    }

    fn message(&self) -> String {
        use LexerErrorKind::*;
        match self.kind {
            UnterminatedString => "Unterminated string".to_string(),
            UnterminatedComment => "Unterminated comment".to_string(),
            UnexpectedCharacter(ch) => format!("Unexpected character: `{}`", ch),
        }
    }

    fn note(&self) -> Option<String> {
        use LexerErrorKind::*;
        match self.kind {
            UnterminatedString => Some(format!("string literal begins here: {}", self.location)),
            UnterminatedComment => Some(format!("comment begins here: {}", self.location)),
            UnexpectedCharacter(_) => None,
        }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self as &dyn CompilerError, f)
    }
}
