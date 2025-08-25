mod token;
mod extensions;

use crate::common::SourceLocation;
use std::rc::Rc;
use std::str::FromStr;
use crate::lexer::extensions::{CharExtensions, OptionCharExtensions, StringExtensions};
use crate::lexer::token::{Token, TokenType, PUNCTS};

pub struct Lexer {
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

    pub fn next_token(&mut self) -> Option<Token> {
        use TokenType::*;
        self.skip_whitespaces_and_comments();
        if self.is_eof() {
            return None;
        }
        let state = self.save();
        let c = self.next_char()?;
        let tk = match c {
            '(' => self.create_punct(OpenParen),
            ')' => self.create_punct(CloseParen),
            '{' => self.create_punct(OpenBrace),
            '}' => self.create_punct(CloseBrace),
            ';' => self.create_punct(Semicolon),
            '+' => self.create_punct(Plus),
            '-' => self.create_punct(Minus),
            '*' => self.create_punct(Star),
            '/' => self.create_punct(Slash),
            '.' => self.create_punct(Dot),
            '"' => {
                self.restore(state);
                self.scan_string()
            }
            digit if digit.is_digit(10) => {
                self.restore(state);
                self.scan_number()
            }
            ch if ch.is_id_start() => {
                self.restore(state);
                self.scan_identifier_or_keyword()
            }
            c => {
                eprintln!("Unexpected character {c}");
                panic!();
            }
        };

        Some(tk)
    }

    pub fn skip_whitespaces_and_comments(&mut self) {
        loop {
            if self.peek_char().filter(|c| c.is_whitespace()).is_some() {
                while self.peek_char().filter(|c| c.is_whitespace()).is_some() {
                    self.skip_char();
                }
            } else if self.starts_with("/*") {
                let mut nested = 1;
                while nested > 0 {
                    while !self.starts_with("*/") {
                        if self.starts_with_peek("/*") {
                            nested += 1;
                        }

                        if self.is_eof() {
                            eprintln!("unterminated */"); // todo: proper error handling
                            return;
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
                break;
            }
        }
    }

    pub fn save(&self) -> LexerState {
        self.state
    }
    pub fn restore(&mut self, state: LexerState) {
        self.state = state;
    }

    fn scan_string(&mut self) -> Token {
        assert_eq!(self.peek_char().unwrap(), '"');
        self.skip_char();
        let start = self.idx();

        // TODO: Escape sequences
        while self.peek_char().is_not('"').is_some() {
            self.skip_char();
        }

        if self.is_eof() {
            eprintln!("unterminated string");
            panic!(); // todo: proper error handling.
        }

        self.skip_char(); // "
        let val: Rc<str> = Rc::from(&self.src[start..self.idx() - 1]);

        Token {
            loc: self.token_start.source_loc(self.filepath.clone()),
            kind: TokenType::String,
            integer: 0,
            double: 0f32,
            string: val,
        }
    }

    fn scan_number(&mut self) -> Token {
        let start = self.idx();

        while self.peek_char().filter(|c| c.is_digit(10)).is_some() {
            self.skip_char()
        }
        let mut is_floating = false;

        if self.peek_char().is('.').is_some() && self.peek_next().is_digit(10) {
            is_floating = true;
            self.skip_char(); // skip .
            while self.peek_char().filter(|c| c.is_digit(10)).is_some() {
                self.skip_char();
            }
        }

        let str: Rc<str> = Rc::from(&self.src[start..self.idx()]);

        let location = self.token_start.source_loc(self.filepath.clone());
        if is_floating {
            let value = f32::from_str(&str).unwrap();
            return Token::double(value, location);
        }
        let value = i32::from_str(&str).unwrap();
        Token::integer(value, self.token_start.source_loc(self.filepath.clone()))
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
            double: 0f32,
            string: Default::default(),
            loc: self.token_start.source_loc(self.filepath.clone()),
        }
    }
}

fn is_punct(p: char) -> bool {
    let binding = [p as u8];
    let s = str::from_utf8(&binding).unwrap();
    PUNCTS.with(|c| c.contains_key(s))
}

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
        let current = self.idx();
        if self.is_eof_at(current + s.len()) {
            return false;
        }
        if self.src[current..].starts_with(s) {
            return true;
        }
        false
    }

    pub fn create_punct(&self, kind: TokenType) -> Token {
        Token {
            kind,
            loc: self.token_start.source_loc(self.filepath.clone()),
            integer: 0,
            double: 0f32,
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
            line_number: 0,
        }
    }
}
