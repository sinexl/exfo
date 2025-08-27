use crate::ast::binop::BinopKind;
use crate::ast::binop::BinopKind::{Addition, Division, Multiplication, Subtraction};
use crate::ast::Expression;
use crate::ast::ExpressionKind::{Binop, Literal};
use crate::common::SourceLocation;
use crate::lexer::token::TokenType::{Minus, Plus, Slash, Star};
use crate::lexer::token::{Token, TokenType};
use crate::parser::ParserErrorKind::AtEof;
use bumpalo::Bump;
use std::rc::Rc;

/// Operator Precedence
/// Factor: Multiplication / Division
/// Term:   Addition / Subtraction
///
/// Comparison (>=, >, <, <=)
/// Equality (==, !=)

// Parser
pub struct Parser<'a> {
    current: usize,
    tokens: Rc<[Token]>,
    bump: &'a Bump,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Rc<[Token]>, bump: &'a Bump) -> Parser<'a> {
        Self {
            current: 0,
            bump,
            tokens,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        let mut left = self.parse_factor()?;
        while let Some(tk) = self.consume(&[Star, Slash]) {
            let right = self.parse_factor()?;
            let new_kind = match  tk.kind {
                Star => Multiplication,
                Slash => Division,
                _ => unreachable!()
            };
            left = self.reconstruct_binop(left, right, new_kind, tk.loc.clone());
        }
        Ok(left)
    }

    pub fn parse_factor(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        let mut left = self.parse_primary()?;
        while let Some(tk) = self.consume(&[Plus, Minus]) {
            let right = self.parse_primary()?;
            let new_kind = match tk.kind {
                Plus => Addition,
                Minus => Subtraction,
                _ => unreachable!(),
            };
            left = self.reconstruct_binop(left, right, new_kind, tk.loc.clone());
        }
        Ok(left)
    }

    pub fn parse_primary(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        use TokenType::*;
        let token = self.peek_token()?;
        let loc = token.loc;
        let kind = match token.kind {
            Integer => Literal(token.integer as f32),
            Double => Literal(token.double),
            _ => todo!(),
        };
        self.skip_token();
        Ok(self.bump.alloc(Expression { loc, kind }))
    }

    pub fn reconstruct_binop(
        &self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        kind: BinopKind,
        loc: SourceLocation,
    ) -> &'a Expression<'a> {
        self.bump.alloc(Expression {
            kind: Binop { left, right, kind },
            loc,
        })
    }
}

impl<'a> Parser<'a> {
    fn next_token(&mut self) -> Result<Token, ParseError> {
        let tk = self
            .tokens
            .get(self.current)
            .cloned()
            .ok_or_else(|| ParseError {
                location: self.tokens.last().unwrap().loc.clone(),
                kind: AtEof,
            })?;
        self.current += 1;
        Ok(tk)
    }
    pub fn skip_token(&mut self) {
        let _ = self.next_token();
    }
    pub fn peek_token(&self) -> Result<Token, ParseError> {
        let r = self.tokens.get(self.current);
        if let Some(token) = r {
            return Ok(token.clone());
        }

        Err(ParseError {
            location: self.tokens.last().unwrap().loc.clone(),
            kind: AtEof,
        })
    }

    // Peeks the token.
    // If token type is either of `expected`, consumes and returns the token.
    // Otherwise, returns None
    pub fn consume(&mut self, expected: &[TokenType]) -> Option<Token> {
        let actual = self.peek_token().ok()?;
        for t in expected {
            if actual.kind == *t {
                self.skip_token();
                return Some(actual.clone());
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct ParseError {
    location: SourceLocation,
    kind: ParserErrorKind,
}

#[derive(Debug)]
pub enum ParserErrorKind {
    AtEof,
}
