use crate::ast::binop::BinopKind;
use crate::ast::ExpressionKind::{Assignment, Binop, Grouping, Literal, VariableAccess};
use crate::ast::{binop, Expression};
use crate::common::{CompilerError, Identifier, SourceLocation};
use crate::lexer::token::TokenType::{Eof, Equal};
use crate::lexer::token::{Token, TokenType};
use crate::parser::ParserErrorKind::InvalidAssignment;
use bumpalo::Bump;
use std::rc::Rc;
// Operator Precedence
// Factor: Multiplication / Division
// Term:   Addition / Subtraction
//
// Comparison (>=, >, <, <=)
// Equality (==, !=)

/// Parser
#[derive(Debug)]
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
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        let target = self.parse_binop(0)?;
        if let Some(tk) = self.consume(&[Equal]) {
            let value = self.parse_assignment()?;
            if !target.kind.is_assignable() {
                return Err(ParseError {
                    location: target.loc.clone(),
                    kind: InvalidAssignment(target.kind.humanize()),
                });
            }

            return Ok(self.bump.alloc(Expression {
                loc: tk.loc,
                kind: Assignment { target, value },
            }));
        }
        Ok(target)
    }

    pub(crate) fn parse_binop(
        &mut self,
        precedence: i32,
    ) -> Result<&'a Expression<'a>, ParseError> {
        if precedence >= binop::MAX_PRECEDENCE {
            return self.parse_primary();
        }
        let mut left = self.parse_binop(precedence + 1)?;
        let mut operator = self.peek_token()?;
        if BinopKind::from_operator(operator.kind)
            .filter(|c| c.precedence() == precedence)
            .is_some()
        {
            'same_precedence: loop {
                let current = self.peek_token()?;
                if let Some(kind) = BinopKind::from_operator(current.kind) {
                    if kind.precedence() != precedence {
                        break 'same_precedence;
                    }
                    operator = self.next_token()?;
                    let right = self.parse_binop(precedence + 1)?;
                    left = self.reconstruct_binop(left, right, kind, operator.loc.clone())
                } else {
                    break 'same_precedence;
                }
            }
        }
        Ok(left)
    }

    pub fn parse_primary(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        use TokenType::*;
        let token = self.peek_token()?;

        if let Some(id) = self.consume(&[Id]) {
            let loc = id.loc.clone();
            return Ok(self.bump.alloc(Expression {
                loc,
                kind: VariableAccess(Identifier::from_token(id, self.bump)),
            }));
        }
        if let Some(integer) = self.consume(&[Integer]) {
            return Ok(self.construct_literal(integer.integer as f32, integer.loc.clone()));
        }
        if let Some(double) = self.consume(&[Double]) {
            return Ok(self.construct_literal(double.double, double.loc.clone()));
        }
        if let Some(paren) = self.consume(&[OpenParen]) {
            let parenthesis_loc = paren.loc;
            let expr = self.parse_expression()?;
            if self.consume(&[CloseParen]).is_none() {
                return Err(ParseError {
                    location: parenthesis_loc,
                    kind: ParserErrorKind::UnbalancedParens,
                });
            }
            let kind = Grouping(expr);
            let loc = parenthesis_loc;
            return Ok(self.bump.alloc(Expression { loc, kind }));
        }

        panic!("unknown token {}", token);
    }

    pub fn construct_literal(&self, value: f32, loc: SourceLocation) -> &'a Expression<'a> {
        self.bump.alloc(Expression {
            loc,
            kind: Literal(value),
        })
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

impl Parser<'_> {
    fn next_token(&mut self) -> Result<Token, ParseError> {
        let tk = self
            .tokens
            .get(self.current)
            .cloned()
            .ok_or_else(|| ParseError {
                location: self.tokens.last().unwrap().loc.clone(),
                kind: ParserErrorKind::AtEof,
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
            kind: ParserErrorKind::AtEof,
        })
    }

    pub fn peek_next(&self) -> Option<Token> {
        self.tokens.get(self.current + 1).cloned()
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

    fn at_eof(&self) -> bool {
        if let Some(token) = self.tokens.get(self.current) {
            token.kind == Eof
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    location: SourceLocation,
    kind: ParserErrorKind,
}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    AtEof,
    UnbalancedParens,
    InvalidAssignment(String),
}

impl CompilerError for ParseError {
    fn location(&self) -> SourceLocation {
        self.location.clone()
    }

    fn message(&self) -> String {
        use ParserErrorKind::*;
        match &self.kind {
            AtEof => "Nothing to parse".to_string(),
            UnbalancedParens => "unbalanced parentheses".to_string(),
            InvalidAssignment(name) => format!("could not assign to {}", name),
        }
    }

    fn note(&self) -> Option<String> {
        use ParserErrorKind::*;
        match &self.kind {
            AtEof => None,
            UnbalancedParens => Some(format!("last parenthesis located here: {}", self.location)),
            InvalidAssignment(_) => todo!(),
        }
    }
}
