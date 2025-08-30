use crate::ast::binop;
use crate::ast::binop::BinopKind;
use crate::ast::expression::ExpressionKind::{
    Assignment, Binop, FunctionCall, Literal, VariableAccess,
};
use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{CompilerError, Identifier, SourceLocation};
use crate::lexing::token::TokenType::CloseParen;
use crate::lexing::token::{Token, TokenType};
use crate::parsing::parser::ParserErrorKind::{InvalidAssignment, UnexpectedToken};
use bumpalo::Bump;
use std::rc::Rc;
/* Grammar:
 program             => statement* EOF ;
 statement           => expressionStatement;
 expressionStatement => expression ";" ;
 expression          => assignment;
 assignment          => IDENTIFIER "=" assignment | binop;
 binop*              => ** | unary;
 unary               => "-" unary | functionCall;
 functionCall        => primary ( "( args ")" )*
 primary             => NUMBER | STRING | IDENTIFIER | "(" expression ")" ;


 * - For Binary Operator Precedence, visit ./src/ast/binop.rs
 ** - All binary operations follow the same pattern:
         left ( (<either of operators>) right )*
*/

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
    pub fn parse_program(&mut self) -> (&'a [&'a Statement], Box<[ParseError]>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while !self.at_eof() {
            match self.parse_statement() {
                Err(error) => errors.push(error),
                Ok(statement) => statements.push(statement),
            }
        }

        (
            self.bump.alloc_slice_copy(&statements),
            errors.into_boxed_slice(),
        )
    }

    pub fn parse_statement(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        self.parse_expression_statement()
    }

    fn parse_expression_statement(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        let loc = self.peek_token()?.loc;
        let expr = self.parse_expression()?;
        self.expect(&[TokenType::Semicolon], "Expected ';' after expression")?;
        Ok(self.bump.alloc(Statement {
            kind: StatementKind::ExpressionStatement(expr),
            loc,
        }))
    }

    pub fn parse_expression(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        let target = self.parse_binop(0)?;
        if let Some(tk) = self.consume(&[TokenType::Equal]) {
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
            return self.parse_unary();
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

    fn parse_unary(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        if let Some(tok) = self.consume(&[TokenType::Minus]) {
            let item = self.parse_unary()?;

            return Ok(self.bump.alloc(Expression {
                kind: ExpressionKind::Unary {
                    item,
                    operator: UnaryKind::Negation,
                },
                loc: tok.loc,
            }));
        }
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        let mut left = self.parse_primary()?;
        while let Some(tk) = self.consume(&[TokenType::OpenParen]) {
            left = self.bump.alloc(Expression {
                kind: FunctionCall {
                    callee: left,
                    arguments: self.parse_args()?,
                },
                loc: tk.clone().loc,
            });
            if self.consume(&[CloseParen]).is_none() {
                return Err(ParseError {
                    location: tk.loc,
                    kind: ParserErrorKind::UnbalancedParens,
                });
            }
        }

        Ok(left)
    }

    fn parse_args(&mut self) -> Result<&'a [&'a Expression<'a>], ParseError> {
        let mut args: Vec<&'a Expression<'a>> = vec![];
        if self.peek_token()?.kind != TokenType::CloseParen {
            let expr = self.parse_expression()?;
            args.push(expr);

            while self.consume(&[TokenType::Comma]).is_some() {
                args.push(self.parse_expression()?);
            }
        }

        Ok(self.bump.alloc_slice_copy(args.as_slice()))
    }

    fn parse_primary(&mut self) -> Result<&'a Expression<'a>, ParseError> {
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
            return Ok(expr);
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

// Helper methods.
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

    pub fn save_state(&self) -> usize {
        self.current
    }

    pub fn restore_state(&mut self, state: usize) {
        self.current = state;
    }

    /// Peeks the token.
    /// If token type is either of `expected`, consumes and returns the token.
    /// Otherwise, returns None
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

    pub fn expect(&mut self, expected: &[TokenType], message: &str) -> Result<Token, ParseError> {
        if let Some(tk) = self.consume(expected) {
            return Ok(tk);
        }
        let last_token_in_input = self.tokens.last().unwrap().clone(); // TODO: Ensure that input is not empty preemptively 
        let token = self.next_token().unwrap_or(last_token_in_input.clone());
        Err(ParseError {
            location: self.peek_token().unwrap_or(last_token_in_input).loc,
            kind: UnexpectedToken {
                expected: Some(expected.to_vec().into_boxed_slice()),
                got: token,
                message: message.to_string(),
            },
        })
    }

    fn at_eof(&self) -> bool {
        if let Some(token) = self.tokens.get(self.current) {
            token.kind == TokenType::Eof
        } else {
            true
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    pub location: SourceLocation,
    pub kind: ParserErrorKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParserErrorKind {
    AtEof,
    UnbalancedParens,
    InvalidAssignment(String),
    UnexpectedToken {
        expected: Option<Box<[TokenType]>>,
        got: Token,
        message: String,
    },
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
            UnexpectedToken {
                expected,
                got,
                message: msg,
            } => {
                msg.clone()
                // TODO: Construct better message
            }
        }
    }

    fn note(&self) -> Option<String> {
        use ParserErrorKind::*;
        match &self.kind {
            AtEof => None,
            UnbalancedParens => Some(format!("last parenthesis located here: {}", self.location)),
            InvalidAssignment(_) => None,
            UnexpectedToken { .. } => None, // TODO: construct note
        }
    }
}
