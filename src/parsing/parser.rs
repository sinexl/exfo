use crate::analysis::r#type::Type;
use crate::ast::binop;
use crate::ast::binop::BinopKind;
use crate::ast::expression::ExpressionKind::{
    Assignment, Binop, FunctionCall, Literal, VariableAccess,
};
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    FunctionDeclaration, FunctionParameter, Statement, StatementKind, VariableDeclaration,
};
use crate::common::BumpVec;
use crate::common::{CompilerError, Identifier, SourceLocation};
use crate::lexing::token::{Token, TokenType};
use crate::parsing::parser::ParserErrorKind::{
    InvalidAssignment, UnbalancedParens, UnexpectedToken, UnknownType,
};
use bumpalo::Bump;
use std::cell::Cell;
use std::rc::Rc;
/* Grammar:
 program             => decl* EOF ;
 decl                => funcDecl | varDecl | statement ;
 statement           => expressionStatement | blockStatement ;
 expressionStatement => expression ";" ;
 varDecl             => IDENTIFIER ":" type? ("=" expression)? ";" ;
 funcDecl            => "func" IDENTIFIER function ;
 function            => "(" args ")" blockStatement ;
 blockStatement      => "{" (declaration*)? "}" ;
 expression          => assignment;
 assignment          => IDENTIFIER "=" assignment | binop;
 binop*              => ** | unary;
 unary               => "-" unary | functionCall;
 functionCall        => primary ( "( args ")" )*
 primary             => NUMBER | STRING | IDENTIFIER | "(" expression ")" ;
 type                => IDENTIFIER


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
    nodes_amount: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Rc<[Token]>, bump: &'a Bump) -> Parser<'a> {
        Self {
            current: 0,
            bump,
            tokens,
            nodes_amount: 0,
        }
    }

    pub fn id(&mut self) -> usize {
        let current = self.nodes_amount;
        self.nodes_amount += 1;
        current
    }
}

impl<'a> Parser<'a> {
    pub fn parse_program(&mut self) -> (&'a [&'a Statement<'a>], Box<[ParseError]>) {
        let mut statements = BumpVec::new_in(self.bump);
        let mut errors = Vec::new();
        while !self.at_eof() {
            match self.parse_declaration() {
                Err(error) => errors.push(error),
                Ok(statement) => statements.push(statement),
            }
        }

        (statements.into_bump_slice(), errors.into_boxed_slice())
    }

    pub fn parse_declaration(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        let state = self.save_state();
        if self.consume(&[TokenType::Func]).is_some() {
            self.restore_state(state);
            return self.parse_function_declaration(); // TODO: Sync state of parser on error
        }
        if self.consume(&[TokenType::Id]).is_some() {
            if self.consume(&[TokenType::Colon]).is_some() {
                self.restore_state(state);
                return self.parse_variable_declaration(); // TODO: Sync state of parser on error
            }
            self.restore_state(state);
        }

        self.parse_statement()
    }

    pub fn parse_variable_declaration(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        let name = self.expect(&[TokenType::Id], "Expected variable name")?;
        let colon = self.expect(&[TokenType::Colon], "Expected colon")?;
        let mut initializer: Option<&'a Expression<'a>> = None;
        if self.consume(&[TokenType::Equal]).is_some() {
            initializer = Some(self.parse_expression()?);
        }
        self.expect(
            &[TokenType::Semicolon],
            "Expected semicolon after variable declaration",
        )?;

        Ok(self.bump.alloc(Statement {
            kind: StatementKind::VariableDeclaration(VariableDeclaration {
                name: Identifier::from_token(name, self.bump),
                initializer,
            }),
            loc: colon.loc,
        }))
    }

    /// func name (args) {} ...
    pub fn parse_function_declaration(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        let func_keyword = self.expect(&[TokenType::Func], "Expected function declaration")?;
        let name = self.expect(&[TokenType::Id], "Expected function name")?;
        debug_assert!(name.kind == TokenType::Id);
        let (_, body) = self.parse_function()?;

        Ok(self.bump.alloc(Statement {
            kind: StatementKind::FunctionDeclaration(FunctionDeclaration {
                name: Identifier::from_token(name, self.bump),
                body,
            }),
            loc: func_keyword.loc,
        }))
    }

    /// (args) {}
    pub fn parse_function(
        &mut self,
    ) -> Result<(&'a [FunctionParameter<'a>], &'a [&'a Statement<'a>]), ParseError> {
        let o_paren = self.expect(
            &[TokenType::OpenParen],
            "Expected opening parenthesis in function declaration",
        )?;
        let peek = |this: &mut Self, tk: Token| {
            this.peek_token().map_err(|mut e| {
                e.kind = UnbalancedParens;
                e.location = tk.loc;
                e
            })
        };
        let mut arguments = BumpVec::new_in(&self.bump);
        if peek(self, o_paren.clone())?.kind != TokenType::CloseParen {
            let mut id = self.expect(&[TokenType::Id], "Expected parameter name")?;
            self.expect(&[TokenType::Colon], "Expected colon")?;
            let mut ty = self.parse_type()?;
            arguments.push(FunctionParameter {
                name: Identifier::from_token(id, self.bump),
                ty,
            });
            while self.consume(&[TokenType::Comma]).is_some() {
                id = self.expect(&[TokenType::Id], "Expected parameter name")?;
                self.expect(&[TokenType::Colon], "Expected colon")?;
                ty = self.parse_type()?;
                arguments.push(FunctionParameter {
                    name: Identifier::from_token(id, self.bump),
                    ty,
                });
            }
        }
        self.expect(
            &[TokenType::CloseParen],
            "Expected closing parenthesis after argument list",
        )?;

        let block = self.parse_block_statement()?;
        let StatementKind::Block(statements) = block.kind else {
            unreachable!()
        };
        Ok((arguments.into_bump_slice(), statements))
    }

    pub fn parse_block_statement(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        let mut statements = BumpVec::new_in(self.bump);
        let left_brace = self.expect(
            &[TokenType::OpenBrace],
            "Expected opening brace in block statement",
        )?;

        while !self.at_eof() && self.peek_token()?.kind != TokenType::CloseBrace {
            let statement = self.parse_declaration()?;
            statements.push(statement);
        }

        if self.consume(&[TokenType::CloseBrace]).is_none() {
            return Err(ParseError {
                location: left_brace.clone().loc,
                kind: ParserErrorKind::UnbalancedBraces,
            });
        }

        Ok(self.bump.alloc(Statement {
            kind: StatementKind::Block(statements.into_bump_slice()),
            loc: left_brace.loc,
        }))
    }

    pub fn parse_statement(&mut self) -> Result<&'a Statement<'a>, ParseError> {
        let state = self.save_state();
        if self.consume(&[TokenType::OpenBrace]).is_some() {
            self.restore_state(state);
            return self.parse_block_statement();
        }
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

    pub fn parse_type(&mut self) -> Result<Type<'a>, ParseError> {
        let name = self.expect(&[TokenType::Id], "Expected type name")?;
        if name.string.as_ref() == "int" {
            return Ok(Type::Int64);
        }
        Err(ParseError {
            kind: UnknownType(String::from(name.string.as_ref())),
            location: name.loc.clone(),
        })
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
                id: self.id(),
                kind: Assignment { target, value },
                ty: Cell::new(Type::Unknown),
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
                    let id = self.id();
                    left = self.reconstruct_binop(left, right, kind, operator.loc.clone(), id)
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
                id: self.id(),
                ty: Cell::new(Type::Unknown),
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
                id: self.id(),
                ty: Cell::new(Type::Unknown),
            });
            if self.consume(&[TokenType::CloseParen]).is_none() {
                return Err(ParseError {
                    location: tk.loc,
                    kind: UnbalancedParens,
                });
            }
        }

        Ok(left)
    }

    fn parse_args(&mut self) -> Result<&'a [&'a Expression<'a>], ParseError> {
        let mut args = BumpVec::new_in(self.bump);
        if self.peek_token()?.kind != TokenType::CloseParen {
            let expr = self.parse_expression()?;
            args.push(expr);

            while self.consume(&[TokenType::Comma]).is_some() {
                args.push(self.parse_expression()?);
            }
        }
        Ok(args.into_bump_slice())
    }

    fn parse_primary(&mut self) -> Result<&'a Expression<'a>, ParseError> {
        use TokenType::*;
        let token = self.peek_token()?;

        if let Some(id) = self.consume(&[Id]) {
            let loc = id.loc.clone();
            return Ok(self.bump.alloc(Expression {
                loc,
                kind: VariableAccess(Identifier::from_token(id, self.bump)),
                id: self.id(),
                ty: Cell::new(Type::Unknown),
            }));
        }
        if let Some(integer) = self.consume(&[Integer]) {
            let value = integer.integer;
            let id = self.id();
            return Ok(self.construct_literal(
                AstLiteral::Integral(value),
                integer.loc.clone(),
                id,
                Type::Int64,
            ));
        }
        if let Some(double) = self.consume(&[Double]) {
            let value = double.double;
            let id = self.id();
            return Ok(self.construct_literal(
                AstLiteral::FloatingPoint(value),
                double.loc.clone(),
                id,
                Type::Float64,
            ));
        }
        if let Some(paren) = self.consume(&[OpenParen]) {
            let parenthesis_loc = paren.loc;
            let expr = self.parse_expression()?;
            if self.consume(&[CloseParen]).is_none() {
                return Err(ParseError {
                    location: parenthesis_loc,
                    kind: UnbalancedParens,
                });
            }
            return Ok(expr);
        }

        panic!("unknown token {}", token);
    }

    pub fn construct_literal(
        &self,
        value: AstLiteral,
        loc: SourceLocation,
        id: usize,
        ty: Type<'a>,
    ) -> &'a Expression<'a> {
        self.bump.alloc(Expression {
            loc,
            kind: Literal(value),
            id,
            ty: Cell::new(ty),
        })
    }

    pub fn reconstruct_binop(
        &self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        kind: BinopKind,
        loc: SourceLocation,
        id: usize,
    ) -> &'a Expression<'a> {
        self.bump.alloc(Expression {
            kind: Binop { left, right, kind },
            loc,
            id,
            ty: Cell::new(Type::Unknown), // TODO: maybe use type of left or right
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
    UnknownType(String),
    AtEof,
    UnbalancedParens,
    UnbalancedBraces,
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
            UnbalancedBraces => "unbalanced braces".to_string(),
            UnknownType(b) => format!("unknown type {}", b),
        }
    }

    fn note(&self) -> Option<String> {
        use ParserErrorKind::*;
        match &self.kind {
            AtEof => None,
            UnbalancedParens => Some(format!("last parenthesis located here: {}", self.location)),
            InvalidAssignment(_) => None,
            UnexpectedToken { .. } => None,
            UnbalancedBraces => Some(format!("last braces located here: {}", self.location)),
            UnknownType(_) => None,
        }
    }
}
