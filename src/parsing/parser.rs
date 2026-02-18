use crate::analysis::r#type::{BasicType, PointerType, Type, TypeCtx, TypeId, TypeIdCell};
use crate::ast::binop;
use crate::ast::binop::BinopKind;
use crate::ast::expression::ExpressionKind::{
    Assignment, Binop, FunctionCall, Literal, VariableAccess,
};
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    ExternKind, ExternalFunction, FunctionDeclaration, FunctionParameter, Statement, StatementKind,
    VariableDeclaration,
};
use crate::common::BumpVec;
use crate::common::{CompilerError, Identifier, SourceLocation};
use crate::lexing::token::{Token, TokenType};
use crate::parsing::parser::ParseErrorKind::{
    InvalidAssignment, UnbalancedParens, UnexpectedToken, UnknownType,
};
use bumpalo::Bump;
use std::cell::Cell;
use std::rc::Rc;
/* Grammar:
    program             => decl* EOF ;
    decl                => funcDecl | varDecl | externDecl | statement ;
    statement           => expressionStatement | blockStatement | returnStatement
                           | ifStatement | whileStatement | breakStatement | continueStatement ;
    ifStatement         => "if" expression statement ( "else" statement )? ;
    whileStatement      => (IDENTIFIER ":" )? "while" expression statement ;
    breakStatement      => "break" ";" ;
    continueStatement   => "continue" ";" ;
    expressionStatement => expression ";" ;
    blockStatement      => "{" (declaration*)? "}" ;
    varDecl             => IDENTIFIER ":" type? ("=" expression)? ";" ;
    returnStatement     => "return" expression? ";"
    funcDecl            => "func" IDENTIFIER function ;
    function            => "(" args ")" (":" type)? blockStatement ;
    externDecl          => "extern" extern_kind  "func" IDENTIFIER "(" args (, ...)? ")" ":" type ";"
    externKind          => "C" ;

    expression          => assignment;
    assignment          => IDENTIFIER "=" assignment | binop;
    binop*              => ** | unary;
    unary               => "-" unary | functionCall;
    functionCall        => primary ( "( args ")" )*
    primary             => NUMBER | STRING | BOOLEAN | IDENTIFIER | "(" expression ")" ;
    type                => IDENTIFIER ("*")?


 * - For Binary Operator Precedence, visit ./src/ast/binop.rs
 ** - All binary operations follow the same pattern:
         left ( (<either of operators>) right )*
*/

/// Parser
#[derive(Debug)]
pub struct Parser<'ast, 'types> {
    current: usize,
    tokens: Rc<[Token]>,
    ast_bump: &'ast Bump,
    types:  *mut TypeCtx<'types>,
    nodes_amount: usize,
}

impl<'ast, 'types> Parser<'ast, 'types> {
    pub fn new(tokens: Rc<[Token]>, ast_bump: &'ast Bump, type_ctx: *mut TypeCtx<'types>) -> Parser<'ast, 'types> {
        Self {
            current: 0,
            ast_bump,
            types: type_ctx,
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

#[allow(clippy::result_large_err)]
impl<'ast, 'types> Parser<'ast, 'types> {
    pub fn parse_program(&mut self) -> (&'ast [&'ast Statement<'ast, 'types>], Box<[ParseError]>) {
        let mut statements = BumpVec::new_in(self.ast_bump);
        let mut errors = Vec::new();
        if self.tokens.len() == 1 {
            return (
                &[],
                vec![ParseError {
                    location: self.tokens.last().unwrap().loc.clone(),
                    kind: ParseErrorKind::EmptyInput,
                }]
                .into_boxed_slice(),
            );
        }
        while !self.at_eof() {
            match self.parse_declaration() {
                Err(error) => errors.push(error),
                Ok(statement) => statements.push(statement),
            }
        }

        (statements.into_bump_slice(), errors.into_boxed_slice())
    }

    pub fn parse_declaration(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let state = self.save_state();

        // --- Function declaration ---
        if self.consume(&[TokenType::Func]).is_some() {
            self.restore_state(state);
            return self
                .parse_function_declaration()
                .map_err(|e| self.sync_and_return(e));
        }

        // --- Variable declaration / Named while Loop ---
        if self.consume(&[TokenType::Id]).is_some() {
            // Ambiguity #1. ID at the beginning can be either:
            // Variable declaration (name: type? = ...) OR named while (name: while ... )
            // OR: expression statement with identifier ( i. e a; or a(); )
            if self.consume(&[TokenType::Colon]).is_some() {
                // Ambiguity #2. ID followed by a colon can be either:
                // Variable declaration ( name: type? = ... )
                // OR: named while ( name: while ... )
                // This ambiguity is resolved by checking whether the colon is followed by "while" keyword.
                // While is parsed in parse_statement().
                let is_named_while = self.consume(&[TokenType::While]).is_some();
                self.restore_state(state);
                if !is_named_while {
                    return self.parse_variable_declaration()
                        .map_err(|e| self.sync_and_return(e))
                }
            }
            self.restore_state(state);
        }

        // --- External declaration ---
        if self.consume(&[TokenType::Extern]).is_some() {
            self.restore_state(state);
            return self
                .parse_external_declaration()
                .map_err(|e| self.sync_and_return(e));
        }

        // --- other: statement ---
        self.parse_statement().map_err(|e| self.sync_and_return(e))
    }

    pub fn parse_variable_declaration(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let name = self.expect(&[TokenType::Id], "Expected variable name")?;
        let colon = self.expect(&[TokenType::Colon], "Expected colon")?;

        let mut initializer: Option<&'ast Expression<'ast>> = None;
        let mut variable_type = TypeId::Unknown;
        if self.consume(&[TokenType::Equal]).is_some() {
            initializer = Some(self.parse_expression()?);
        } else {
            variable_type = self.parse_type()?;
            if self.consume(&[TokenType::Equal]).is_some() {
                initializer = Some(self.parse_expression()?);
            }
        }

        self.expect(
            &[TokenType::Semicolon],
            "Expected semicolon after variable declaration",
        )?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::VariableDeclaration(VariableDeclaration {
                name: Identifier::from_token(name, self.ast_bump),
                initializer,
                ty: variable_type.into()
            }),
            loc: colon.loc,
        }))
    }

    pub fn parse_external_declaration(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let keyword = self.expect(&[TokenType::Extern], "Expected extern keyword")?;
        let kind = self.expect(
            &[TokenType::String],
            "Expected external kind (for example \"C\")",
        )?;

        let kind = match kind.string.as_ref() {
            "C" => ExternKind::C,
            _ => {
                return Err(ParseError {
                    location: kind.loc.clone(),
                    kind: ParseErrorKind::UnknownExternKind(String::from(kind.string.as_ref())),
                });
            }
        };

        self.expect(&[TokenType::Func], "Expected func keyword")?;
        let name = self.expect(&[TokenType::Id], "Expected identifier")?;

        self.expect(&[TokenType::OpenParen], "Expected open parenthesis")?;

        // TODO : On error, we should map error into UnbalancedParens
        let mut is_variadic = false;

        let mut res: BumpVec<TypeIdCell> = BumpVec::new_in(self.types().bump());
        if self.peek_token()?.kind != TokenType::CloseParen {

            let expr = self.parse_type()?;
            res.push(expr.into());

            while self.consume(&[TokenType::Comma]).is_some() {
                if let Some(td) = self.consume(&[TokenType::TripleDot]) {
                    is_variadic = true;
                    break;
                }
                res.push(self.parse_type()?.into());
            }
        }

        let parameters: &[TypeIdCell] = res.into_bump_slice();
        let mut message = String::from("Expected closing parenthesis.");
        if is_variadic { message += " Variadic parameters should be last in parameter list." }
        self.expect(&[TokenType::CloseParen], message.as_str())?;
        self.expect(
            &[TokenType::Colon],
            "Expected colon after argument list. \
            In external functions its mandatory to provide return type of the function explicitly",
        )?;
        let return_type = self.parse_type()?;
        self.expect(
            &[TokenType::Semicolon],
            "Expected semicolon after external function declaration. \
        External functions can't have body",
        )?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::Extern(ExternalFunction {
                name: Identifier::from_token(name, self.ast_bump),
                kind,
                is_variadic,
                parameters,
                return_type: return_type.into(),
            }),
            loc: keyword.loc,
        }))
    }

    /// func name (args) {} ...
    pub fn parse_function_declaration(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let func_keyword = self.expect(&[TokenType::Func], "Expected function declaration")?;
        let name = self.expect(&[TokenType::Id], "Expected function name")?;
        debug_assert!(name.kind == TokenType::Id);
        let (parameters, body, return_type) = self.parse_function()?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::FunctionDeclaration(FunctionDeclaration {
                name: Identifier::from_token(name, self.ast_bump),
                body,
                parameters,
                return_type: return_type.into(),
            }),
            loc: func_keyword.loc,
        }))
    }

    /// (args) {}
    pub fn parse_function(
        &mut self,
    ) -> Result<
        (
            &'ast [FunctionParameter<'ast>],
            &'ast [&'ast Statement<'ast, 'types>],
            TypeId,
        ),
        ParseError,
    > {
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
        let mut arguments: &[FunctionParameter<'ast>] = &[];
        if peek(self, o_paren.clone())?.kind != TokenType::CloseParen {
            arguments = self
                .parse_comma_separated(|this| {
                    let id = this.expect(&[TokenType::Id], "Expected parameter name")?;
                    this.expect(&[TokenType::Colon], "Expected colon")?;
                    let ty = this.parse_type()?;
                    Ok(FunctionParameter {
                        name: Identifier::from_token(id, this.ast_bump),
                        ty: ty.into(),
                    })
                })
                .map_err(|mut e| {
                    e.kind = UnbalancedParens;
                    e.location = o_paren.loc;
                    e
                })?;
        }
        self.expect(
            &[TokenType::CloseParen],
            "Expected closing parenthesis after argument list",
        )?;

        let mut return_type = TypeId::Unknown;
        if self.consume(&[TokenType::Colon]).is_some() {
            return_type = self.parse_type()?;
        }

        let block = self.parse_block_statement()?;
        let StatementKind::Block(statements) = block.kind else {
            unreachable!()
        };
        Ok((arguments, statements, return_type))
    }

    pub fn parse_statement(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let state = self.save_state();
        if self.consume(&[TokenType::OpenBrace]).is_some() {
            self.restore_state(state);
            return self.parse_block_statement();
        } else if self.consume(&[TokenType::Return]).is_some() {
            self.restore_state(state);
            return self.parse_return_statement();
        } else if self.consume(&[TokenType::If]).is_some() {
            self.restore_state(state);
            return self.parse_if_statement();
        } else if self.consume(&[TokenType::While]).is_some() {
            self.restore_state(state);
            return self.parse_while_statement();
        } else if self.consume(&[TokenType::Id]).is_some() {
            if self.consume(&[TokenType::Colon]).is_some() && self.consume(&[TokenType::While]).is_some() {
                self.restore_state(state);
                return self.parse_while_statement();
            }
            self.restore_state(state);
        } else if let Some(flow) = self.consume(&[TokenType::Break, TokenType::Continue]) {
            let id = 0.into(); // parser doesn't set any ids. it is done by resolver
            let name = self
                .consume(&[TokenType::Id])
                .map(|e| Identifier::from_token(e, self.ast_bump));
            let (kind, name) = match flow.kind {
                TokenType::Break => (StatementKind::Break { id, name }, "break"),
                TokenType::Continue => (StatementKind::Continue { id, name }, "continue"),
                _ => unreachable!(),
            };
            let statement = self.ast_bump.alloc(Statement {
                kind,
                loc: flow.loc.clone(),
            });
            self.expect(
                &[TokenType::Semicolon],
                &format!("expected semicolon after {name} statement"),
            )?;
            return Ok(statement);
        }
        self.parse_expression_statement()
    }

    pub fn parse_if_statement(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let loc = self.expect(&[TokenType::If], "Expected if keyword")?.loc;

        let condition = self.parse_expression()?;

        let then = self.parse_statement()?;
        let mut r#else = None;
        if self.consume(&[TokenType::Else]).is_some() {
            r#else = Some(self.parse_statement()?);
        }

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::If {
                condition,
                then,
                r#else,
            },
            loc,
        }))
    }
    pub fn parse_while_statement(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let mut name: Option<Identifier<'ast>> = None;
        if let Some(id) = self.consume(&[TokenType::Id]) {
            name = Some(Identifier::from_token(id, self.ast_bump));
            self.expect(&[TokenType::Colon], "Expected colon after 'while' label.")?;
        }
        let loc = self
            .expect(&[TokenType::While], "Expected while keyword")?
            .loc;

        let condition = self.parse_expression()?;
        let body = self.parse_statement()?;
        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::While {
                condition,
                body,
                name,
                id: Cell::new(0),
            },
            loc,
        }))
    }

    pub fn parse_return_statement(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let loc = self
            .expect(&[TokenType::Return], "Expected return keyword")?
            .loc;
        let mut val: Option<&'ast Expression<'ast>> = None;
        if self.peek_token()?.kind != TokenType::Semicolon {
            val = Some(self.parse_expression()?);
        }

        self.expect(
            &[TokenType::Semicolon],
            "Expected semicolon in return expression",
        )?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::Return(val),
            loc,
        }))
    }

    pub fn parse_block_statement(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let mut statements = BumpVec::new_in(self.ast_bump);
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
                kind: ParseErrorKind::UnbalancedBraces,
            });
        }

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::Block(statements.into_bump_slice()),
            loc: left_brace.loc,
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<&'ast Statement<'ast, 'types>, ParseError> {
        let loc = self.peek_token()?.loc;
        let expr = self.parse_expression()?;
        self.expect(&[TokenType::Semicolon], "Expected ';' after expression")?;
        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::ExpressionStatement(expr),
            loc,
        }))
    }

    pub fn parse_expression(&mut self) -> Result<&'ast mut Expression<'ast>, ParseError> {
        self.parse_assignment()
    }

    pub fn parse_type(&mut self) -> Result<TypeId, ParseError> {
        let name = self.expect(&[TokenType::Id], "Expected type name")?;

        let mut ty = match name.string.as_ref() {
            "int" => Ok(TypeId::from_basic(BasicType::Int64)),
            "void" => Ok(TypeId::from_basic(BasicType::Void)),
            "bool" => Ok(TypeId::from_basic(BasicType::Bool)),
            "char_ptr" => Ok(TypeId::from_basic(BasicType::CharPtr)),
            _ => Err(ParseError {
                kind: UnknownType(String::from(name.string.as_ref())),
                location: name.loc.clone(),
            }),
        }?;
        while self.consume(&[TokenType::Star]).is_some() { 
            ty = unsafe { (*self.types).monomorph_or_get_pointer(ty) };
        }
        Ok(ty)
    }

    fn parse_assignment(&mut self) -> Result<&'ast mut Expression<'ast>, ParseError> {
        let target = self.parse_binop(0)?;
        if let Some(tk) = self.consume(&[TokenType::Equal]) {
            let value = self.parse_assignment()?;
            if !target.kind.lvalue() {
                return Err(ParseError {
                    location: target.loc.clone(),
                    kind: InvalidAssignment(target.kind.humanize()),
                });
            }

            return Ok(self.ast_bump.alloc(Expression {
                loc: tk.loc,
                id: self.id(),
                kind: Assignment { target, value },
                ty: TypeId::Unknown.into()
            }));
        }
        Ok(target)
    }

    pub(crate) fn parse_binop(
        &mut self,
        precedence: i32,
    ) -> Result<&'ast mut Expression<'ast>, ParseError> {
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

    fn parse_unary(&mut self) -> Result<&'ast mut Expression<'ast>, ParseError> {
        if let Some(tok) = self.consume(&[TokenType::Minus, TokenType::Star, TokenType::Ampersand]) {
            let item = self.parse_unary()?;

            return Ok(self.ast_bump.alloc(Expression {
                kind: ExpressionKind::Unary {
                    item,
                    operator: match tok.kind {
                        TokenType::Star => UnaryKind::Dereferencing,
                        TokenType::Minus => UnaryKind::Negation,
                        TokenType::Ampersand => UnaryKind::AddressOf,
                        _ => unreachable!()
                    },
                },
                loc: tok.loc,
                id: self.id(),
                ty: TypeId::Unknown.into()
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<&'ast mut Expression<'ast>, ParseError> {
        let mut left = self.parse_primary()?;
        while let Some(tk) = self.consume(&[TokenType::OpenParen]) {
            left = self.ast_bump.alloc(Expression {
                kind: FunctionCall {
                    callee: left,
                    arguments: self.parse_args()?,
                },
                loc: tk.clone().loc,
                id: self.id(),
                ty: TypeId::Unknown.into()
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

    fn parse_args(&mut self) -> Result<&'ast [*mut Expression<'ast>], ParseError> {
        let mut args = BumpVec::<*mut Expression<'ast>>::new_in(self.ast_bump);
        if self.peek_token()?.kind != TokenType::CloseParen {
            let comma_separated= self.parse_comma_separated::<*mut Expression<'ast>>(|this|
                 this.parse_expression().map(|e| e as *mut _)
            )?;
            args.extend(comma_separated);
        }
        Ok(args.into_bump_slice())
    }

    fn parse_primary(&mut self) -> Result<&'ast mut Expression<'ast>, ParseError> {
        use TokenType::*;
        let token = self.peek_token()?;

        if let Some(id) = self.consume(&[Id]) {
            let loc = id.loc.clone();
            return Ok(self.ast_bump.alloc(Expression {
                loc,
                kind: VariableAccess(Identifier::from_token(id, self.ast_bump)),
                id: self.id(),
                ty: TypeId::Unknown.into()
            }));
        }

        if let Some(integer) = self.consume(&[Integer]) {
            let value = integer.integer;
            let id = self.id();
            return Ok(self.construct_literal(
                AstLiteral::Integral(value),
                integer.loc.clone(),
                id,
                TypeId::from_basic(BasicType::Int64),
            ));
        }
        if let Some(double) = self.consume(&[Double]) {
            let value = double.double;
            let id = self.id();
            return Ok(self.construct_literal(
                AstLiteral::FloatingPoint(value),
                double.loc.clone(),
                id,
                TypeId::from_basic(BasicType::Float64)
            ));
        }
        if let Some(boolean) = self.consume(&[False, True]) {
            let id = self.id();
            let bool = match boolean.kind {
                False => false,
                True => true,
                _ => unreachable!(),
            };

            return Ok(self.construct_literal(
                AstLiteral::Boolean(bool),
                boolean.loc.clone(),
                id,
                TypeId::from_basic(BasicType::Bool),
            ));
        }
        if let Some(string) = self.consume(&[String]) {
            let id = self.id();
            let val = self.ast_bump.alloc_str(string.string.as_ref());
            return Ok(self.construct_literal(
                AstLiteral::String(val),
                string.loc.clone(),
                id,
                TypeId::from_basic(BasicType::CharPtr)
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

        self.skip_token();
        Err(ParseError {
            kind: ParseErrorKind::UnknownToken(token.clone()),
            location: token.loc,
        })
    }

    pub fn construct_literal(
        &self,
        value: AstLiteral<'ast>,
        loc: SourceLocation,
        id: usize,
        ty: TypeId
    ) -> &'ast mut Expression<'ast> {
        self.ast_bump.alloc(Expression {
            loc,
            kind: Literal(value),
            id,
            ty: ty.into(),
        })
    }

    pub fn reconstruct_binop(
        &self,
        left: &'ast mut Expression<'ast>,
        right: &'ast mut Expression<'ast>,
        kind: BinopKind,
        loc: SourceLocation,
        id: usize,
    ) -> &'ast mut Expression<'ast> {
        self.ast_bump.alloc(Expression {
            kind: Binop { left, right, kind },
            loc,
            id,
            ty: TypeId::Unknown.into()
        })
    }

    pub fn sync_and_return<T>(&mut self, err: T) -> T {
        self.sync_state_to_statement();
        err
    }
    pub fn sync_state_to_statement(&mut self) {
        if self.at_eof() {
            return;
        }
        self.skip_token();

        while let Ok(tk) = self.peek_token() {
            if tk.kind.is_statement_beginning() {
                break;
            }
            if tk.kind == TokenType::Semicolon {
                self.skip_token();
                break;
            }
            self.skip_token();
        }
        // while !self.at_eof() && self.peek_token().unwrap().kind != TokenType::Semicolon
        // {
        //     // if (self.peek().Type == TokenType.Semicolon) return;
        //     // if (PeekToken().Type.IsStatementBeginning()) return;
        //     // SkipToken();
        // }
    }
}
/// Helper methods for parsing
impl<'ast, 'types> Parser<'ast, 'types> {
    #[allow(clippy::result_large_err)]
    pub fn parse_comma_separated<T>(
        &mut self,
        mut single_item: impl FnMut(&mut Self) -> Result<T, ParseError>,
    ) -> Result<&'ast [T], ParseError> {
        let mut res = BumpVec::new_in(self.ast_bump);
        let expr = single_item(self)?;
        res.push(expr);

        while self.consume(&[TokenType::Comma]).is_some() {
            res.push(single_item(self)?);
        }
        Ok(res.into_bump_slice())
    }
}

// Machinery for parser.
#[allow(clippy::result_large_err)]
impl<'ast, 'types> Parser<'ast, 'types> {
    fn next_token(&mut self) -> Result<Token, ParseError> {
        let tk = self
            .tokens
            .get(self.current)
            .cloned()
            .ok_or_else(|| ParseError {
                location: self.tokens.last().unwrap().loc.clone(),
                kind: ParseErrorKind::AtEof,
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
            kind: ParseErrorKind::AtEof,
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
        let last_token_in_input = self.tokens.last().unwrap().clone();
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

    pub fn types(&self) -> &'types TypeCtx<'types> {
        unsafe { self.types.as_ref().expect("ERROR: Type context is NULL") }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    pub location: SourceLocation,
    pub kind: ParseErrorKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParseErrorKind {
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
    UnknownToken(Token),
    UnknownExternKind(String),
    EmptyInput,
}

impl CompilerError for ParseError {
    fn location(&self) -> SourceLocation {
        self.location.clone()
    }

    fn message(&self) -> String {
        use ParseErrorKind::*;
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
            UnknownExternKind(k) => format!("unknown extern kind: \"{}\"", k),
            UnknownToken(tk) => format!("unknown token: {tk}").to_string(),
            EmptyInput => "empty input: nothing to parse".to_string(),
        }
    }

    fn note(&self) -> Option<String> {
        use ParseErrorKind::*;
        match &self.kind {
            AtEof => None,
            UnbalancedParens => Some(format!("last parenthesis located here: {}", self.location)),
            InvalidAssignment(_) => None,
            UnexpectedToken { .. } => None,
            UnbalancedBraces => Some(format!("last braces located here: {}", self.location)),
            UnknownType(_) => None,
            UnknownExternKind(_) => None, // todo: maybe point out existing external kinds.
            UnknownToken(_) => None,
            EmptyInput => None,
        }
    }
}
