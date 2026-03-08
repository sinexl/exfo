use crate::analysis::r#type::{BasicType, PointerType, TypeId, TypeIdCell};
use crate::analysis::type_system::type_context::TypeCtx;
use crate::ast::binop;
use crate::ast::binop::BinopKind;
use crate::ast::expression::ExpressionKind::{
    Assignment, Binop, FunctionCall, Literal, VariableAccess,
};
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, RefId, SymId, UnaryKind};
use crate::ast::statement::{
    ExternKind, ExternalFunction, FunctionDeclaration, FunctionParameter, Statement, StatementKind,
    VariableDeclaration,
};
use crate::common::identifier::Identifier;
use crate::common::{BumpVec, SourceLocation};
use crate::lexing::token::{Token, TokenType};
use crate::parsing::errors::{ParseError, ParseErrorKind};
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
pub struct Parser<'ast, 'types, 'errors> {
    current: usize,
    tokens: Rc<[Token]>,
    ast_bump: &'ast Bump,
    errors_bump: &'errors Bump,
    types: *mut TypeCtx<'types>,
    // All expressions are assigned with unique ID's
    nodes_count: usize,
    // All symbols (declarations) are assigned with unique ID besides the expression ID
    symbols_count: usize,
}

macro_rules! expect {
    // Case: Just the tokens
    ($self:ident, $expected:expr) => {
        $self.expect($expected, "", None)
    };
    // Case: Tokens + "where"
    ($self:ident, $expected:expr, $where:expr) => {
        $self.expect($expected, $where, None)
    };
    // Case: Tokens + "where" + "note"
    ($self:ident, $expected:expr, $where:expr, $note:expr) => {
        $self.expect($expected, $where, Some($note))
    };
}
type ParseResult<'errors, T> = Result<T, ParseError<'errors>>;


impl<'ast, 'types, 'errors> Parser<'ast, 'types, 'errors> {
    pub fn new(
        tokens: Rc<[Token]>,
        ast_bump: &'ast Bump,
        errors_bump: &'errors Bump,
        type_ctx: *mut TypeCtx<'types>,
    ) -> Parser<'ast, 'types, 'errors> {
        Self {
            current: 0,
            ast_bump,
            errors_bump,
            types: type_ctx,
            tokens,
            nodes_count: 0,
            symbols_count: 0,
        }
    }

    pub fn id(&mut self) -> usize {
        let current = self.nodes_count;
        self.nodes_count += 1;
        current
    }

    pub fn sym_id(&mut self) -> SymId {
        let current = self.symbols_count;
        self.symbols_count += 1;
        SymId(current)
    }
}

impl<'ast, 'types, 'errors> Parser<'ast, 'types, 'errors> {
    pub fn parse_program(
        &mut self,
    ) -> (
        &'ast [&'ast Statement<'ast, 'types>],
        BumpVec<'errors, ParseError<'errors>>,
    ) {
        let mut statements = BumpVec::new_in(self.ast_bump);
        let mut errors = BumpVec::new_in(self.errors_bump);
        if self.tokens.len() == 1 {
            return (&[], {
                let mut x = BumpVec::new_in(self.errors_bump);
                x.push(ParseError {
                    loc: self.tokens.last().unwrap().loc.clone(),
                    kind: ParseErrorKind::EmptyInput,
                });
                x
            });
        }
        while !self.at_eof() {
            match self.parse_declaration() {
                Err(error) => errors.push(error),
                Ok(statement) => statements.push(statement),
            }
        }

        (statements.into_bump_slice(), errors)
    }

    pub fn parse_declaration(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let state = self.save_state();
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
                    return self
                        .parse_variable_declaration()
                        .map_err(|e| self.sync_and_return(e));
                }
            }
            self.restore_state(state);
        }
        // Fall through to other cases
        match self.peek_token()?.kind {
            // --- Function declaration ---
            TokenType::Func => self
                .parse_function_declaration()
                .map_err(|e| self.sync_and_return(e)),
            // --- External declaration ---
            TokenType::Extern => self
                .parse_external_declaration()
                .map_err(|e| self.sync_and_return(e)),
            // --- other: statement ---
            _ => self.parse_statement().map_err(|e| self.sync_and_return(e)),
        }
    }

    pub fn parse_variable_declaration(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let name = expect!(self, &[TokenType::Id])?;
        let colon = expect!(self, &[TokenType::Colon])?;

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

        expect!(self, &[TokenType::Semicolon], "after variable declaration")?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::VariableDeclaration(
                VariableDeclaration {
                    name: Identifier::from_token(name, self.ast_bump),
                    initializer,
                    ty: variable_type.into(),
                },
                self.sym_id(),
            ),
            loc: colon.loc,
        }))
    }

    pub fn parse_external_declaration(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let keyword = expect!(self, &[TokenType::Extern])?;
        let kind = expect!(self, &[TokenType::String])?;

        let kind = match kind.string.as_ref() {
            "C" => ExternKind::C,
            _ => {
                return Err(ParseError {
                    loc: kind.loc.clone(),
                    kind: ParseErrorKind::UnknownExternKind(
                        self.errors_bump.alloc_str(kind.string.as_ref()),
                    ),
                });
            }
        };

        expect!(self, &[TokenType::Func])?;
        let name = expect!(self, &[TokenType::Id])?;

        expect!(self, &[TokenType::OpenParen])?;

        // TODO : On error, we should map error into UnbalancedParens
        let mut is_variadic = false;

        let mut res: BumpVec<TypeIdCell> = BumpVec::new_in(self.types().bump());
        if self.peek_token()?.kind != TokenType::CloseParen {
            let expr = self.parse_type()?;
            res.push(expr.into());

            while self.consume(&[TokenType::Comma]).is_some() {
                if self.consume(&[TokenType::TripleDot]).is_some() {
                    is_variadic = true;
                    break;
                }
                res.push(self.parse_type()?.into());
            }
        }

        let parameters: &[TypeIdCell] = res.into_bump_slice();
        let note = if is_variadic {
            Some("Variadic parameters should be last in parameter list.")
        } else {
            None
        };
        self.expect(
            &[TokenType::CloseParen],
            "after function argument declaration",
            note,
        )?;
        expect!(
            self,
            &[TokenType::Colon],
            "after argument list",
            "In external functions its mandatory to provide return type of the function explicitly"
        )?;
        let return_type = self.parse_type()?;
        expect!(
            self,
            &[TokenType::Semicolon],
            "after external function declaration.",
            "External functions can't have body"
        )?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::Extern(
                ExternalFunction {
                    name: Identifier::from_token(name, self.ast_bump),
                    kind,
                    is_variadic,
                    parameters,
                    return_type: return_type.into(),
                },
                self.sym_id(),
            ),
            loc: keyword.loc,
        }))
    }

    /// func name (args) {} ...
    pub fn parse_function_declaration(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let func_keyword = expect!(self, &[TokenType::Func])?;
        let name = expect!(self, &[TokenType::Id])?;
        debug_assert!(name.kind == TokenType::Id);
        let (parameters, body, return_type) = self.parse_function()?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::FunctionDeclaration(
                FunctionDeclaration {
                    name: Identifier::from_token(name, self.ast_bump),
                    body,
                    parameters,
                    return_type: return_type.into(),
                },
                self.sym_id(),
            ),
            loc: func_keyword.loc,
        }))
    }

    /// (args) {}
    pub fn parse_function(
        &mut self,
    ) -> ParseResult<'errors,
        (
            &'ast [FunctionParameter<'ast>],
            &'ast [&'ast Statement<'ast, 'types>],
            TypeId,
        ),
    > {
        let o_paren = expect!(self, &[TokenType::OpenParen], "in function declaration")?;
        let peek = |this: &mut Self, tk: Token| {
            this.peek_token().map_err(|mut e| {
                e.kind = ParseErrorKind::UnbalancedParens;
                e.loc = tk.loc;
                e
            })
        };
        let mut arguments: &[FunctionParameter<'ast>] = &[];
        if peek(self, o_paren.clone())?.kind != TokenType::CloseParen {
            arguments = self
                .parse_comma_separated(|this| {
                    let id = expect!(this, &[TokenType::Id])?;
                    expect!(this, &[TokenType::Colon])?;
                    let ty = this.parse_type()?;
                    Ok(FunctionParameter {
                        name: Identifier::from_token(id, this.ast_bump),
                        ty: ty.into(),
                        id: this.sym_id(),
                    })
                })
                .map_err(|mut e| {
                    e.kind = ParseErrorKind::UnbalancedParens;
                    e.loc = o_paren.loc;
                    e
                })?;
        }
        expect!(self, &[TokenType::CloseParen], "after argument list")?;

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

    pub fn parse_statement(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        use TokenType::*;
        let state = self.save_state();

        if self.consume(&[Id]).is_some() {
            if self.consume(&[Colon]).is_some() && self.consume(&[While]).is_some() {
                self.restore_state(state);
                return self.parse_while_statement();
            }
        }
        self.restore_state(state);
        let tk = self.peek_token()?;
        let loc = tk.loc.clone();
        match tk.kind {
            OpenBrace => self.parse_block_statement(),
            Return => self.parse_return_statement(),
            If => self.parse_if_statement(),
            While => self.parse_while_statement(),
            flow if flow == Break || flow == Continue => {
                self.skip_token();
                let id = 0.into(); // parser doesn't set any ids. it is done by resolver
                let name = self
                    .consume(&[Id])
                    .map(|e| Identifier::from_token(e, self.ast_bump));
                let (kind, name) = match flow {
                    Break => (StatementKind::Break { id, name }, "break"),
                    Continue => (StatementKind::Continue { id, name }, "continue"),
                    _ => unreachable!(),
                };

                let statement = self.ast_bump.alloc(Statement { kind, loc });

                expect!(
                    self,
                    &[Semicolon],
                    bumpalo::format!(in self.errors_bump, "after {} statement", name)
                        .into_bump_str()
                )?;
                Ok(statement)
            }
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_if_statement(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let loc = expect!(self, &[TokenType::If])?.loc;

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
    pub fn parse_while_statement(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let mut name: Option<Identifier<'ast>> = None;
        if let Some(id) = self.consume(&[TokenType::Id]) {
            name = Some(Identifier::from_token(id, self.ast_bump));
            expect!(self, &[TokenType::Colon], "after 'while' label")?;
        }
        let loc = expect!(self, &[TokenType::While])?.loc;

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

    pub fn parse_return_statement(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let loc = expect!(self, &[TokenType::Return])?.loc;
        let mut val: Option<&'ast Expression<'ast>> = None;
        if self.peek_token()?.kind != TokenType::Semicolon {
            val = Some(self.parse_expression()?);
        }

        expect!(self, &[TokenType::Semicolon], "in return expression")?;

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::Return(val),
            loc,
        }))
    }

    pub fn parse_block_statement(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let mut statements = BumpVec::new_in(self.ast_bump);
        let left_brace = expect!(self, &[TokenType::OpenBrace], "in block statement")?;

        while !self.at_eof() && self.peek_token()?.kind != TokenType::CloseBrace {
            let statement = self.parse_declaration()?;
            statements.push(statement);
        }

        if self.consume(&[TokenType::CloseBrace]).is_none() {
            return Err(ParseError {
                loc: left_brace.clone().loc,
                kind: ParseErrorKind::UnbalancedBraces,
            });
        }

        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::Block(statements.into_bump_slice()),
            loc: left_brace.loc,
        }))
    }

    fn parse_expression_statement(
        &mut self,
    ) -> ParseResult<'errors, &'ast Statement<'ast, 'types>> {
        let loc = self.peek_token()?.loc;
        let expr = self.parse_expression()?;
        expect!(self, &[TokenType::Semicolon], "after expression")?;
        Ok(self.ast_bump.alloc(Statement {
            kind: StatementKind::ExpressionStatement(expr),
            loc,
        }))
    }

    pub fn parse_expression(&mut self) -> ParseResult<'errors, &'ast mut Expression<'ast>> {
        self.parse_assignment()
    }

    pub fn parse_type(&mut self) -> ParseResult<'errors, TypeId> {
        let name = expect!(self, &[TokenType::Id])?;

        let mut ty = match name.string.as_ref() {
            "int" => Ok(TypeId::from_basic(BasicType::Int64)),
            "void" => Ok(TypeId::from_basic(BasicType::Void)),
            "bool" => Ok(TypeId::from_basic(BasicType::Bool)),
            "char_ptr" => Ok(TypeId::from_basic(BasicType::CharPtr)),
            _ => Err(ParseError {
                kind: ParseErrorKind::UnknownType(self.errors_bump.alloc_str(name.string.as_ref())),
                loc: name.loc.clone(),
            }),
        }?;
        while self.consume(&[TokenType::Star]).is_some() {
            ty = unsafe { (*self.types).monomorph_or_get_pointer(PointerType { inner: ty }) };
        }
        Ok(ty)
    }

    fn parse_assignment(&mut self) -> ParseResult<'errors, &'ast mut Expression<'ast>> {
        let target = self.parse_binop(0)?;
        if let Some(tk) = self.consume(&[TokenType::Equal]) {
            let value = self.parse_assignment()?;
            if !target.kind.lvalue() {
                return Err(ParseError {
                    loc: target.loc.clone(),
                    kind: ParseErrorKind::InvalidAssignment(target.kind.humanize()),
                });
            }

            return Ok(self.ast_bump.alloc(Expression {
                loc: tk.loc,
                id: self.id(),
                kind: Assignment { target, value },
                ty: TypeId::Unknown.into(),
            }));
        }
        Ok(target)
    }

    pub(crate) fn parse_binop(
        &mut self,
        precedence: i32,
    ) -> ParseResult<'errors, &'ast mut Expression<'ast>> {
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

    fn parse_unary(&mut self) -> ParseResult<'errors, &'ast mut Expression<'ast>> {
        if let Some(tok) = self.consume(&[
            TokenType::Minus,
            TokenType::Star,
            TokenType::Ampersand,
            TokenType::Bang,
        ]) {
            let item = self.parse_unary()?;
            let operator = match tok.kind {
                TokenType::Star => UnaryKind::Dereferencing,
                TokenType::Minus => UnaryKind::Negation,
                TokenType::Ampersand => {
                    if !item.kind.lvalue() {
                        return Err(ParseError {
                            loc: tok.loc.clone(),
                            kind: ParseErrorKind::InvalidAddressOf(item.kind.humanize()),
                        });
                    }
                    UnaryKind::AddressOf
                }
                TokenType::Bang => UnaryKind::Not,
                _ => unreachable!(),
            };

            return Ok(self.ast_bump.alloc(Expression {
                kind: ExpressionKind::Unary { item, operator },
                loc: tok.loc,
                id: self.id(),
                ty: TypeId::Unknown.into(),
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<'errors, &'ast mut Expression<'ast>> {
        let mut left = self.parse_primary()?;
        while let Some(tk) = self.consume(&[TokenType::OpenParen]) {
            left = self.ast_bump.alloc(Expression {
                kind: FunctionCall {
                    callee: left,
                    arguments: self.parse_args()?,
                },
                loc: tk.clone().loc,
                id: self.id(),
                ty: TypeId::Unknown.into(),
            });
            if self.consume(&[TokenType::CloseParen]).is_none() {
                return Err(ParseError {
                    loc: tk.loc,
                    kind: ParseErrorKind::UnbalancedParens,
                });
            }
        }

        Ok(left)
    }

    fn parse_args(&mut self) -> ParseResult<'errors, &'ast [*mut Expression<'ast>]> {
        let mut args = BumpVec::<*mut Expression<'ast>>::new_in(self.ast_bump);
        if self.peek_token()?.kind != TokenType::CloseParen {
            let comma_separated = self.parse_comma_separated::<*mut Expression<'ast>>(|this| {
                this.parse_expression().map(|e| e as *mut _)
            })?;
            args.extend(comma_separated);
        }
        Ok(args.into_bump_slice())
    }

    fn parse_primary(&mut self) -> ParseResult<'errors, &'ast mut Expression<'ast>> {
        use TokenType::*;
        let token = self.peek_token()?;
        self.skip_token();
        let loc = token.loc.clone();
        let id = self.id();
        match token.kind {
            Id => Ok(self.ast_bump.alloc(Expression {
                loc,
                kind: VariableAccess(
                    Identifier::from_token(token, self.ast_bump),
                    RefId::unknown(),
                ),
                id,
                ty: TypeId::Unknown.into(),
            })),
            Integer => Ok(self.construct_literal(
                AstLiteral::Integral(token.integer),
                loc,
                id,
                TypeId::from_basic(BasicType::Int64),
            )),
            Double => Ok(self.construct_literal(
                AstLiteral::FloatingPoint(token.double),
                loc,
                id,
                TypeId::from_basic(BasicType::Float64),
            )),
            boolean if boolean == True || boolean == False => {
                let bool = match boolean {
                    False => false,
                    True => true,
                    _ => unreachable!(),
                };
                Ok(self.construct_literal(
                    AstLiteral::Boolean(bool),
                    loc,
                    id,
                    TypeId::from_basic(BasicType::Bool),
                ))
            }
            String => {
                let val = self.ast_bump.alloc_str(token.string.as_ref());
                Ok(self.construct_literal(
                    AstLiteral::String(val),
                    loc,
                    id,
                    TypeId::from_basic(BasicType::CharPtr),
                ))
            }
            OpenParen => {
                let expr = self.parse_expression()?;
                if self.consume(&[CloseParen]).is_none() {
                    return Err(ParseError {
                        loc,
                        kind: ParseErrorKind::UnbalancedParens,
                    });
                }
                Ok(expr)
            }

            _ => {
                self.nodes_count -= 1;
                Err(ParseError {
                    kind: ParseErrorKind::UnknownToken(token.clone()),
                    loc: token.loc,
                })
            }
        }
    }

    pub fn construct_literal(
        &self,
        value: AstLiteral<'ast>,
        loc: SourceLocation,
        id: usize,
        ty: TypeId,
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
            ty: TypeId::Unknown.into(),
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
impl<'ast, 'types, 'errors> Parser<'ast, 'types, 'errors> {
    pub fn parse_comma_separated<T>(
        &mut self,
        mut single_item: impl FnMut(&mut Self) -> ParseResult<'errors, T>,
    ) -> ParseResult<'errors, &'ast [T]> {
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
impl<'ast, 'types, 'errors> Parser<'ast, 'types, 'errors> {
    pub fn count_symbols(&self) -> usize {
        self.symbols_count
    }
    fn next_token(&mut self) -> ParseResult<'errors, Token> {
        let tk = self
            .tokens
            .get(self.current)
            .cloned()
            .ok_or_else(|| ParseError {
                loc: self.tokens.last().unwrap().loc.clone(),
                kind: ParseErrorKind::AtEof,
            })?;
        self.current += 1;
        Ok(tk)
    }
    pub fn skip_token(&mut self) {
        let _ = self.next_token();
    }
    pub fn peek_token(&self) -> ParseResult<'errors, Token> {
        let r = self.tokens.get(self.current);
        if let Some(token) = r {
            return Ok(token.clone());
        }

        Err(ParseError {
            loc: self.tokens.last().unwrap().loc.clone(),
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

    pub fn expect(
        &mut self,
        expected: &[TokenType],
        r#where: &'errors str,
        note: Option<&'errors str>,
    ) -> ParseResult<'errors, Token> {
        if let Some(tk) = self.consume(expected) {
            return Ok(tk);
        }
        let last_token_in_input = self.tokens.last().unwrap().clone();
        let token = self.next_token().unwrap_or(last_token_in_input.clone());
        Err(ParseError {
            loc: self.peek_token().unwrap_or(last_token_in_input).loc,
            kind: ParseErrorKind::UnexpectedToken {
                expected: self.errors_bump.alloc_slice_clone(expected),
                got: token.kind,
                note,
                r#where,
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

