use crate::analysis::get_at::GetAt;
use crate::analysis::r#type::{FunctionType, Type};
use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{FunctionDeclaration, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{BumpVec, CompilerError, Identifier, IdentifierBox, SourceLocation, Stack};
use bumpalo::Bump;
use bumpalo::collections::CollectIn;
use std::collections::HashMap;

type Scope<'ast> = HashMap<
    &'ast str,
    // Exfo allows shadowing, so, each name might relate to several variables in the same scope
    Variable<'ast>,
>;

pub type Resolutions<'ast> = HashMap<&'ast Expression<'ast>, usize>;

pub struct Analyzer<'ast> {
    bump: &'ast Bump,
    locals: Stack<Scope<'ast>>,
    current_initializer: Option<Identifier<'ast>>,

    current_function: Option<&'ast FunctionDeclaration<'ast>>,

    pub resolutions: Resolutions<'ast>,
}

#[derive(Debug)]
struct Variable<'ast> {
    pub state: VariableState,
    pub name: Identifier<'ast>,
    pub ty: Type<'ast>,
}
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum VariableState {
    Declared,
    Defined,
}

macro_rules! current_scope {
    ($dst:expr) => {
        $dst.locals
            .last_mut()
            .expect("COMPILER BUG: Scopes should contain at least global scope.")
    };
}

impl<'ast> Analyzer<'ast> {
    pub fn new(bump: &'ast Bump) -> Analyzer<'ast> {
        Analyzer {
            resolutions: HashMap::new(),
            current_initializer: None,
            locals: vec![{
                // TODO: Obviously, this is a hack.
                let mut globals = HashMap::new();
                globals.insert(
                    "print_i64",
                    Variable {
                        state: VariableState::Defined,
                        ty: Type::Function(FunctionType {
                            return_type: &Type::Void,
                            parameters: &[Type::Int64],
                        }),
                        name: Identifier {
                            name: "print_i64",
                            location: Default::default(),
                        },
                    },
                );
                globals
            }],
            bump,
            current_function: None,
        }
    }

    pub fn analyze_statements(
        &mut self,
        statements: &[&'ast Statement<'ast>],
    ) -> Vec<AnalysisError> {
        let mut analysis_errors: Vec<AnalysisError> = Vec::new();

        let resolution_errors = self.resolve_statements(statements);
        analysis_errors.reserve(resolution_errors.len());
        for e in resolution_errors {
            analysis_errors.push(e)
        }

        analysis_errors
    }
}

impl<'ast> Analyzer<'ast> {
    // Type Checking
    pub fn typecheck_expression(
        &mut self,
        expression: &'ast Expression<'ast>,
    ) -> Result<(), TypeError> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, .. } => {
                self.typecheck_expression(left)?;
                self.typecheck_expression(right)?;
                if left.ty != right.ty {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo,
                        loc: expression.loc.clone(),
                    });
                }
                expression.ty.set(left.ty.get());
            }
            ExpressionKind::Unary { item, .. } => {
                self.typecheck_expression(item)?;
                expression.ty.set(item.ty.get())
            }
            ExpressionKind::Assignment { target, value } => {
                self.typecheck_expression(target)?;
                self.typecheck_expression(value)?;
                if target.ty != value.ty {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo,
                        loc: expression.loc.clone(),
                    });
                }
                expression.ty.set(target.ty.get());
            }
            ExpressionKind::Literal(_) => {
                assert_ne!(expression.ty.get(), Type::Unknown);
            }
            ExpressionKind::VariableAccess(n) => {
                let depth = self.resolutions.get(expression).expect("Analysis failed");
                let var = self.locals.get_at(&n.name, *depth);
                expression.ty.set(var.ty);
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                self.typecheck_expression(callee)?;
                for arg in *arguments {
                    self.typecheck_expression(arg)?;
                }
                dbg!(callee.ty.get());
                if let Type::Function(FunctionType {
                    return_type,
                    parameters: _,
                }) = callee.ty.get()
                {
                    expression.ty.set(*return_type);
                } else {
                    todo!("Proper error handling")
                }
            }
        }
        Ok(())
    }
}

impl<'ast> Analyzer<'ast> {
    // Resolving
    pub fn resolve_statement(
        &mut self,
        statement: &'ast Statement<'ast>,
    ) -> Result<(), Vec<AnalysisError>> {
        match &statement.kind {
            StatementKind::ExpressionStatement(expression) => {
                self.resolve_expression(expression)
                    .map_err(|e| vec![e.into()])?;
                self.typecheck_expression(expression)
                    .map_err(|e| vec![e.into()])?;
            }
            StatementKind::FunctionDeclaration(decl) => {
                let b = self.bump;
                let return_type = self.bump.alloc(decl.return_type.get());
                let fn_type = FunctionType {
                    return_type,
                    parameters: decl
                        .parameters
                        .iter()
                        .map(|p| p.ty)
                        .collect_in::<BumpVec<_>>(b)
                        .into_bump_slice(),
                };
                // TODO: Ensure that in_function is returned to false in cases where function exits early (? operator)
                self.current_function = Some(decl);
                self.declare(&decl.name, Type::Function(fn_type));
                self.define(&decl.name);
                self.enter_scope();
                for param in decl.parameters {
                    self.declare(&param.name, param.ty);
                    self.define(&param.name);
                }
                let errors = self.resolve_statements(decl.body);
                self.exit_scope();
                self.current_function = None;
                if !errors.is_empty() {
                    return Err(errors);
                }
            }
            StatementKind::Block(statements) => {
                self.enter_scope();
                for s in *statements {
                    self.resolve_statement(s)?;
                }
                self.exit_scope();
            }
            StatementKind::VariableDeclaration(VariableDeclaration {
                name,
                initializer,
                ty,
            }) => {
                self.current_initializer = Some(name.clone());
                let variable_type = ty;
                // In variable shadowing, user might want to read from variable with the same name.
                // a := 10;
                // a := a + 15;
                // That's why initializer is resolved prior to declaring variable
                let mut initializer_type = Type::Unknown;
                if let Some(init) = initializer {
                    self.resolve_expression(init).map_err(|e| vec![e.into()])?;
                    self.typecheck_expression(init)
                        .map_err(|e| vec![e.into()])?;
                    initializer_type = init.ty.get();
                }

                if variable_type.get() == Type::Unknown {
                    variable_type.set(initializer_type);
                } else if variable_type.get() != initializer_type {
                    todo!("For now, initializer type must be the same with declaration type")
                }

                self.current_initializer = None;
                self.declare(name, variable_type.get());
                self.define(name);
            }
            StatementKind::Return(val) => {
                let current_fn = self
                    .current_function
                    .ok_or_else(|| vec![AnalysisError::TopLevelReturn(statement.loc.clone())])?;
                if let Some(val) = val {
                    self.resolve_expression(val).map_err(|e| vec![e.into()])?;
                    self.typecheck_expression(val).map_err(|e| vec![e.into()])?;

                    if let Type::Unknown = current_fn.return_type.get() {
                        current_fn.return_type.set(val.ty.get());
                    } else if current_fn.return_type != val.ty {
                        todo!("TODO: Proper error handling")
                    }
                }
            }
        }
        Ok(())
    }

    pub fn resolve_statements(
        &mut self,
        statements: &[&'ast Statement<'ast>],
    ) -> Vec<AnalysisError> {
        let mut resolution_errors = Vec::new();
        for statement in statements {
            if let Err(mut err) = self.resolve_statement(statement) {
                resolution_errors.append(&mut err);
            }
        }
        resolution_errors
    }

    fn resolve_expression(
        &mut self,
        expression: &'ast Expression<'ast>,
    ) -> Result<(), ResolverError> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            ExpressionKind::Unary { item, .. } => {
                self.resolve_expression(item)?;
            }
            ExpressionKind::Assignment { target, value } => {
                self.resolve_expression(target)?;
                self.resolve_expression(value)?;
            }
            ExpressionKind::VariableAccess(read) => {
                if let Some(value) = current_scope!(self).get(read.name)
                    && value.state < VariableState::Defined
                {
                    // TODO: This is kinda unreachable
                    return Err(ResolverError {
                        loc: read.location.clone(),
                        kind: ResolverErrorKind::ReadingFromInitializer {
                            read: IdentifierBox::from_borrowed(read),
                        },
                    });
                }
                self.resolve_local_variable(expression, read)?;
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                self.resolve_expression(callee)?;
                for arg in *arguments {
                    self.resolve_expression(arg)?;
                }
            }
            ExpressionKind::Literal(_) => { /* nothing */ }
        }
        Ok(())
    }
    fn declare(&mut self, name: &Identifier<'ast>, ty: Type<'ast>) {
        let current_scope = current_scope!(self);
        // Case 1: there are already variables with that name defined in the scope.
        if let Some(declaration) = current_scope.get_mut(name.name) {
            // TODO: Catch accidental variable shadows to improve UX of the language
            //   By example, we could warn user if variable was shadowed but not read from.
            *declaration = Variable {
                state: VariableState::Declared,
                name: name.clone(),
                ty,
            };
            return;
        }
        // Case 2. There are no variables with that name defined is the scope
        current_scope.insert(
            name.name,
            Variable {
                ty,
                state: VariableState::Declared,
                name: name.clone(),
            },
        );
    }
    fn define(&mut self, name: &Identifier<'ast>) {
        if let Some(var) = current_scope!(self).get_mut(&name.name) {
            if var.state == VariableState::Defined {
                panic!(
                    "COMPILER BUG: Variable `{}` ({:?}) is already defined\n",
                    name.name, var.state
                );
            }
            var.state = VariableState::Defined;
            return;
        }
        panic!(
            "COMPILER BUG: Variable `{}` is not declared to be defined.",
            name.name
        );
    }

    pub fn enter_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.locals.pop();
    }
    fn resolve_local_variable(
        &mut self,
        expression: &'ast Expression<'ast>,
        var: &Identifier<'ast>,
    ) -> Result<(), ResolverError> {
        for (i, scope) in self.locals.iter().rev().enumerate() {
            if scope.contains_key(var.name) {
                self.resolutions.insert(expression, i);
                return Ok(());
            }
        }

        if self
            .current_initializer
            .clone()
            .filter(|e| e.name == var.name)
            .is_some()
        {
            return Err(ResolverError {
                loc: var.location.clone(),
                kind: ResolverErrorKind::ReadingFromInitializer {
                    read: IdentifierBox::from_borrowed(var),
                },
            });
        }

        Err(ResolverError {
            loc: var.location.clone(),
            kind: ResolverErrorKind::UndeclaredIdentifier {
                usage: IdentifierBox::from_borrowed(var),
            },
        })
    }
}

#[allow(dead_code)]
fn debug_scopes(scopes: &Stack<Scope<'_>>) {
    for (index, scope) in scopes.iter().rev().enumerate() {
        println!("---- Scope {index} ----");
        debug_scope(scope);
    }
}
#[allow(dead_code)]
fn debug_scope(scope: &Scope<'_>) {
    for (name, resolutions) in scope {
        println!("{name} = {resolutions:?}");
    }
}

#[derive(Clone)]
pub enum AnalysisError {
    ResolverError(ResolverError),
    TypeError(TypeError),
    TopLevelReturn(SourceLocation),
}

impl CompilerError for AnalysisError {
    fn location(&self) -> SourceLocation {
        match &self {
            AnalysisError::ResolverError(e) => e.loc.clone(),
            AnalysisError::TypeError(e) => e.loc.clone(),
            &AnalysisError::TopLevelReturn(loc) => loc.clone(),
        }
    }

    fn message(&self) -> String {
        match &self {
            AnalysisError::ResolverError(e) => e.message(),
            AnalysisError::TypeError(e) => e.message(),
            AnalysisError::TopLevelReturn(_) => "Could not return from top-level.".to_string(),
        }
    }

    fn note(&self) -> Option<String> {
        match &self {
            AnalysisError::ResolverError(e) => e.note(),
            AnalysisError::TypeError(e) => e.note(),
            AnalysisError::TopLevelReturn(_) => None,
        }
    }
}

#[derive(Clone)]
pub struct TypeError {
    pub loc: SourceLocation,
    pub kind: TypeErrorKind,
}

#[derive(Clone)]
pub enum TypeErrorKind {
    Todo,
}

#[derive(Clone)]
pub struct ResolverError {
    pub loc: SourceLocation,
    pub kind: ResolverErrorKind,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ResolverErrorKind {
    UndeclaredIdentifier { usage: IdentifierBox },
    ReadingFromInitializer { read: IdentifierBox },
}

impl CompilerError for ResolverError {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn message(&self) -> String {
        match &self.kind {
            ResolverErrorKind::UndeclaredIdentifier { usage } => {
                format!("Undeclared identifier `{}`", usage.name)
            }
            ResolverErrorKind::ReadingFromInitializer { read } => format!(
                "Could not read `{n}` from it's own initializer",
                n = read.name
            ),
        }
    }

    fn note(&self) -> Option<String> {
        match &self.kind {
            ResolverErrorKind::UndeclaredIdentifier { .. } => None,
            ResolverErrorKind::ReadingFromInitializer { .. } => None,
        }
    }
}

impl CompilerError for TypeError {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn message(&self) -> String {
        match self.kind {
            TypeErrorKind::Todo => todo!(),
        }
    }

    fn note(&self) -> Option<String> {
        match self.kind {
            TypeErrorKind::Todo => None,
        }
    }
}

impl From<TypeError> for AnalysisError {
    fn from(value: TypeError) -> Self {
        Self::TypeError(value)
    }
}
impl From<ResolverError> for AnalysisError {
    fn from(value: ResolverError) -> Self {
        Self::ResolverError(value)
    }
}
