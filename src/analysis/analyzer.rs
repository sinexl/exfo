use crate::analysis::get_at::GetAt;
use crate::analysis::r#type::Type;
use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{FunctionDeclaration, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{CompilerError, Identifier, IdentifierBox, SourceLocation, Stack};
use std::collections::HashMap;

type Scope<'ast> = HashMap<
    &'ast str,
    // Exfo allows shadowing, so, each name might relate to several variables in the same scope
    Variable<'ast>,
>;

pub type Resolutions<'ast> = HashMap<&'ast Expression<'ast>, usize>;


pub struct Analyzer<'ast> {
    locals: Stack<Scope<'ast>>,
    current_initializer: Option<Identifier<'ast>>,

    pub resolutions: Resolutions<'ast>,
}

#[derive(Debug)]
struct Variable<'ast> {
    pub state: VariableState,
    pub name: Identifier<'ast>,
    pub r#type: Type,
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
    pub fn new() -> Analyzer<'ast> {
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
                        r#type: Type::Unknown, // todo
                        name: Identifier {
                            name: "print_i64",
                            location: Default::default(),
                        },
                    },
                );
                globals
            }],
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
                if left.r#type != right.r#type {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo,
                        loc: expression.loc.clone(),
                    });
                }
                expression.r#type.set(left.r#type.get());
            }
            ExpressionKind::Unary { item, .. } => {
                self.typecheck_expression(item)?;
                expression.r#type.set(item.r#type.get())
            }
            ExpressionKind::Assignment { target, value } => {
                self.typecheck_expression(target)?;
                self.typecheck_expression(value)?;
                if target.r#type != value.r#type {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo,
                        loc: expression.loc.clone(),
                    });
                }
                expression.r#type.set(target.r#type.get());
            }
            ExpressionKind::Literal(_) => {
                assert_ne!(expression.r#type.get(), Type::Unknown);
            }
            ExpressionKind::VariableAccess(n) => {
                let depth = self.resolutions.get(expression).expect("Analysis failed");
                let var = self.locals.get_at(&n.name, *depth);
                expression.r#type.set(var.r#type);
            }
            ExpressionKind::FunctionCall { .. } => {}
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
            StatementKind::FunctionDeclaration(FunctionDeclaration { name, body }) => {
                self.declare(name, Type::Unknown);
                self.define(name);
                self.enter_scope();
                let errors = self.resolve_statements(body);
                if !errors.is_empty() {
                    return Err(errors);
                }
                self.exit_scope();
            }
            StatementKind::Block(statements) => {
                self.enter_scope();
                for s in *statements {
                    self.resolve_statement(s)?;
                }
                self.exit_scope();
            }
            StatementKind::VariableDeclaration(VariableDeclaration { name, initializer }) => {
                self.current_initializer = Some(name.clone());
                // In variable shadowing, user might want to read from variable with the same name.
                // a := 10;
                // a := a + 15;
                // That's why initializer is resolved prior to declaring variable
                let mut t = Type::Unknown;
                if let Some(init) = initializer {
                    self.resolve_expression(init).map_err(|e| vec![e.into()])?;
                    self.typecheck_expression(init)
                        .map_err(|e| vec![e.into()])?;
                    t = init.r#type.get();
                }
                self.current_initializer = None;
                self.declare(name, t);
                self.define(name);
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
                if let Some(value) = current_scope!(self).get(read.name) {
                    if value.state < VariableState::Defined {
                        // TODO: This is kinda unreachable
                        return Err(ResolverError {
                            loc: read.location.clone(),
                            kind: ResolverErrorKind::ReadingFromInitializer {
                                read: IdentifierBox::from_borrowed(read),
                            },
                        });
                    }
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
    fn declare(&mut self, name: &Identifier<'ast>, r#type: Type<'ast>) {
        let current_scope = current_scope!(self);
        // Case 1: there are already variables with that name defined in the scope.
        if let Some(declaration) = current_scope.get_mut(name.name) {
            // TODO: Catch accidental variable shadows to improve UX of the language
            //   By example, we could warn user if variable was shadowed but not read from.
            *declaration = Variable {
                state: VariableState::Declared,
                name: name.clone(),
                r#type,
            };
            return;
        }
        // Case 2. There are no variables with that name defined is the scope
        current_scope.insert(
            name.name,
            Variable {
                r#type,
                state: VariableState::Declared,
                name: name.clone(),
            },
        );
    }
    fn define(&mut self, name: &Identifier<'ast>) -> () {
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
                // TODO
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
}

impl CompilerError for AnalysisError {
    fn location(&self) -> SourceLocation {
        match self {
            AnalysisError::ResolverError(e) => e.loc.clone(),
            AnalysisError::TypeError(e) => e.loc.clone(),
        }
    }

    fn message(&self) -> String {
        match self {
            AnalysisError::ResolverError(e) => e.message(),
            AnalysisError::TypeError(e) => e.message(),
        }
    }

    fn note(&self) -> Option<String> {
        match self {
            AnalysisError::ResolverError(e) => e.note(),
            AnalysisError::TypeError(e) => e.note(),
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
