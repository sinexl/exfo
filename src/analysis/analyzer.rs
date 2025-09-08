use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{FunctionDeclaration, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{CompilerError, Identifier, IdentifierBox, SourceLocation};
use std::collections::HashMap;

type Stack<T> = Vec<T>;
type Scope<'a> = HashMap<
    &'a str,
    // Exfo allows shadowing, so, each name might relate to several variables in the same scope
    Vec<Variable<'a>>,
>;
pub struct Analyzer<'a> {
    pub resolutions: HashMap<&'a Expression<'a>, usize>,
    locals: Stack<Scope<'a>>,
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub defined: bool,
    pub name: Identifier<'a>,
}

macro_rules! current_scope {
    ($dst:expr) => {
        $dst.locals
            .last_mut()
            .expect("COMPILER BUG: Scopes should contain at least global scope.")
    };
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Analyzer<'a> {
        Analyzer {
            resolutions: HashMap::new(),
            locals: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.locals.pop();
    }

    fn resolve_expression(&mut self, expression: &'a Expression<'a>) -> Result<(), ResolverError> {
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
                if let Some(values) = current_scope!(self).get(read.name) {
                    if !values.last().unwrap().defined {
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

    pub fn resolve_statement(
        &mut self,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ResolverError>> {
        match &statement.kind {
            StatementKind::ExpressionStatement(expression) => {
                self.resolve_expression(expression).map_err(|e| vec![e])?;
            }
            StatementKind::FunctionDeclaration(FunctionDeclaration { name, body }) => {
                self.declare(name);
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
                self.declare(name);
                if let Some(init) = initializer {
                    self.resolve_expression(init).map_err(|e| vec![e])?;
                }
                self.define(name);
            }
        }
        Ok(())
    }

    pub fn resolve_statements(&mut self, statements: &[&'a Statement<'a>]) -> Vec<ResolverError> {
        let mut resolution_errors = Vec::new();
        for statement in statements {
            if let Err(mut err) = self.resolve_statement(statement) {
                resolution_errors.append(&mut err);
            }
        }
        resolution_errors
    }

    pub fn analyze_statements(&mut self, statements: &[&'a Statement<'a>]) -> Vec<AnalysisError> {
        let mut analysis_errors: Vec<AnalysisError> = Vec::new();

        let resolution_errors = self.resolve_statements(statements);
        analysis_errors.reserve(resolution_errors.len());
        for e in resolution_errors {
            analysis_errors.push(AnalysisError::ResolverError(e))
        }

        analysis_errors
    }

    fn resolve_local_variable(
        &mut self,
        expression: &'a Expression<'a>,
        var: &Identifier<'a>,
    ) -> Result<(), ResolverError> {
        for (i, scope) in self.locals.iter().rev().enumerate() {
            if scope.contains_key(var.name) {
                // TODO
                self.resolutions.insert(expression, i);
                return Ok(());
            }
        }

        Err(ResolverError {
            loc: var.location.clone(),
            kind: ResolverErrorKind::UndeclaredVariable {
                usage: IdentifierBox::from_borrowed(var),
            },
        })
    }

    fn declare(&mut self, name: &Identifier<'a>) {
        let current_scope = current_scope!(self);
        // Case 1: there are already variables with that name defined in the scope.
        if let Some(declarations) = current_scope.get_mut(name.name) {
            declarations.push(Variable {
                defined: false,
                name: name.clone(),
            });
            return;
        }
        // Case 2. There are no variables with that name defined is the scope
        current_scope.insert(
            name.name,
            vec![Variable {
                defined: false,
                name: name.clone(),
            }],
        );
    }
    fn define(&mut self, name: &Identifier<'a>) -> () {
        if let Some(definitions) = current_scope!(self).get_mut(&name.name) {
            if let Some(var) = definitions.last_mut() {
                if var.defined {
                    panic!("COMPILER BUG: Variable `{}` is already defined.", name.name);
                }
                var.defined = true;
                return;
            }
        }
        panic!(
            "COMPILER BUG: Variable `{}` is not declared to be defined.",
            name.name
        );
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

pub enum AnalysisError {
    ResolverError(ResolverError),
}

impl CompilerError for AnalysisError {
    fn location(&self) -> SourceLocation {
        match self {
            AnalysisError::ResolverError(e) => e.loc.clone(),
        }
    }

    fn message(&self) -> String {
        match self {
            AnalysisError::ResolverError(e) => e.message(),
        }
    }

    fn note(&self) -> Option<String> {
        match self {
            AnalysisError::ResolverError(e) => e.note(),
        }
    }
}

pub struct ResolverError {
    pub loc: SourceLocation,
    pub kind: ResolverErrorKind,
}
pub enum ResolverErrorKind {
    UndeclaredVariable { usage: IdentifierBox },
    ReadingFromInitializer { read: IdentifierBox },
}

impl CompilerError for ResolverError {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn message(&self) -> String {
        match &self.kind {
            ResolverErrorKind::UndeclaredVariable { usage } => {
                format!("Undeclared variable: `{}`", usage.name)
            }
            ResolverErrorKind::ReadingFromInitializer { read } => format!(
                "Could not read `{n}` from it's own initializer",
                n = read.name
            ),
        }
    }

    fn note(&self) -> Option<String> {
        match &self.kind {
            ResolverErrorKind::UndeclaredVariable { .. } => None,
            ResolverErrorKind::ReadingFromInitializer { .. } => None,
        }
    }
}
