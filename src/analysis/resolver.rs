use crate::analysis::get_at::GetAt;
use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{ExternalFunction, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{
    CompilerError, CompilerWarning, Identifier, IdentifierBox, IntoBox, SourceLocation, Stack,
};
use std::collections::HashMap;

type Scope<'ast> = HashMap<
    &'ast str,
    // Exfo allows shadowing, so, each name might relate to several variables in the same scope
    Variable<'ast>,
>;

pub type Resolutions<'ast> = HashMap<&'ast Expression<'ast>, usize>;

pub struct Resolver<'ast> {
    locals: Stack<Scope<'ast>>,
    current_initializer: Option<Identifier<'ast>>,
    in_function: bool,
    loop_stack: Stack<While<'ast>>,
    loop_count: usize,

    pub resolutions: Resolutions<'ast>,
    pub warnings: Vec<Box<dyn CompilerWarning>>,
}

#[derive(Debug)]
struct Variable<'ast> {
    pub state: VariableState,
    pub name: Identifier<'ast>,
}
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum VariableState {
    Declared,
    Defined,
    Read,
}

struct While<'a> {
    name: Option<Identifier<'a>>,
    index: usize,
}

macro_rules! current_scope {
    ($dst:expr) => {
        $dst.locals
            .last_mut()
            .expect("COMPILER BUG: Scopes should contain at least global scope.")
    };
}

impl<'ast> Resolver<'ast> {
    pub fn new() -> Resolver<'ast> {
        Resolver {
            resolutions: HashMap::new(),
            current_initializer: None,
            locals: vec![HashMap::new()],
            in_function: false,
            warnings: vec![],
            loop_stack: vec![],
            loop_count: 0,
        }
    }
}
impl<'ast> Resolver<'ast> {
    // Resolving
    pub fn resolve_statement(
        &mut self,
        statement: &'ast Statement<'ast>,
    ) -> Result<(), Vec<ResolverError>> {
        match &statement.kind {
            StatementKind::ExpressionStatement(expression) => {
                self.resolve_expression(expression).map_err(|e| vec![e])?;
            }
            StatementKind::FunctionDeclaration(decl) => {
                // TODO: Ensure that in_function is returned to false in cases where function exits early (? operator)
                self.declare(&decl.name);
                self.define(&decl.name);
                self.enter_scope();
                for param in decl.parameters {
                    self.declare(&param.name);
                    self.define(&param.name);
                }
                self.in_function = true;
                let errors = self.resolve_statements(decl.body);
                self.in_function = false;
                self.exit_scope();
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
                ty: _ty,
            }) => {
                self.current_initializer = Some(name.clone());
                // In variable shadowing, user might want to read from variable with the same name.
                // a := 10;
                // a := a + 15;
                // That's why initializer is resolved prior to declaring variable
                if let Some(init) = initializer {
                    self.resolve_expression(init).map_err(|e| vec![e])?;
                }

                self.current_initializer = None;
                self.declare(name);
                self.define(name);
            }
            StatementKind::Return(val) => {
                if !self.in_function {
                    return Err(vec![ResolverError {
                        kind: ResolverErrorKind::TopLevelReturn,
                        loc: statement.loc.clone(),
                    }]);
                }
                if let Some(val) = val {
                    self.resolve_expression(val).map_err(|e| vec![e])?;
                }
            }
            StatementKind::Extern(ExternalFunction {
                name,
                kind: _kind,
                parameters: _parameters,
                return_type: _return_type,
            }) => {
                self.declare(name);
                self.define(name);
            }
            StatementKind::If {
                condition,
                then,
                r#else,
            } => {
                self.resolve_expression(condition).map_err(|e| vec![e])?;
                self.resolve_statement(then)?;
                if let Some(r#else) = r#else {
                    self.resolve_statement(r#else)?;
                }
            }
            StatementKind::While {
                condition,
                body,
                name,
                id,
            } => {
                self.resolve_expression(condition).map_err(|e| vec![e])?;
                if let Some(target) = name
                    && let Some(original_name) = self
                        .loop_stack
                        .iter()
                        .rev()
                        .filter_map(|e| e.name.clone())
                        .find(|n| n.name == target.name)
                {
                    return Err(vec![ResolverError {
                        loc: statement.loc.clone(),
                        kind: ResolverErrorKind::LoopLabelRedefinition {
                            original_name: IdentifierBox::from_borrowed(&original_name),
                        },
                    }]);
                }
                let index = self.allocate_loop_index();
                id.set(index);
                self.loop_stack.push(While {
                    name: name.clone(),
                    index,
                });
                self.resolve_statement(body)?;
                self.loop_stack.pop();
            }
            StatementKind::Break { name, id } => {
                if self.loop_stack.is_empty() {
                    return Err(vec![ResolverError {
                        kind: ResolverErrorKind::BreakOutsideOfLoop,
                        loc: statement.loc.clone(),
                    }]);
                }
                id.set(self.resolve_loop_label(name).map_err(|e| vec![e])?);
            }
            StatementKind::Continue { name, id } => {
                if self.loop_stack.is_empty() {
                    return Err(vec![ResolverError {
                        kind: ResolverErrorKind::ContinueOutsideOfLoop,
                        loc: statement.loc.clone(),
                    }]);
                }
                id.set(self.resolve_loop_label(name).map_err(|e| vec![e])?);
            }
        }
        Ok(())
    }

    pub fn resolve_statements(
        &mut self,
        statements: &[&'ast Statement<'ast>],
    ) -> Vec<ResolverError> {
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
                self.resolve_local_variable(expression, read)?;
                let depth = self
                    .resolutions
                    .get(expression)
                    .expect("COMPILER BUG: resolve_local_variable failed");
                let var = self.locals.get_at_mut(&read.name, *depth);
                var.state = VariableState::Read;
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
    fn declare(&mut self, name: &Identifier<'ast>) {
        let current_scope = current_scope!(self);
        // Case 1 (Shadowing): there are already variables with that name defined in the scope.
        if let Some(declaration) = current_scope.get_mut(name.name) {
            if declaration.state < VariableState::Read {
                self.warnings.push(
                    ResolverWarning {
                        loc: name.location.clone(),
                        kind: ResolverWarningKind::AccidentalShadowing {
                            variable_declaration: IdentifierBox::from_borrowed(&declaration.name),
                        },
                    }
                    .into_box(),
                );
            }
            *declaration = Variable {
                state: VariableState::Declared,
                name: name.clone(),
            };
            return;
        }
        // Case 2. There are no variables with that name defined is the scope
        current_scope.insert(
            name.name,
            Variable {
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

    fn resolve_loop_label(&self, name: &Option<Identifier<'ast>>) -> Result<usize, ResolverError> {
        if let Some(name) = name {
            if let Some((l, _)) = self
                .loop_stack
                .iter()
                .rev()
                .filter_map(|e| e.name.clone().map(|name| (e, name)))
                .find(|(_, loop_name)| name.name == loop_name.name)
            {
                return Ok(l.index);
            }
            return Err(ResolverError {
                loc: name.location.clone(),
                kind: ResolverErrorKind::UndeclaredLoopLabel {
                    name: IdentifierBox::from_borrowed(name),
                },
            });
        }

        Ok(self
            .loop_stack
            .last()
            .expect("Compiler bug: resolve_loop_label should never be called outside of loop")
            .index)
    }

    fn allocate_loop_index(&mut self) -> usize {
        let index = self.loop_count;
        self.loop_count += 1;
        index
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
pub struct ResolverError {
    pub loc: SourceLocation,
    pub kind: ResolverErrorKind,
}
pub struct ResolverWarning {
    pub loc: SourceLocation,
    pub kind: ResolverWarningKind,
}
pub enum ResolverWarningKind {
    AccidentalShadowing { variable_declaration: IdentifierBox },
}

impl CompilerWarning for ResolverWarning {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn message(&self) -> String {
        match &self.kind {
            ResolverWarningKind::AccidentalShadowing {
                variable_declaration,
            } => {
                format!(
                    "variable `{variable_declaration}` is being shadowed without being read. Did you mean to mutate it?"
                )
            }
        }
    }

    fn note(&self) -> Option<String> {
        match &self.kind {
            ResolverWarningKind::AccidentalShadowing {
                variable_declaration: decl,
            } => Some(format!(
                "`{}` is declared here: {}",
                decl.name, decl.location
            )),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ResolverErrorKind {
    UndeclaredIdentifier { usage: IdentifierBox },
    ReadingFromInitializer { read: IdentifierBox },
    TopLevelReturn,
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    LoopLabelRedefinition { original_name: IdentifierBox },
    UndeclaredLoopLabel { name: IdentifierBox },
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
            ResolverErrorKind::TopLevelReturn => "could not return from top-level".into(),
            ResolverErrorKind::BreakOutsideOfLoop => "'break' statement outside of loop".into(),
            ResolverErrorKind::ContinueOutsideOfLoop => {
                "'continue' statement outside of loop".into()
            }
            ResolverErrorKind::LoopLabelRedefinition { original_name } => {
                format!("redefinition of loop label '{original_name}'")
            }
            ResolverErrorKind::UndeclaredLoopLabel { name } => {
                format!("undeclared loop label '{name}'")
            }
        }
    }

    fn note(&self) -> Option<String> {
        match &self.kind {
            ResolverErrorKind::UndeclaredIdentifier { .. } => None,
            ResolverErrorKind::ReadingFromInitializer { .. } => None,
            ResolverErrorKind::TopLevelReturn => None,
            ResolverErrorKind::BreakOutsideOfLoop => None,
            ResolverErrorKind::ContinueOutsideOfLoop => None,
            ResolverErrorKind::LoopLabelRedefinition { original_name } => Some(format!(
                "loop label {original_name} is first defined here: {}",
                original_name.location
            )),
            ResolverErrorKind::UndeclaredLoopLabel { .. } => None,
        }
    }
}
