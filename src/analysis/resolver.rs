use crate::analysis::Stack;
use crate::ast::expression::{Expression, ExpressionKind, RefId, SymId};
use crate::ast::statement::{ExternalFunction, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::errors_warnings::{CompilerError, CompilerWarning};
use crate::common::identifier::{Identifier, IdentifierBox};
use crate::common::{IntoBox, SourceLocation};
use std::collections::HashMap;
use std::fmt::Write;
use bumpalo::Bump;

pub struct Resolver<'ast, 'errors> {
    scopes: Stack<HashMap<&'ast str, Variable<'ast>>>,
    current_initializer: Option<Identifier<'ast>>,
    in_function: bool,
    loop_stack: Stack<While<'ast>>,
    loop_count: usize,

    errors: &'errors Bump,

    pub warnings: Vec<Box<dyn CompilerWarning>>,
}

#[derive(Debug)]
struct Variable<'ast> {
    pub state: VariableState,
    pub name: Identifier<'ast>,
    pub sym_id: SymId,
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
        $dst.scopes
            .last_mut()
            .expect("COMPILER BUG: Scopes should contain at least global scope.")
    };
}

impl<'ast, 'errors> Resolver<'ast, 'errors> {
    pub fn new(errors: &'errors Bump) -> Resolver<'ast, 'errors> {
        Resolver {
            current_initializer: None,
            scopes: vec![HashMap::new()],
            in_function: false,
            warnings: vec![],
            loop_stack: vec![],
            loop_count: 0,
            errors,
        }
    }
}
impl<'ast, 'types, 'errors> Resolver<'ast, 'errors> {
    // Resolving
    pub fn resolve_statement(
        &mut self,
        statement: &'ast Statement<'ast, 'types>,
    ) -> Result<(), Vec<ResolverError<'errors>>> {
        match &statement.kind {
            StatementKind::ExpressionStatement(expression) => {
                self.resolve_expression(expression).map_err(|e| vec![e])?;
            }
            StatementKind::FunctionDeclaration(decl, id) => {
                // TODO: Ensure that in_function is returned to false in cases where function exits early (? operator)
                self.declare(&decl.name, *id);
                self.define(&decl.name);
                self.enter_scope();
                for param in decl.parameters {
                    self.declare(&param.name, param.id);
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
            StatementKind::VariableDeclaration(
                VariableDeclaration {
                    name,
                    initializer,
                    ty: _ty,
                },
                id,
            ) => {
                self.current_initializer = Some(name.clone());
                // In variable shadowing, user might want to read from variable with the same name.
                // a := 10;
                // a := a + 15;
                // That's why initializer is resolved prior to declaring variable
                if let Some(init) = initializer {
                    self.resolve_expression(init).map_err(|e| vec![e])?;
                }

                self.current_initializer = None;
                self.declare(name, *id);
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
            StatementKind::Extern(
                ExternalFunction {
                    name,
                    kind: _kind,
                    parameters: _parameters,
                    return_type: _return_type,
                    is_variadic: _variadic,
                },
                id,
            ) => {
                self.declare(name, *id);
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
                        loc: target.location.clone(),
                        kind: ResolverErrorKind::LoopLabelRedefinition {
                            original_name: original_name.clone_into(self.errors),
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
        statements: &[&'ast Statement<'ast, 'types>],
    ) -> Vec<ResolverError<'errors>> {
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
    ) -> Result<(), ResolverError<'errors>> {
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
            ExpressionKind::VariableAccess(read, id) => {
                let depth = self.resolve_local_variable(id, read)?;
                let var = self.get_at_mut(&read.name, depth);
                var.state = VariableState::Read;
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                self.resolve_expression(callee)?;
                for arg in *arguments {
                    self.resolve_expression(unsafe {
                        arg.as_ref()
                            .expect("Expression::FunctionCall: null argument")
                    })?;
                }
            }
            ExpressionKind::Literal(_) => { /* nothing */ }
        }
        Ok(())
    }
    fn declare(&mut self, name: &Identifier<'ast>, id: SymId) {
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
                sym_id: id,
            };
            return;
        }
        // Case 2. There are no variables with that name defined is the scope
        current_scope.insert(
            name.name,
            Variable {
                state: VariableState::Declared,
                name: name.clone(),
                sym_id: id,
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
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
    fn resolve_local_variable(
        &mut self,
        ref_id: &RefId,
        var: &Identifier<'ast>,
        // Returns depth
    ) -> Result<usize, ResolverError<'errors>> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(e) = scope.get(var.name) {
                let id = e.sym_id;
                ref_id.set(id);
                return Ok(i);
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
                    read: var.clone_into(self.errors),
                },
            });
        }

        Err(ResolverError {
            loc: var.location.clone(),
            kind: ResolverErrorKind::UndeclaredIdentifier {
                usage: var.clone_into(self.errors),
            },
        })
    }

    fn resolve_loop_label(&self, name: &Option<Identifier<'ast>>) -> Result<usize, ResolverError<'errors>> {
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
                    name: name.clone_into(self.errors),
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

    fn get_at_mut(&mut self, name: &'ast str, depth: usize) -> &mut Variable<'ast> {
        let scope_index = self.scopes.len() - 1 - depth;
        let scope = &mut self.scopes[scope_index];
        scope
            .get_mut(name)
            .unwrap_or_else(|| panic!("RESOLVER BUG: No key `{name}` in the scope."))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ResolverError<'errors> {
    pub loc: SourceLocation,
    pub kind: ResolverErrorKind<'errors>,
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
pub enum ResolverErrorKind<'errors> {
    UndeclaredIdentifier { usage: Identifier<'errors> },
    ReadingFromInitializer { read: Identifier<'errors> },
    TopLevelReturn,
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    LoopLabelRedefinition { original_name: Identifier<'errors> },
    UndeclaredLoopLabel { name: Identifier<'errors> },
}

impl<'errors> CompilerError<()> for ResolverError<'errors> {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn display_message(&self, f: &mut impl Write, _ctx: ()) -> std::fmt::Result {
        match &self.kind {
            ResolverErrorKind::UndeclaredIdentifier { usage } => {
                write!(f, "Undeclared identifier `{}`", usage.name)?
            }

            ResolverErrorKind::ReadingFromInitializer { read } => write!(
                f,
                "Could not read `{n}` from it's own initializer",
                n = read.name
            )?,
            ResolverErrorKind::TopLevelReturn => write!(f, "could not return from top-level")?,
            ResolverErrorKind::BreakOutsideOfLoop => {
                write!(f, "'break' statement outside of loop")?
            }
            ResolverErrorKind::ContinueOutsideOfLoop => {
                write!(f, "'continue' statement outside of loop")?
            }
            ResolverErrorKind::LoopLabelRedefinition { original_name } => {
                write!(f, "redefinition of loop label '{original_name}'")?
            }
            ResolverErrorKind::UndeclaredLoopLabel { name } => {
                write!(f, "undeclared loop label '{name}'")?
            }
        };
        Ok(())
    }

    fn display_note(&self, f: &mut impl Write, _ctx: ()) -> std::fmt::Result {
        match &self.kind {
            ResolverErrorKind::UndeclaredIdentifier { .. } => Ok(()),
            ResolverErrorKind::ReadingFromInitializer { .. } => Ok(()),
            ResolverErrorKind::TopLevelReturn => Ok(()),
            ResolverErrorKind::BreakOutsideOfLoop => Ok(()),
            ResolverErrorKind::ContinueOutsideOfLoop => Ok(()),
            ResolverErrorKind::LoopLabelRedefinition { original_name } => write!(f,
                "loop label {original_name} is first defined here: {}",
                original_name.location
            ),
            ResolverErrorKind::UndeclaredLoopLabel { .. } => Ok(()),
        }
    }
}
