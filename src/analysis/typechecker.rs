use crate::analysis::r#type::{FunctionType, Type};
use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::VariableDeclaration;
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{CompilerError, SourceLocation, Stack};
use bumpalo::Bump;
use std::cell::Cell;
use std::collections::HashMap;
use crate::analysis::get_at::GetAt;
use crate::analysis::resolver::Resolutions;

struct Typechecker<'ast> {
    current_function_type: Option<*const Cell<Type<'ast>>>,
    bump: &'ast Bump,

    pub resolutions: Resolutions<'ast>,
    locals: Stack<HashMap<&'ast str, Variable<'ast>>>,
}

struct Variable<'a> {
    ty: Cell<Type<'a>>,
}

impl<'ast> Typechecker<'ast> {
    pub fn typecheck_expression(
        &mut self,
        expression: &'ast Expression<'ast>,
    ) -> Result<(), TypeError> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind } => {
                self.typecheck_expression(left)?;
                self.typecheck_expression(right)?;
                if left.ty != right.ty {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo,
                        loc: expression.loc.clone(),
                    });
                }
                expression.ty.set(match kind.is_logical() {
                    true => Type::Bool,
                    false => left.ty.get(),
                });
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
                expression.ty.set(var.ty.get());
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                self.typecheck_expression(callee)?;
                for arg in *arguments {
                    self.typecheck_expression(arg)?;
                }
                if let Type::Function(FunctionType {
                    return_type,
                    parameters,
                }) = callee.ty.get()
                {
                    if parameters.len() != arguments.len() {
                        return Err(TypeError {
                            loc: expression.loc.clone(),
                            kind: TypeErrorKind::InvalidArity {
                                expected_arguments: parameters.len(),
                                actual_arguments: arguments.len(),
                                function_name: None, // TODO: Get function name.
                            },
                        });
                    }
                    for (expected, arg) in parameters.iter().zip(arguments.iter()) {
                        let got = arg.ty.get();
                        if *expected != got {
                            return Err(TypeError {
                                loc: arg.loc.clone(),
                                kind: TypeErrorKind::MismatchedArgumentType {
                                    function_location: None,
                                    function_name: None, // TODO: Get function name and location
                                    expected_type: expected.to_string(),
                                    actual_type: got.to_string(),
                                },
                            });
                        }
                    }
                    expression.ty.set(*return_type);
                } else {
                    todo!("Proper error handling")
                }
            }
        }
        Ok(())
    }

    pub fn typecheck_statement(
        &mut self,
        statement: &'ast Statement<'ast>,
    ) -> Result<(), TypeError> {
        match &statement.kind {
            StatementKind::ExpressionStatement(expression) => {
                self.typecheck_expression(expression)?
            }

            StatementKind::FunctionDeclaration(_) => {
                todo!("Typecheck functions not yet implemented")
                // TODO: If there are no return in function and no return type is specified, treat it as returning void
            }
            StatementKind::VariableDeclaration(VariableDeclaration {
                name,
                initializer,
                ty,
            }) => {
                // let return_type = self.bump.alloc(decl.return_type.get());
                // let b = self.bump;
                // let fn_type = FunctionType {
                //     return_type,
                //     parameters: decl
                //         .parameters
                //         .iter()
                //         .map(|p| p.ty)
                //         .collect_in::<BumpVec<_>>(b)
                //         .into_bump_slice(),
                // };
                let variable_type = ty;
                let mut initializer_type = Type::Unknown;
                if let Some(init) = initializer {
                    self.typecheck_expression(init)?;
                    initializer_type = init.ty.get();
                }

                if variable_type.get() == Type::Unknown {
                    variable_type.set(initializer_type);
                } else if variable_type.get() != initializer_type {
                    todo!("For now, initializer type must be the same with declaration type")
                }
            }
            StatementKind::Block(statements) => {
                for statement in *statements {
                    self.typecheck_statement(statement)?;
                }
            }
            StatementKind::Return(val) => {
                if let Some(val) = val {
                    self.typecheck_expression(val)?;
                    let current_fn = if let Some(current_fn) = self.current_function_type {
                        current_fn
                    } else {
                        unreachable!("this should be handled by resolver.");
                    };

                    let current_ty = unsafe { (*current_fn).get() };

                    if let Type::Function(mut fn_type) = current_ty {
                        if let Type::Unknown = fn_type.return_type {
                            fn_type.return_type = self.bump.alloc(val.ty.get());
                        } else if *fn_type.return_type != val.ty.get() {
                            todo!("TODO: Proper error handling")
                        }
                        unsafe {
                            (*current_fn).set(Type::Function(fn_type));
                        }
                    } else {
                        panic!("Caller of resolve_statement() is broken.")
                    }
                }
            }
            StatementKind::Extern(_) => {
                todo!("Typecheck extern")
            }
            StatementKind::If {
                condition,
                then,
                r#else,
            } => {
                self.typecheck_expression(condition)?;
                if condition.ty.get() != Type::Bool {
                    return Err(TypeError {
                        loc: statement.loc.clone(),
                        kind: TypeErrorKind::MismatchedIfConditionType,
                    });
                }
                self.typecheck_statement(then)?;
                if let Some(r#else) = r#else {
                    self.typecheck_statement(r#else)?;
                }
            }
        }

        Ok(())
    }
}
pub struct TypeError {
    pub loc: SourceLocation,
    pub kind: TypeErrorKind,
}
impl CompilerError for TypeError {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeErrorKind::Todo => todo!(),
            TypeErrorKind::MismatchedArgumentType {
                function_location: _function_location,
                function_name: _function_name,
                expected_type,
                actual_type,
            } => format!(
                "Expected `{}`, found `{}`, Mismatched argument type",
                expected_type, actual_type
            ),
            //
            //
            TypeErrorKind::InvalidArity {
                expected_arguments,
                actual_arguments,
                function_name,
            } => {
                assert_ne!(actual_arguments, expected_arguments);
                let s = if expected_arguments < actual_arguments {
                    "too many"
                } else {
                    "not enough"
                };

                let function_name = if let Some(function_name) = function_name {
                    format!(" `{}`", function_name)
                } else {
                    "".to_string()
                };
                format!(
                    "{s} arguments were provided. Function{function_name} expects {expected_arguments} arguments, but got {actual_arguments}"
                )
            }
            TypeErrorKind::MismatchedIfConditionType => {
                "If condition should evaluate to `bool` type".to_string()
            }
        }
    }

    fn note(&self) -> Option<String> {
        match &self.kind {
            TypeErrorKind::Todo => None,
            TypeErrorKind::MismatchedArgumentType {
                function_location,
                function_name,
                expected_type: _expected_type,
                actual_type: _actual_type,
            } => {
                if let Some(function_location) = function_location {
                    return Some(format!(
                        "{}is defined here: {loc}",
                        if let Some(function_name) = function_name {
                            format!("`{}` ", function_name)
                        } else {
                            "function ".to_owned()
                        },
                        loc = function_location,
                    ));
                }
                None
            }
            TypeErrorKind::InvalidArity { .. } => None,
            TypeErrorKind::MismatchedIfConditionType => None,
        }
    }
}
pub enum TypeErrorKind {
    Todo,
    MismatchedArgumentType {
        function_location: Option<SourceLocation>,
        function_name: Option<Box<str>>,
        expected_type: String,
        actual_type: String,
    },
    InvalidArity {
        expected_arguments: usize,
        actual_arguments: usize,
        function_name: Option<String>,
    },
    MismatchedIfConditionType,
}
