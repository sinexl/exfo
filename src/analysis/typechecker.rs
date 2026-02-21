use crate::analysis::get_at::GetAt;
use crate::analysis::resolver::Resolutions;
use crate::analysis::r#type::{
    BasicType, DisplayType, FunctionType, PointerType, Type, TypeId, TypeIdCell,
};
use crate::analysis::type_context::TypeCtx;
use crate::ast::binop::BinopFamily;
use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{ExternalFunction, FunctionDeclaration, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::{BumpVec, CompilerError, SourceLocation, Stack};
use crate::{ast, debug_scopes};
use bumpalo::collections::CollectIn;
use std::collections::HashMap;

pub struct Typechecker<'ast, 'types> {
    current_function_type: Option<TypeId>,
    types: *mut TypeCtx<'types>,

    pub resolutions: Resolutions<'ast>,
    locals: Stack<HashMap<&'ast str, Variable>>,
}

#[derive(Debug)]
struct Variable {
    ty: TypeId,
}

impl<'ast, 'types> Typechecker<'ast, 'types> {
    pub fn new(types: *mut TypeCtx<'types>, resolutions: Resolutions<'ast>) -> Self {
        Self {
            current_function_type: None,
            types,
            resolutions,
            locals: vec![HashMap::new()],
        }
    }
}

impl<'ast, 'types> Typechecker<'ast, 'types> {
    pub fn types(&self) -> &'types TypeCtx<'types> {
        unsafe { self.types.as_ref().expect("ERROR: Type context is NULL") }
    }
    pub fn typecheck_expression(
        &mut self,
        expression: &'ast Expression<'ast>,
    ) -> Result<(), TypeError> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind } => {
                let kind = *kind;
                self.typecheck_expression(left)?;
                self.typecheck_expression(right)?;
                let error = Err(TypeError {
                    loc: expression.loc.clone(),
                    kind: TypeErrorKind::InvalidOperands(
                        kind,
                        DisplayType(left.ty.inner(), self.types())
                            .to_string()
                            .into_boxed_str(),
                        DisplayType(right.ty.inner(), self.types())
                            .to_string()
                            .into_boxed_str(),
                    ),
                });

                let left_id = left.ty.clone();
                let right_id = right.ty.clone();
                let left = left_id.get(self.types());
                let right = right_id.get(self.types());

                if (left.is_bool() || right.is_bool()) && kind.family() != BinopFamily::Logical {
                    return error;
                }
                let ty: TypeId = match kind.family() {
                    BinopFamily::Arithmetic => {
                        if left != right {
                            return Err(TypeError {
                                kind: TypeErrorKind::Todo("coercion".to_owned()), // TODO.
                                loc: expression.loc.clone(),
                            });
                        }
                        left_id.inner()
                    }
                    BinopFamily::Logical => {
                        if !left.is_bool() || (left != right) {
                            return error;
                        }
                        left_id.inner()
                    }
                    BinopFamily::Ordering => {
                        if left != right {
                            return Err(TypeError {
                                kind: TypeErrorKind::Todo("coercion".to_owned()), // TODO.
                                loc: expression.loc.clone(),
                            });
                        }
                        TypeId::from_basic(BasicType::Bool)
                    }
                };
                expression.ty.set(ty);
            }
            ExpressionKind::Unary { item, operator } => {
                self.typecheck_expression(item)?;
                let ty = match operator {
                    UnaryKind::Negation => item.ty.inner(),
                    UnaryKind::Dereferencing => {
                        use Type::*;

                        let underlying = item.ty.get(self.types());
                        match underlying {
                            Basic(_) | Function(_) | UserDefined(_) => {
                                return Err(TypeError {
                                    loc: expression.loc.clone(),
                                    kind: TypeErrorKind::DereferencingNonPointer {
                                        actual_type: DisplayType(item.ty.inner(), self.types())
                                            .to_string()
                                            .into_boxed_str(),
                                    },
                                });
                            }
                            Pointer(PointerType { inner }) => *inner,
                        }
                    }
                    UnaryKind::AddressOf => {
                        if !item.kind.lvalue() {
                            return Err(TypeError {
                                loc: expression.loc.clone(),
                                kind: TypeErrorKind::RvalueAddressOf(
                                    DisplayType(item.ty.inner(), self.types())
                                        .to_string()
                                        .into_boxed_str(),
                                ),
                            });
                        }
                        unsafe { (*self.types).monomorph_or_get_pointer(item.ty.inner()) }
                    }
                };
                expression.ty.set(ty);
            }
            ExpressionKind::Assignment { target, value } => {
                self.typecheck_expression(target)?;
                self.typecheck_expression(value)?;
                if target.ty != value.ty {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo("coercion".to_owned()), // TODO:
                        loc: expression.loc.clone(),
                    });
                }
                expression.ty.set(target.ty.inner());
            }
            ExpressionKind::Literal(_) => {
                assert_ne!(
                    expression.ty.inner(),
                    TypeId::Unknown,
                    "Compiler bug: literals should always have a type."
                );
            }
            ExpressionKind::VariableAccess(n) => {
                let depth = self
                    .resolutions
                    .get(expression)
                    .expect("Compiler bug: resolution failed");
                let var = self.locals.get_at(&n.name, *depth);

                expression.ty.set(var.ty);
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                self.typecheck_expression(callee)?;
                for arg in *arguments {
                    self.typecheck_expression(unsafe {
                        arg.as_mut()
                            .expect("Expression::FunctionCall: null argument")
                    })?;
                }
                if let Type::Function(FunctionType {
                    return_type,
                    parameters,
                    is_variadic,
                }) = callee.ty.get(self.types())
                {
                    let arity_error = TypeError {
                        loc: if let Some(last) = arguments.last() {
                            unsafe { (**last).loc.clone() }
                        } else {
                            expression.loc.clone()
                        },
                        kind: TypeErrorKind::InvalidArity {
                            expected_arguments: parameters.len(),
                            actual_arguments: arguments.len(),
                            function_name: None, // TODO: Get function name.
                        },
                    };
                    if arguments.len() < parameters.len() {
                        return Err(arity_error);
                    }
                    if !is_variadic && arguments.len() != parameters.len() {
                        return Err(arity_error);
                    }
                    for (expected, arg) in parameters.iter().zip(arguments.iter()) {
                        let arg = unsafe { arg.as_ref().expect("FunctionCall: null argument") };
                        // TODO: for now, we are comparing TypeIds themselves. Maybe its worth comparing Types themselves, because there would be less
                        //  restrictions for implementing type coercion, but not sure.
                        let got = arg.ty.clone(); // .get();
                        let expected = expected.clone();
                        if expected.inner() != got.clone().inner() {
                            return Err(TypeError {
                                loc: arg.loc.clone(),
                                kind: TypeErrorKind::MismatchedArgumentType {
                                    function_location: None,
                                    function_name: None, // TODO: Get function name and location
                                    expected_type: Box::from(
                                        DisplayType(expected.inner(), self.types())
                                            .to_string()
                                            .as_str(),
                                    ),
                                    actual_type: Box::from(
                                        DisplayType(got.inner(), self.types()).to_string().as_str(),
                                    ),
                                },
                            });
                        }
                    }
                    let return_type = return_type.clone();
                    expression.ty.set(return_type.inner());
                } else {
                    todo!("Proper error handling")
                }
            }
        }
        Ok(())
    }

    pub fn typecheck_statements(
        &mut self,
        statements: &'ast [&'ast Statement<'ast, 'types>],
    ) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        for statement in statements {
            if let Err(e) = self.typecheck_statement(statement) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            return Ok(());
        }
        Err(errors)
    }
    pub fn typecheck_statement(
        &mut self,
        statement: &'ast Statement<'ast, 'types>,
    ) -> Result<(), TypeError> {
        match &statement.kind {
            StatementKind::ExpressionStatement(expression) => {
                self.typecheck_expression(expression)?
            }

            StatementKind::FunctionDeclaration(FunctionDeclaration {
                name,
                parameters,
                body,
                return_type,
            }) => {
                // if return_type.get() == Type::Unknown {
                //     let actual = self.try_infer_type(body)?;
                //     return_type.set(actual);
                // }
                let b = self.types().bump();
                let fn_type = FunctionType {
                    return_type: return_type.clone(),
                    parameters: parameters
                        .iter()
                        .map(|p| {
                            assert_ne!(
                                p.ty.inner(),
                                TypeId::Unknown,
                                "COMPILER BUG: Function parameter type id could not be Unknown"
                            );
                            p.ty.clone()
                        })
                        .collect_in::<BumpVec<_>>(b)
                        .into_bump_slice(),
                    is_variadic: false,
                };
                let ty = unsafe { (*self.types).allocate(Type::Function(fn_type)) };
                self.locals
                    .last_mut()
                    .expect("Compiler bug: there should be at least a global scope")
                    .insert(name.name, Variable { ty });
                self.current_function_type =
                    Some(self.locals.last().unwrap().get(name.name).unwrap().ty);
                self.locals.push(HashMap::new());
                for param in *parameters {
                    self.locals.last_mut().unwrap().insert(
                        param.name.name,
                        Variable {
                            ty: param.ty.inner(),
                        },
                    );
                }
                for statement in body.iter() {
                    self.typecheck_statement(statement)?;
                }
                self.current_function_type = None;

                self.locals.pop();
                // TODO: If there are no return in function and no return type is specified, treat it as returning void
            }
            StatementKind::VariableDeclaration(VariableDeclaration {
                name,
                initializer,
                ty,
            }) => {
                let variable_type = ty;
                let mut initializer_type = TypeId::Unknown;
                if let Some(init) = initializer {
                    self.typecheck_expression(init)?;
                    initializer_type = init.ty.inner();
                }

                if variable_type.inner() == TypeId::Unknown {
                    variable_type.set(initializer_type);
                } else if variable_type.inner() != initializer_type {
                    return Err(TypeError {
                        loc: statement.loc.clone(),
                        kind: TypeErrorKind::Todo("Coercions are not implemented yet".to_owned()), // TODO
                    });
                }
                self.locals
                    .last_mut()
                    .expect("Compiler bug: there should be at least a global scope")
                    .insert(name.name, Variable { ty: ty.inner() });
            }
            StatementKind::Block(statements) => {
                self.locals.push(HashMap::new());
                for statement in *statements {
                    self.typecheck_statement(statement)?;
                }
                self.locals.pop();
            }
            StatementKind::Return(val) => {
                if let Some(val) = val {
                    self.typecheck_expression(val)?;
                    let current_fn = if let Some(current_fn) = self.current_function_type {
                        current_fn
                    } else {
                        unreachable!(
                            "Compiler bug: Top-level returns should be handled by Resolver."
                        )
                    };

                    if let Type::Function(fn_type) = current_fn.get_mut(self.types()) {
                        if let TypeId::Unknown = fn_type.return_type.inner() {
                            fn_type.return_type.set(val.ty.inner());
                        } else if fn_type.return_type != val.ty {
                            todo!("TODO: Proper error handling")
                        }
                    } else {
                        panic!("Caller of resolve_statement() is broken.")
                    }
                }
            }
            StatementKind::Extern(ExternalFunction {
                name,
                kind: _kind,
                parameters,
                return_type,
                is_variadic,
            }) => {
                self.locals
                    .last_mut()
                    .expect("Compiler bug: there should be at least a global scope.")
                    .insert(
                        name.name,
                        Variable {
                            ty: unsafe {
                                (*self.types).allocate(Type::Function(FunctionType {
                                    return_type: return_type.clone(),
                                    parameters,
                                    is_variadic: *is_variadic,
                                }))
                            },
                        },
                    );
            }
            StatementKind::If {
                condition,
                then,
                r#else,
            } => {
                self.typecheck_expression(condition)?;
                if condition.ty != TypeIdCell::from_basic(BasicType::Bool) {
                    return Err(TypeError {
                        loc: condition.loc.clone(),
                        kind: TypeErrorKind::MismatchedConditionType("if"),
                    });
                }
                self.typecheck_statement(then)?;
                if let Some(r#else) = r#else {
                    self.typecheck_statement(r#else)?;
                }
            }
            StatementKind::While {
                condition,
                body,
                name: _,
                id: _,
            } => {
                self.typecheck_expression(condition)?;
                if condition.ty != TypeIdCell::from_basic(BasicType::Bool) {
                    return Err(TypeError {
                        loc: condition.loc.clone(),
                        kind: TypeErrorKind::MismatchedConditionType("while"),
                    });
                }
                self.typecheck_statement(body)?;
            }
            StatementKind::Break { .. } | StatementKind::Continue { .. } => {}
        }

        Ok(())
    }

    pub fn try_infer_type(
        &mut self,
        body: &'ast [&'ast Statement<'ast, 'types>],
    ) -> Result<Type<'ast>, TypeError> {
        todo!()
    }
}
pub struct TypeError {
    pub loc: SourceLocation,
    pub kind: TypeErrorKind,
}

pub enum TypeErrorKind {
    Todo(String),
    MismatchedArgumentType {
        function_location: Option<SourceLocation>,
        function_name: Option<Box<str>>,
        expected_type: Box<str>,
        actual_type: Box<str>,
    },
    InvalidArity {
        expected_arguments: usize,
        actual_arguments: usize,
        function_name: Option<String>,
    },
    MismatchedConditionType(&'static str),
    CouldntInferFunctionReturnType,
    /// This error kind is needed to ignore malformed code that should be marked as erroneous by previous passes.
    /// For example, Top-level returns are handled by resolver, so, if we meet such in typechecker, we can skip it
    FromPrevious,
    InvalidOperands(ast::binop::BinopKind, Box<str>, Box<str>),
    DereferencingNonPointer {
        actual_type: Box<str>,
    },

    RvalueAddressOf(Box<str>),
}
impl CompilerError for TypeError {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn message(&self) -> String {
        match &self.kind {
            TypeErrorKind::Todo(message) => format!("Not implemented: {}", message),
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
            TypeErrorKind::MismatchedConditionType(name) => {
                format!("{name} condition should evaluate to `bool` type")
            }
            TypeErrorKind::CouldntInferFunctionReturnType => {
                "Could not infer function return type".to_string()
            }
            TypeErrorKind::FromPrevious => "".to_string(),
            TypeErrorKind::InvalidOperands(op, a, b) => format!(
                "Invalid operands to '{op}' operation ({a}, {b})",
                op = op.operator()
            ),
            TypeErrorKind::DereferencingNonPointer { actual_type } => {
                format!("Could not dereference `{}`", actual_type)
            }
            TypeErrorKind::RvalueAddressOf(typename) => {
                format!("Could not take an address of rvalue of type `{typename}`")
            }
        }
    }

    fn note(&self) -> Option<String> {
        match &self.kind {
            TypeErrorKind::Todo(_) => None,
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
            TypeErrorKind::MismatchedConditionType(_) => None,
            TypeErrorKind::CouldntInferFunctionReturnType => None,
            TypeErrorKind::FromPrevious => None,
            TypeErrorKind::InvalidOperands(_, _, _) => None,
            TypeErrorKind::DereferencingNonPointer { .. } => None,
            TypeErrorKind::RvalueAddressOf(_) => None,
        }
    }
}

fn print_type<'types>(item: TypeId, types: &'types TypeCtx<'types>) {
    let ty = item.get(types);
    dbg!(&ty);
}
