use crate::analysis::r#type::{
    BasicType, DisplayType, FunctionType, PointerType, Type, TypeId, TypeIdCell,
};
use crate::analysis::type_system::type_context::TypeCtx;
use crate::ast;
use crate::ast::binop::BinopKind;
use crate::ast::expression::{Expression, ExpressionKind, SymId, UnaryKind};
use crate::ast::statement::{ExternalFunction, FunctionDeclaration, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::errors_warnings::CompilerError;
use crate::common::symbol_table::{CompilerEntity, SymbolTable, Transform};
use crate::common::{BumpVec, SourceLocation};
use crate::compiling::compiler;
use bumpalo::collections::CollectIn;
use bumpalo::Bump;
use std::fmt::Write;

pub struct Typechecker<'types> {
    current_function_type: Option<TypeId>,
    types: *mut TypeCtx<'types>,
    bump: &'types Bump,

    pub symbols: SymbolTable<TypedEntity>,
    binary_operators: BumpVec<'types, BinaryOperator>,
    unary_operators: BumpVec<'types, UnaryOperator>,
}

#[derive(Debug, Clone, Copy)]
pub struct TypedEntity {
    ty: TypeId,
}

impl CompilerEntity for TypedEntity {}
impl Transform<compiler::Entity> for SymbolTable<TypedEntity> {
    fn transform(&self) -> SymbolTable<compiler::Entity> {
        let mut result = SymbolTable::new(self.size());
        for (i, var) in self.inner.iter().enumerate() {
            assert_ne!(var.ty, TypeId::Unknown);
            result.insert(
                SymId(i),
                compiler::Entity {
                    ty: var.ty,
                    kind: None,
                },
            )
        }
        result
    }
}

impl<'types> Typechecker<'types> {
    pub fn new(bump: &'types Bump, types: *mut TypeCtx<'types>, symbols_count: usize) -> Self {
        let mut res = Self {
            current_function_type: None,
            types,
            bump,
            symbols: SymbolTable::new(symbols_count),
            binary_operators: BumpVec::new_in(bump),
            unary_operators: BumpVec::new_in(bump),
        };
        Self::fill_operators(&mut res);
        res
    }
    pub fn fill_operators(&mut self) {
        use BasicType::*;
        use BinopKind::*;
        use UnaryKind::*;
        use ast::binop::constants;

        let int = Int64.id();
        let bool = Bool.id();
        let type_commutative = true;
        for kind in constants::ARITHMETIC_BINOPS.iter().copied() {
            self.binary_operators.push(BinaryOperator {
                kind,
                x: int,
                y: int,
                result: int,
                type_commutative,
            });
        }
        for kind in constants::COMPARISON_BINOPS.iter().copied() {
            self.binary_operators.push(BinaryOperator {
                kind,
                x: int,
                y: int,
                result: bool,
                type_commutative,
            });
        }

        for kind in constants::LOGICAL_BINOPS
            .iter()
            .chain(&[Equality, Inequality])
            .copied()
        {
            self.binary_operators.push(BinaryOperator {
                kind,
                x: bool,
                y: bool,
                result: bool,
                type_commutative,
            });
        }

        // Unary operations
        self.unary_operators.push(UnaryOperator {
            kind: Negation,
            x: int,
            result: int,
        });
        self.unary_operators.push(UnaryOperator {
            kind: Not,
            x: bool,
            result: bool,
        });
    }
}

struct BinaryOperator {
    // result = f(x, y)
    kind: BinopKind,
    x: TypeId,
    y: TypeId,
    result: TypeId,
    type_commutative: bool,
}

struct UnaryOperator {
    // result = f(x)
    kind: UnaryKind,
    x: TypeId,
    result: TypeId,
}

impl Eq for BinaryOperator {}

impl Eq for UnaryOperator {}
impl PartialEq for UnaryOperator {
    fn eq(&self, other: &Self) -> bool {
        let UnaryOperator {
            kind: k1,
            x: i1,
            result: r1,
        } = self;
        let UnaryOperator {
            kind: k2,
            x: i2,
            result: r2,
        } = other;
        k1 == k2 && i1 == i2 && r1 == r2
    }
}
impl PartialEq for BinaryOperator {
    fn eq(&self, other: &Self) -> bool {
        let BinaryOperator {
            kind: k1,
            x: x1,
            y: y1,
            result: r1,
            type_commutative: commute1,
        } = self;
        let BinaryOperator {
            kind: k2,
            x: x2,
            y: y2,
            result: r2,
            type_commutative: commute2,
        } = other;
        if k1 != k2 || r1 != r2 || commute1 != commute2 {
            return false;
        }

        if *commute1 {
            (x1 == x2 && y1 == y2) || (x1 == y2 && y1 == x2)
        } else {
            x1 == x2 && y1 == y2
        }
    }
}

impl<'ast, 'types> Typechecker<'types> {
    pub fn types(&self) -> &'types TypeCtx<'types> {
        unsafe { self.types.as_ref().expect("ERROR: Type context is NULL") }
    }
    pub fn typecheck_binop(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        kind: BinopKind,
        loc: SourceLocation,
    ) -> Result<TypeId, TypeError> {
        for BinaryOperator {
            kind: _,
            x,
            y,
            result,
            type_commutative,
        } in self.binary_operators.iter().filter(|e| e.kind == kind)
        {
            // Exact match
            if *x == lhs && *y == rhs {
                return Ok(*result);
            }
            // type commutative match
            if *type_commutative && *x == rhs && *y == lhs {
                return Ok(*result);
            }
        }
        Err(TypeError {
            loc,
            kind: TypeErrorKind::InvalidBinopOperands(kind, lhs, rhs),
        })
    }

    pub fn typecheck_unary_basic(
        &self,
        item: TypeId,
        kind: UnaryKind,
        loc: SourceLocation,
    ) -> Result<TypeId, TypeError> {
        for UnaryOperator { kind: _, x, result } in
            self.unary_operators.iter().filter(|e| e.kind == kind)
        {
            if *x == item {
                return Ok(*result);
            }
        }
        Err(TypeError {
            loc,
            kind: TypeErrorKind::InvalidUnopOperand(kind, item),
        })
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
                let left_id = left.ty.clone().inner();
                let right_id = right.ty.clone().inner();
                let ty = self.typecheck_binop(left_id, right_id, kind, expression.loc.clone())?;
                expression.ty.set(ty);
            }
            ExpressionKind::Unary { item, operator } => {
                self.typecheck_expression(item)?;
                let ty = match operator {
                    UnaryKind::Negation | UnaryKind::Not => self.typecheck_unary_basic(
                        item.ty.inner(),
                        *operator,
                        expression.loc.clone(),
                    )?,

                    UnaryKind::Dereferencing => {
                        use Type::*;

                        let underlying = item.ty.get(self.types());
                        match underlying {
                            Basic(_) | Function(_) | UserDefined(_) => {
                                return Err(TypeError {
                                    loc: expression.loc.clone(),
                                    kind: TypeErrorKind::DereferencingNonPointer {
                                        actual_type: item.ty.inner(),
                                    },
                                });
                            }
                            Pointer(PointerType { inner }) => *inner,
                        }
                    }
                    UnaryKind::AddressOf => {
                        assert!(
                            item.kind.lvalue(),
                            "COMPILER BUG: Could not take address of rvalue. This should've been handled by Parser"
                        );
                        unsafe {
                            (*self.types).monomorph_or_get_pointer(PointerType {
                                inner: item.ty.inner(),
                            })
                        }
                    }
                };
                expression.ty.set(ty);
            }
            ExpressionKind::Assignment { target, value } => {
                self.typecheck_expression(target)?;
                self.typecheck_expression(value)?;
                if target.ty != value.ty {
                    return Err(TypeError {
                        kind: TypeErrorKind::Todo {
                            message: "Coercions are not implemented yet".to_owned(),
                            file: &file!(),
                            line: line!(),
                            column: column!(),
                        }, // TODO:
                        loc: expression.loc.clone(),
                    });
                }
                expression.ty.set(target.ty.inner());
            }
            ExpressionKind::Literal(_) => {
                assert_ne!(
                    expression.ty.inner(),
                    TypeId::Unknown,
                    "Compiler bug: literals should always have a type set up by parser."
                );
            }
            ExpressionKind::VariableAccess(_n, id) => {
                let var = self.symbols[id.get()];

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
                let Type::Function(FunctionType {
                    return_type,
                    parameters,
                    is_variadic,
                }) = callee.ty.get(self.types())
                else {
                    todo!("Proper error handling");
                };
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
                                expected_type: expected.inner(),
                                actual_type: got.inner(),
                            },
                        });
                    }
                }
                let return_type = return_type.clone();
                expression.ty.set(return_type.inner());
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

            StatementKind::FunctionDeclaration(
                FunctionDeclaration {
                    name,
                    parameters,
                    body,
                    return_type,
                },
                id,
            ) => {
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
                self.symbols.insert(*id, TypedEntity { ty });

                self.current_function_type = Some(ty);
                for param in *parameters {
                    self.symbols.insert(
                        param.id,
                        TypedEntity {
                            ty: param.ty.inner(),
                        },
                    );
                }

                for statement in body.iter() {
                    self.typecheck_statement(statement)?;
                }
                self.current_function_type = None;

                // TODO: If there are no return in function and no return type is specified, treat it as returning void
            }
            StatementKind::VariableDeclaration(
                VariableDeclaration {
                    name: _,
                    initializer,
                    ty,
                },
                id,
            ) => {
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
                        kind: TypeErrorKind::Todo {
                            message: "Coercions are not implemented yet".to_owned(),
                            line: line!(),
                            column: column!(),
                            file: file!(),
                        }, // TODO
                    });
                }
                self.symbols.insert(*id, TypedEntity { ty: ty.inner() });
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
            StatementKind::Extern(
                ExternalFunction {
                    name,
                    kind: _kind,
                    parameters,
                    return_type,
                    is_variadic,
                },
                id,
            ) => {
                self.symbols.insert(
                    *id,
                    TypedEntity {
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
}
pub struct TypeError {
    pub loc: SourceLocation,
    pub kind: TypeErrorKind,
}

pub enum TypeErrorKind {
    Todo {
        message: String,
        file: &'static str,
        line: u32,
        column: u32,
    },
    MismatchedArgumentType {
        function_location: Option<SourceLocation>,
        function_name: Option<Box<str>>,
        expected_type: TypeId,
        actual_type: TypeId,
    },
    InvalidArity {
        expected_arguments: usize,
        actual_arguments: usize,
        function_name: Option<String>,
    },

    //                      kind (while, if)
    MismatchedConditionType(&'static str),

    InvalidBinopOperands(BinopKind, TypeId, TypeId),
    InvalidUnopOperand(UnaryKind, TypeId),
    DereferencingNonPointer {
        actual_type: TypeId,
    },
}
impl<'types> CompilerError<&TypeCtx<'types>> for TypeError {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn display_message(&self, f: &mut impl Write, ctx: &TypeCtx<'types>) -> std::fmt::Result {
        match &self.kind {
            TypeErrorKind::Todo { message, .. } => write!(f, "Not implemented: {}", message)?,
            TypeErrorKind::MismatchedArgumentType {
                function_location: _function_location,
                function_name: _function_name,
                expected_type,
                actual_type,
            } => write!(
                f,
                "Expected `{}`, found `{}`, Mismatched argument type",
                DisplayType(*expected_type, ctx),
                DisplayType(*actual_type, ctx)
            )?,
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
                write!(
                    f,
                    "{s} arguments were provided. Function{function_name} expects {expected_arguments} arguments, but got {actual_arguments}"
                )?
            }
            TypeErrorKind::MismatchedConditionType(name) => {
                write!(f, "{name} condition should evaluate to `bool` type")?
            }

            TypeErrorKind::InvalidBinopOperands(op, a, b) => write!(
                f,
                "Invalid operands to '{op}' operation ({a}, {b})",
                op = op.operator(),
                a = DisplayType(*a, ctx),
                b = DisplayType(*b, ctx)
            )?,
            TypeErrorKind::DereferencingNonPointer { actual_type } => write!(
                f,
                "Could not dereference `{}`",
                DisplayType(*actual_type, ctx)
            )?,

            TypeErrorKind::InvalidUnopOperand(kind, r#type) => {
                write!(f, "Could not {} {}", kind.verb(), DisplayType(*r#type, ctx))?
            }
        }
        Ok(())
    }

    fn display_note(&self, f: &mut impl Write, ctx: &TypeCtx<'types>) -> std::fmt::Result {
        match &self.kind {
            TypeErrorKind::Todo {
                message,
                file,
                line,
                column,
            } => write!(f, "{file}:{line}:{column}"),
            TypeErrorKind::MismatchedArgumentType {
                function_location,
                function_name,
                expected_type: _expected_type,
                actual_type: _actual_type,
            } => {
                if let Some(function_location) = function_location {
                    return write!(
                        f,
                        "{}is defined here: {loc}",
                        if let Some(function_name) = function_name {
                            format!("`{}` ", function_name)
                        } else {
                            "function ".to_owned()
                        },
                        loc = function_location,
                    );
                }
                Ok(())
            }
            TypeErrorKind::InvalidArity { .. } => Ok(()),
            TypeErrorKind::MismatchedConditionType(_) => Ok(()),
            TypeErrorKind::InvalidBinopOperands(_, _, _) => Ok(()),
            TypeErrorKind::DereferencingNonPointer { .. } => Ok(()),
            TypeErrorKind::InvalidUnopOperand(_, _) => Ok(()),
        }
    }
}

fn print_type<'types>(item: TypeId, types: &'types TypeCtx<'types>) {
    let ty = item.get(types);
    dbg!(&ty);
}
