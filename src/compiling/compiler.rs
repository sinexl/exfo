use crate::analysis::r#type::{FunctionType, Type, TypeId, TypeIdCell};
use crate::analysis::type_context::TypeCtx;
use crate::ast::binop::{BinopFamily, BinopKind};
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, SymId, UnaryKind};
use crate::ast::statement::{
    ExternalFunction, FunctionDeclaration, Statement, StatementKind, VariableDeclaration,
};
use crate::common::BumpVec;
use crate::common::symbol_table::{CompilerEntity, SymbolTable};
use crate::compiling::ir::binop;
use crate::compiling::ir::binop::{Binop, BitwiseBinop, BitwiseKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Lvalue, Opcode, Rvalue};
use bumpalo::Bump;
use bumpalo::collections::CollectIn;
use std::collections::HashMap;

pub struct Compiler<'ir, 'types> {
    // allocators
    ir_bump: &'ir Bump,
    types: *mut TypeCtx<'types>,

    // Compiler state
    current_function: Option<BumpVec<'ir, Opcode<'ir>>>,
    entities: SymbolTable<Entity>,
    loops: HashMap<usize, While>,
    stack_size: StackSize,
    label_count: usize,
    // A "bucket" lvalue. Used for copy propagation/forwarding
    bucket: Option<Lvalue>,

    // Compiler output
    pub ir: &'ir mut IntermediateRepresentation<'ir>,
}

#[derive(Debug, Copy, Clone)]
struct Entity {
    ty: TypeId,
    kind: EntityKind,
}

impl CompilerEntity for Entity {}

#[derive(Clone, Copy, Debug)]
enum EntityKind {
    Local(Lvalue),
    Function,
}

#[derive(Copy, Clone)]
struct While {
    start: usize,
    exit: usize,
}

#[derive(Copy, Clone)]
struct StackSize {
    count: usize,
    max: usize,
}

impl StackSize {
    fn zero() -> Self {
        Self { count: 0, max: 0 }
    }
}

impl<'ir, 'ast, 'types> Compiler<'ir, 'types> {
    pub fn new(
        ir_bump: &'ir Bump,
        types: *mut TypeCtx<'types>,
        symbol_count: usize,
    ) -> Compiler<'ir, 'types> {
        Self {
            ir_bump,
            types,
            ir: ir_bump.alloc(IntermediateRepresentation::new(ir_bump)),
            current_function: None,
            stack_size: StackSize::zero(),
            entities: SymbolTable::new(symbol_count),
            label_count: 0,
            loops: Default::default(),
            bucket: None,
        }
    }
    pub fn types(&self) -> &'types TypeCtx<'types> {
        unsafe { self.types.as_ref().expect("ERROR: Type context is NULL") }
    }
}

impl<'ir, 'ast, 'types> Compiler<'ir, 'types> {
    pub fn allocate(&mut self, size: usize) -> Lvalue {
        if let Some(bucket) = self.bucket.take_if(|e| e.size() == size) {
            return bucket;
        }
        let i = self.allocate_on_stack(size);
        i
    }
    pub fn allocate_on_stack(&mut self, size: usize) -> Lvalue {
        self.stack_size.count += size;
        if self.stack_size.count >= self.stack_size.max {
            self.stack_size.max = self.stack_size.count;
        }
        Lvalue::StackOffset {
            offset: self.stack_size.count,
            size,
        }
    }
    // This functions allocates a label but doesn't place it anywhere.
    // To place label, Op::Label is used.
    pub fn allocate_label(&mut self) -> usize {
        let index = self.label_count;
        self.label_count += 1;
        index
    }

    pub fn compile_expression(&mut self, expression: &'ast Expression<'ast>) -> Arg<'ir> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind }
                if kind.family() == BinopFamily::Logical =>
            {
                let size = expression.ty.get(self.types()).size();
                assert_eq!(size, 1); // TODO: replace 1 to Platform::BoolSize or something
                let left = self.compile_expression(left);
                let result = self.allocate(size);
                match kind {
                    BinopKind::And => {
                        /*
                        Scheme of AND in IR that is implemented below.
                        suppose pseudocode: res = a && b; after;
                        generated pseudo-IR:
                          a.
                          if !a jump FALSE:
                             b;
                             res = a && b;
                             jump out
                          FALSE:
                             res = false
                          out:
                            after;
                        */
                        let short_circuit = self.allocate_label();
                        // Cases: if lhs is false, jump to do_check.
                        self.push_opcode(Opcode::JmpIfNot {
                            label: short_circuit,
                            condition: left.clone(),
                        });

                        // Case 1: `a` is true, so b should be evaluated.
                        let right = self.compile_expression(right);
                        self.push_opcode(Opcode::Binop {
                            left,
                            right,
                            result,
                            kind: Binop::Bitwise(BitwiseBinop {
                                kind: BitwiseKind::And,
                                is_logical_with_short_circuit: true,
                            }),
                        });
                        let out_label = self.allocate_label();
                        self.push_opcode(Opcode::Jmp { label: out_label });

                        // Case 2: `a` is false, so && is false.
                        self.push_opcode(Opcode::Label {
                            index: short_circuit,
                        });
                        self.push_opcode(Opcode::Assign {
                            result,
                            source: Arg::RValue(Rvalue::Bool(false)),
                        });
                        self.push_opcode(Opcode::Label { index: out_label });

                        Arg::LValue(result)
                    }
                    BinopKind::Or => {
                        /*
                        Scheme of OR in IR:
                        suppose pseudocode: res = a || b; after;
                        generated pseudo-IR:
                          a.
                          if !a jump check
                             res = true
                             jump out
                          check:
                             b;
                             res = a || b;
                          out:
                            after;
                        */
                        let do_check = self.allocate_label();
                        // Cases: if lhs is false, jump to do_check.
                        self.push_opcode(Opcode::JmpIfNot {
                            label: do_check,
                            condition: left.clone(),
                        });
                        // Case one: The lhs is true, so entire || is true.
                        self.push_opcode(Opcode::Assign {
                            result,
                            source: Arg::RValue(Rvalue::Bool(true)),
                        });
                        let out_label = self.allocate_label();
                        self.push_opcode(Opcode::Jmp { label: out_label });

                        // Case two: The lhs is false, so rhs should be also evaluated to complete ||.
                        self.push_opcode(Opcode::Label { index: do_check });
                        let right = self.compile_expression(right);
                        self.push_opcode(Opcode::Binop {
                            left,
                            right,
                            result,
                            kind: Binop::Bitwise(BitwiseBinop {
                                kind: BitwiseKind::Or,
                                is_logical_with_short_circuit: true,
                            }),
                        });

                        self.push_opcode(Opcode::Label { index: out_label });
                        Arg::LValue(result)
                    }
                    _ => unreachable!("Unknown binary operation"),
                }
            }

            ExpressionKind::Binop { left, right, kind } => {
                let size = expression.ty.get(self.types()).size();
                let left = self.compile_expression(left);
                let right = self.compile_expression(right);
                let result = self.allocate(size);
                self.push_opcode(Opcode::Binop {
                    left,
                    right,
                    result,
                    kind: match kind.family() {
                        BinopFamily::Arithmetic | BinopFamily::Ordering => {
                            //                         TODO: Unsigned
                            binop::IntegerBinop::from_ast(true, size, *kind)
                        }
                        BinopFamily::Logical => {
                            unreachable!("This should be handled by another match arm")
                        }
                    },
                });
                Arg::LValue(result)
            }

            ExpressionKind::Unary { item, operator } => {
                let ty = expression.ty.clone();
                let item = self.compile_expression(item);
                match operator {
                    UnaryKind::Negation => {
                        let size = ty.get(self.types()).size();
                        let result = self.allocate(size);
                        self.push_opcode(Opcode::Negate { item, result });
                        Arg::LValue(result)
                    }
                    UnaryKind::Dereferencing => {
                        let size = ty.get(self.types()).size();
                        let result = self.allocate(size);
                        self.push_opcode(Opcode::Load {
                            result,
                            source: item,
                        });
                        Arg::LValue(result)
                    }
                    UnaryKind::AddressOf => {
                        let size = 8;
                        let result = self.allocate(size);
                        let item = match item {
                            Arg::LValue(e) => e,
                            Arg::RValue(_) => panic!(
                                "COMPILER BUG: Could not take address of rvalue. This should have been handled by Type Checker."
                            ),
                        };
                        self.push_opcode(Opcode::AddressOf {
                            result,
                            source: item,
                        });

                        Arg::LValue(result)
                    }
                }
            }
            ExpressionKind::Assignment { target, value } => {
                // Deref assignment.
                // *target = value
                if let ExpressionKind::Unary { item, operator } = &target.kind
                    && *operator == UnaryKind::Dereferencing
                {
                    let result = match self.compile_expression(item) {
                        Arg::LValue(e) => e,
                        Arg::RValue(_) => panic!(
                            "Could not assign to an rvalue. This should have been handled by Type Checker."
                        ),
                    };
                    let arg = self.compile_expression(value);
                    self.push_opcode(Opcode::Store {
                        result,
                        source: arg,
                    });

                    return Arg::LValue(result);
                }

                // Normal assignment
                let target = self.compile_expression(target);
                match target {
                    Arg::LValue(result) => {
                        let source = self.compile_expression(value);
                        self.push_opcode(Opcode::Assign { result, source });
                        Arg::LValue(result)
                    }
                    _ => panic!(
                        "Could not assign to an rvalue. This should have been handled by Type Checker."
                    ),
                }
            }
            ExpressionKind::Literal(l) => {
                match l {
                    AstLiteral::Integral(i) => {
                        Arg::RValue(Rvalue::Int64 {
                            signed: true, // TODO
                            bits: i.to_le_bytes(),
                        })
                    }
                    AstLiteral::FloatingPoint(_) => {
                        todo!("Floats are not supported by compiler yet")
                    }
                    AstLiteral::Boolean(b) => Arg::RValue(Rvalue::Bool(*b)),
                    AstLiteral::String(s) => {
                        let index = self.ir.strings.len();
                        self.ir.strings.push(self.ir_bump.alloc_str(s));

                        Arg::RValue(Rvalue::String { index })
                    }
                }
            }
            ExpressionKind::VariableAccess(n, id) => {
                let entity = self.entities[id.get()];
                match entity.kind {
                    EntityKind::Local(lvalue) => Arg::LValue(lvalue),
                    EntityKind::Function => {
                        Arg::RValue(Rvalue::ExternalFunction(n.clone_into(self.ir_bump)))
                    }
                }
            }

            ExpressionKind::FunctionCall { callee, arguments } => {
                let (is_variadic, return_size) = if let Type::Function(FunctionType {
                    parameters,
                    is_variadic,
                    return_type,
                }) = callee.ty.get(self.types())
                {
                    (*is_variadic, return_type.inner().get(self.types()).size())
                } else {
                    todo!("Indirect function call")
                };
                let callee = self.compile_expression(callee);
                let b = self.ir_bump;
                let args = arguments
                    .iter()
                    .filter_map(|a| {
                        let expr = unsafe { a.as_ref().expect("FunctionCall: Null Argument") };
                        if expr.ty.get(self.types()).size() != 0 {
                            return Some(self.compile_expression(expr));
                        }
                        return None;
                    })
                    .collect_in::<BumpVec<_>>(b)
                    .into_bump_slice();
                let result = if return_size != 0 {
                    Some(self.allocate(return_size))
                } else {
                    None
                };

                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args,
                    result,
                    is_variadic,
                });
                if let Some(result) = result {
                    return Arg::LValue(result);
                }
                Arg::RValue(Rvalue::Void)
            }
        }
    }

    pub fn push_opcode(&mut self, opcode: Opcode<'ir>) {
        if let Some(current_function) = self.current_function.as_mut() {
            current_function.push(opcode);
            return;
        }
        todo!("Top-level statements are not supported yet")
    }

    pub fn compile_statement(&mut self, statement: &'ast Statement<'ast, 'types>) {
        match &statement.kind {
            StatementKind::ExpressionStatement(expr) => {
                // The result of compile_expression is always temporary unless we actually assign it.
                // That means, we can reuse stack space for each new expression.
                let stack_size = self.stack_size.count;
                self.compile_expression(expr);
                self.stack_size.count = stack_size;
            }
            StatementKind::FunctionDeclaration(
                FunctionDeclaration {
                    name,
                    body,
                    parameters,
                    return_type,
                },
                id,
            ) => {
                struct Param {
                    ty: TypeIdCell,
                    id: SymId,
                };
                self.current_function = Some(BumpVec::new_in(self.ir_bump));
                // Collect non-zero parameters
                let parameters = parameters
                    .iter()
                    .filter_map(|e| {
                        let types =
                            unsafe { self.types.as_ref().expect("ERROR: Type context is NULL") };
                        if e.ty.get(types).size() != 0 {
                            return Some(Param {
                                ty: e.ty.clone(),
                                id: e.id,
                            });
                        }
                        None
                    })
                    .collect::<Vec<_>>();

                let parameters_types = parameters
                    .iter()
                    .map(|p| p.ty.clone())
                    .collect_in::<BumpVec<_>>(self.types().bump())
                    .into_bump_slice();
                self.entities.insert(
                    *id,
                    Entity {
                        ty: unsafe {
                            (*self.types).allocate(Type::Function(FunctionType {
                                return_type: return_type.clone(),
                                parameters: parameters_types,
                                is_variadic: false,
                            }))
                        },
                        kind: EntityKind::Function,
                    },
                );

                let mut sizes = BumpVec::with_capacity_in(parameters.len(), self.ir_bump);
                for (i, param) in parameters.iter().enumerate() {
                    let param_size = param.ty.get(self.types()).size();
                    self.entities.insert(
                        param.id,
                        Entity {
                            ty: param.ty.inner(),
                            kind: EntityKind::Local(Lvalue::Argument {
                                index: i,
                                size: param_size,
                            }),
                        },
                    );
                    sizes.push(param.ty.get(self.types()).size());
                }

                for statement in *body {
                    self.compile_statement(statement);
                }

                let code = self.current_function.take().unwrap().into_bump_slice();
                self.ir.functions.push(Function {
                    name: name.clone_into(self.ir_bump),
                    code,
                    stack_size: self.stack_size.max,
                    params: sizes.into_bump_slice(),
                });

                self.current_function = None;
                self.stack_size = StackSize::zero();
            }

            StatementKind::Extern(
                ExternalFunction {
                    name: _,
                    kind: _kind,
                    parameters,
                    return_type,
                    is_variadic,
                },
                id,
            ) => {
                let var = Entity {
                    ty: unsafe {
                        (*self.types).allocate(Type::Function(FunctionType {
                            return_type: return_type.clone(),
                            parameters,
                            is_variadic: *is_variadic,
                        }))
                    },
                    kind: EntityKind::Function,
                };
                self.entities.insert(*id, var);
            }
            StatementKind::Block(statements) => {
                let stack_size = self.stack_size.count;
                for i in *statements {
                    self.compile_statement(i);
                }
                self.stack_size.count = stack_size;
            }
            StatementKind::VariableDeclaration(
                VariableDeclaration {
                    name: _,
                    initializer,
                    ty,
                },
                id,
            ) => {
                let size = ty.get(self.types()).size();
                let stack_offset = self.allocate_on_stack(size);
                // To allow shadowing, initializers are compiled before variable is inserted in the compiler tables.
                // i. e
                // a := 10;
                // a := a + 15;
                if let Some(initializer) = initializer {
                    self.bucket = Some(stack_offset);
                    let source = self.compile_expression(initializer);
                    if source != Arg::LValue(stack_offset) {
                        self.push_opcode(
                            Opcode::Assign {
                                result: stack_offset,
                                source: source.clone(),
                            }
                            .clone(),
                        );
                    }
                    self.bucket = None;
                }
                let var = Entity {
                    ty: ty.inner(),
                    kind: EntityKind::Local(stack_offset),
                };
                self.entities.insert(*id, var);
            }
            StatementKind::Return(val) => {
                let ret_arg = val.as_ref().map(|arg| self.compile_expression(arg));
                self.push_opcode(Opcode::Return(ret_arg));
            }
            StatementKind::If {
                condition,
                then,
                r#else,
            } => {
                /*
                  Scheme of if statement in IR:
                    stmt1;
                    condition;
                    if !condition: jump else_label
                        then; // only going to be executed if condition is true
                        out;  // prevent falling through to `else_branch`.
                    else_label:
                        r#else
                    out_label:
                    stmt2 // code after if-else block.
                */

                let else_label = self.allocate_label();

                let stack_size = self.stack_size.count;
                let condition = self.compile_expression(condition);
                self.stack_size.count = stack_size;

                self.push_opcode(Opcode::JmpIfNot {
                    label: else_label,
                    condition,
                });
                self.compile_statement(then);

                if let Some(r#else) = r#else {
                    let out_label = self.allocate_label();
                    self.push_opcode(Opcode::Jmp { label: out_label });
                    self.push_opcode(Opcode::Label { index: else_label });
                    self.compile_statement(r#else);
                    self.push_opcode(Opcode::Label { index: out_label });
                } else {
                    self.push_opcode(Opcode::Label { index: else_label })
                }
            }
            StatementKind::While {
                condition,
                body,
                name: _name,
                id,
            } => {
                /*
                Scheme of While loop in IR:
                  start:
                    condition;
                    if !condition: jump exit;
                    body1;
                    bod2;
                    jump start;
                  exit:
                    after1;
                 */
                let start = self.allocate_label();
                self.push_opcode(Opcode::Label { index: start });
                let stack_size = self.stack_size.count;
                let condition = self.compile_expression(condition);
                self.stack_size.count = stack_size;

                let exit = self.allocate_label();
                self.loops.insert(id.get(), While { start, exit });
                self.push_opcode(Opcode::JmpIfNot {
                    label: exit,
                    condition,
                });
                self.compile_statement(body);
                self.push_opcode(Opcode::Jmp { label: start });
                self.push_opcode(Opcode::Label { index: exit });
            }
            StatementKind::Break { .. } | StatementKind::Continue { .. } => {
                const MSG: &str = "Compiler bug: loop label resolution failed";
                self.push_opcode(Opcode::Jmp {
                    label: match &statement.kind {
                        StatementKind::Break { id, .. } => {
                            self.loops.get(&id.get()).expect(MSG).exit
                        }
                        StatementKind::Continue { id, .. } => {
                            self.loops.get(&id.get()).expect(MSG).start
                        }
                        _ => unreachable!(),
                    },
                })
            }
        }
    }

    pub fn compile_statements(&mut self, statements: &[&'ast Statement<'ast, 'types>]) {
        for statement in statements {
            self.compile_statement(statement);
        }
    }
}
