use crate::analysis::get_at::GetAt;
use crate::analysis::r#type::{FunctionType, Type, TypeCtx, TypeId};
use crate::analysis::resolver::Resolutions;
use crate::ast::binop::{BinopFamily, BinopKind};
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    ExternalFunction, FunctionDeclaration, Statement, StatementKind, VariableDeclaration,
};
use crate::common::{BumpVec, Stack};
use crate::compiling::ir::binop;
use crate::compiling::ir::binop::{Binop, BitwiseBinop, BitwiseKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::Opcode::{Jmp, JmpIfNot};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::collections::CollectIn;
use bumpalo::Bump;
use std::collections::HashMap;

pub struct Compiler<'ir, 'ast, 'types> {
    // allocators
    ir_bump: &'ir Bump,
    types: *mut TypeCtx<'types>,

    // Compiler state
    current_function: Option<BumpVec<'ir, Opcode<'ir>>>,
    variables: Stack<HashMap<&'ast str, Variable>>,
    loops: HashMap<usize, While>,
    stack_size: StackSize,
    label_count: usize,

    // Compiler output
    pub ir: &'ir mut IntermediateRepresentation<'ir>,

    resolutions: Resolutions<'ast>,
}

#[derive(Debug, Copy, Clone)]
struct Variable {
    ty: TypeId,
    stack_offset: usize, // TODO #2: variable location should not be strictly tied to stack offset.
    param_index: Option<usize>,
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

#[derive(Copy, Clone)]
struct ControlFlowLabels {
    enter_label: usize,
    exit_label: usize,
}

impl StackSize {
    fn zero() -> Self {
        Self { count: 0, max: 0 }
    }
}

impl<'ir, 'ast, 'types> Compiler<'ir, 'ast, 'types> {
    pub fn new(
        ir_bump: &'ir Bump,
        types: *mut TypeCtx<'types>,
        resolutions: Resolutions<'ast>,
    ) -> Compiler<'ir, 'ast, 'types> {
        Self {
            ir_bump,
            types,
            ir: ir_bump.alloc(IntermediateRepresentation::new(ir_bump)),
            current_function: None,
            stack_size: StackSize::zero(),
            resolutions,
            variables: Stack::new(),
            label_count: 0,
            loops: Default::default(),
        }
    }
    pub fn types(&self) -> &'types TypeCtx<'types> {
        unsafe { self.types.as_ref().expect("ERROR: Type context is NULL") }
    }
}

impl<'ir, 'ast, 'types> Compiler<'ir, 'ast, 'types> {
    pub fn allocate_on_stack(&mut self, size: usize) -> usize {
        self.stack_size.count += size;
        if self.stack_size.count >= self.stack_size.max {
            self.stack_size.max = self.stack_size.count;
        }
        self.stack_size.count
    }
    // This functions allocates a label but doesn't place it anywhere.
    // To place label, Op::Label is used.
    pub fn allocate_label(&mut self) -> usize {
        let index = self.label_count;
        self.label_count += 1;
        index
    }

    pub fn compile_expression(&mut self, expression: &Expression<'ast>) -> Arg<'ir> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind }
                if kind.family() == BinopFamily::Logical =>
            {
                let size = expression.ty.get(self.types()).size();
                assert_eq!(size, 1); // TODO: replace 1 to Platform::BoolSize or something
                let left = self.compile_expression(left);
                let offset = self.allocate_on_stack(size);
                match kind {
                    BinopKind::And => {
                        /*
                        Scheme of AND in IR: res = a && b; after;
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
                        self.push_opcode(JmpIfNot {
                            label: short_circuit,
                            condition: left.clone(),
                        });

                        // Case 1: `a` is true, so b should be evaluated.
                        let right = self.compile_expression(right);
                        self.push_opcode(Opcode::Binop {
                            left,
                            right,
                            result: offset,
                            kind: Binop::Bitwise(BitwiseBinop {
                                kind: BitwiseKind::And,
                                is_logical_with_short_circuit: true,
                            }),
                        });
                        let out_label = self.allocate_label();
                        self.push_opcode(Jmp { label: out_label });

                        // Case 2: `a` is false, so && is false.
                        self.push_opcode(Opcode::Label {
                            index: short_circuit,
                        });
                        self.push_opcode(Opcode::Assign {
                            result: offset,
                            arg: Arg::Bool(false),
                        });
                        self.push_opcode(Opcode::Label { index: out_label });

                        Arg::StackOffset { offset, size }
                    }
                    BinopKind::Or => {
                        /*
                        Scheme of OR in IR: res = a || b; after;
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
                        self.push_opcode(JmpIfNot {
                            label: do_check,
                            condition: left.clone(),
                        });
                        // Case one: The lhs is true, so entire || is true.
                        self.push_opcode(Opcode::Assign {
                            result: offset,
                            arg: Arg::Bool(true),
                        });
                        let out_label = self.allocate_label();
                        self.push_opcode(Jmp { label: out_label });

                        // Case two: The lhs is false, so rhs should be also evaluated to complete ||.
                        self.push_opcode(Opcode::Label { index: do_check });
                        let right = self.compile_expression(right);
                        self.push_opcode(Opcode::Binop {
                            left,
                            right,
                            result: offset,
                            kind: Binop::Bitwise(BitwiseBinop {
                                kind: BitwiseKind::Or,
                                is_logical_with_short_circuit: true,
                            }),
                        });

                        self.push_opcode(Opcode::Label { index: out_label });
                        Arg::StackOffset { offset, size }
                    }
                    _ => unreachable!("Unknown binary operation"),
                }
            }

            ExpressionKind::Binop { left, right, kind } => {
                let size = expression.ty.get(self.types()).size();
                let left = self.compile_expression(left);
                let right = self.compile_expression(right);
                let offset = self.allocate_on_stack(size);
                self.push_opcode(Opcode::Binop {
                    left,
                    right,
                    result: offset,
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
                Arg::StackOffset { offset, size }
            }

            ExpressionKind::Unary { item, operator } => {
                let size = item.ty.get(self.types()).size();
                let item = self.compile_expression(item);
                match operator {
                    UnaryKind::Negation => {
                        let result = self.allocate_on_stack(size);
                        self.push_opcode(Opcode::Negate { item, result });
                        Arg::StackOffset {
                            offset: result,
                            size,
                        }
                    }
                    UnaryKind::Dereferencing=> todo!("Compile dereferencing"),
                    UnaryKind::AddressOf => todo!("Compile address-of"),
                }
            }
            ExpressionKind::Assignment { target, value } => {
                let target = self.compile_expression(target);
                let arg = self.compile_expression(value);
                match target {
                    Arg::StackOffset { offset, size } => {
                        self.push_opcode(Opcode::Assign {
                            result: offset,
                            arg,
                        });
                        Arg::StackOffset { offset, size }
                    }
                    _ => todo!(),
                }
            }
            ExpressionKind::Literal(l) => {
                match l {
                    AstLiteral::Integral(i) => {
                        Arg::Int64 {
                            signed: true, // TODO
                            bits: i.to_le_bytes(),
                        }
                    }
                    AstLiteral::FloatingPoint(_) => {
                        todo!("Floats are not supported by compiler yet")
                    }
                    AstLiteral::Boolean(b) => Arg::Bool(*b),
                    AstLiteral::String(s) => {
                        let index = self.ir.strings.len();
                        self.ir.strings.push(self.ir_bump.alloc_str(s));

                        Arg::String { index }
                    }
                }
            }
            ExpressionKind::VariableAccess(n) => {
                let depth = *self.resolutions.get(expression).expect("Analysis failed");
                let var = self.variables.get_at(&n.name, depth);
                let var_type = var.ty.get(self.types());
                if let Type::Function(fn_type) = var_type {
                    Arg::ExternalFunction(n.clone_into(self.ir_bump))
                } else {
                    let size = var_type.size();
                    if let Some(index) = var.param_index {
                        assert_eq!(var.stack_offset, usize::MAX); // See TODO #2
                        return Arg::Argument { index, size };
                    }
                    Arg::StackOffset {
                        offset: var.stack_offset,
                        size,
                    }
                }
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                let is_variadic = if let Type::Function(FunctionType { is_variadic, .. }) =
                    callee.ty.get(self.types())
                {
                    *is_variadic
                } else {
                    false
                };
                let callee = self.compile_expression(callee);
                let b = self.ir_bump;
                let args = arguments
                    .iter()
                    .map(|a| {
                        self.compile_expression(unsafe {
                            a.as_ref().expect("FunctionCall: Null argument")
                        })
                    })
                    .collect_in::<BumpVec<_>>(b)
                    .into_bump_slice();
                let result = self.allocate_on_stack(8); // TODO: Return value size

                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args,
                    result,
                    is_variadic,
                });
                Arg::StackOffset {
                    offset: result,
                    size: 8, // TODO: Return value size
                }
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
            StatementKind::FunctionDeclaration(FunctionDeclaration {
                name,
                body,
                parameters,
                return_type,
            }) => {
                self.current_function = Some(BumpVec::new_in(self.ir_bump));

                let parent_scope = self.variables.len() - 1;

                let parameters_types = parameters
                    .iter()
                    .map(|p| p.ty.clone())
                    .collect_in::<BumpVec<_>>(self.types().bump())
                    .into_bump_slice();
                self.variables[parent_scope].insert(
                    name.name,
                    Variable {
                        ty: unsafe {
                            (*self.types).allocate(Type::Function(FunctionType {
                                return_type: return_type.clone(),
                                parameters: parameters_types,
                                is_variadic: false,
                            }))
                        },
                        stack_offset: 0,
                        param_index: None,
                    },
                );

                // Function argument allocation.
                // Function argument loading convention:
                // 1st argument <- stack[n_1];
                // 2nd argument <- stack[n_1 + n_2];
                // 3rd argument <- stack[n_1 + n_2 + n_3];
                // ....
                // mth argument <- stack[<sum of all previous arguments' sizes> + m_n];
                // where:
                // n_1 ... m_n => mth argument size,
                //           m => amount of arguments
                let mut sizes = BumpVec::with_capacity_in(parameters.len(), self.ir_bump);
                self.variables.push(HashMap::new());
                let function_scope = self.variables.len() - 1;
                for (i, param) in parameters.iter().enumerate() {
                    self.variables[function_scope].insert(
                        param.name.name,
                        Variable {
                            ty: param.ty.inner(),
                            stack_offset: usize::MAX,
                            param_index: Some(i),
                        },
                    );
                    sizes.push(param.ty.get(self.types()).size());
                }

                for statement in *body {
                    self.compile_statement(statement);
                }

                let code = self.current_function.take().unwrap().into_bump_slice();
                self.ir.functions.insert(
                    name.clone_into(self.ir_bump),
                    self.ir_bump.alloc(Function {
                        name: name.clone_into(self.ir_bump),
                        code,
                        stack_size: self.stack_size.max,
                        params: sizes.into_bump_slice(),
                    }),
                );

                self.current_function = None;
                self.variables.pop();
                self.stack_size = StackSize::zero();
            }

            StatementKind::Extern(ExternalFunction {
                name,
                kind: _kind,
                parameters,
                return_type,
                is_variadic,
            }) => {
                let var = Variable {
                    ty: unsafe {
                        (*self.types).allocate(Type::Function(FunctionType {
                            return_type: return_type.clone(),
                            parameters,
                            is_variadic: *is_variadic,
                        }))
                    },
                    stack_offset: usize::MAX,
                    param_index: None,
                };
                self.variables
                    .last_mut()
                    .expect("Compiler bug: there should be at least global scope")
                    .insert(name.name, var);
            }
            StatementKind::Block(statements) => {
                self.variables.push(HashMap::new());
                let stack_size = self.stack_size.count;
                for i in *statements {
                    self.compile_statement(i);
                }
                self.stack_size.count = stack_size;
                self.variables.pop();
            }
            StatementKind::VariableDeclaration(VariableDeclaration {
                name,
                initializer,
                ty,
            }) => {
                let stack_offset = self.allocate_on_stack(ty.get(self.types()).size());
                // Since shadowing is allowed, initializers are compiled before variable is inserted in the compiler tables.
                // i. e
                // a := 10;
                // a := a + 15;
                if let Some(initializer) = initializer {
                    let arg = self.compile_expression(initializer);
                    self.push_opcode(Opcode::Assign {
                        result: stack_offset,
                        arg,
                    })
                }
                let var = Variable {
                    stack_offset,
                    ty: ty.inner(),
                    param_index: None,
                };
                self.variables
                    .last_mut()
                    .expect("Compiler bug: there should be at least global scope")
                    .insert(name.name, var);
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
        self.variables.push(HashMap::new());
        for statement in statements {
            self.compile_statement(statement);
        }
    }
}
