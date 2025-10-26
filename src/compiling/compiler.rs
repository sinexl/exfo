use crate::analysis::resolver::Resolutions;
use crate::analysis::get_at::GetAt;
use crate::analysis::r#type::{FunctionType, Type};
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    ExternalFunction, FunctionDeclaration, Statement, StatementKind, VariableDeclaration,
};
use crate::common::{BumpVec, Stack};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::Bump;
use bumpalo::collections::CollectIn;
use std::collections::HashMap;

pub struct Compiler<'ir, 'ast> {
    ir_bump: &'ir Bump,
    ast_bump: &'ast Bump, // TODO: Compiler should not do any ast-related allocations.
    current_function: Option<BumpVec<'ir, Opcode<'ir>>>,
    variables: Stack<HashMap<&'ast str, Variable<'ast>>>,

    resolutions: Resolutions<'ast>,
    pub ir: &'ir mut IntermediateRepresentation<'ir>,

    stack_size: StackSize,
    label_count: usize,
}

#[derive(Debug, Copy, Clone)]
struct Variable<'a> {
    ty: Type<'a>,
    stack_offset: usize,
}

struct StackSize {
    count: usize,
    max: usize,
}

impl StackSize {
    fn zero() -> Self {
        Self { count: 0, max: 0 }
    }
}

impl<'ir, 'ast> Compiler<'ir, 'ast> {
    pub fn new(
        ir_bump: &'ir Bump,
        ast_bump: &'ast Bump,
        resolutions: Resolutions<'ast>,
    ) -> Compiler<'ir, 'ast> {
        Self {
            ir_bump,
            ast_bump,
            ir: ir_bump.alloc(IntermediateRepresentation::new(ir_bump)),
            current_function: None,
            stack_size: StackSize::zero(),
            resolutions,
            variables: Stack::new(),
            label_count: 0,
        }
    }
}

impl<'ir, 'ast> Compiler<'ir, 'ast> {
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
            ExpressionKind::Binop { left, right, kind } => {
                let size = match kind.is_logical() {
                    true => Type::Bool.size(),
                    false => left.ty.get().size()
                };
                let left = self.compile_expression(left);
                let right = self.compile_expression(right);
                let offset = self.allocate_on_stack(size);
                self.push_opcode(Opcode::Binop {
                    left,
                    right,
                    result: offset,
                    kind: *kind,
                });
                Arg::StackOffset { offset, size }
            }
            ExpressionKind::Unary { item, operator } => {
                let size = item.ty.get().size();
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
                        todo!()
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

                if let Type::Function(fn_type) = var.ty {
                    Arg::ExternalFunction(n.clone_into(self.ir_bump))
                } else {
                    Arg::StackOffset {
                        offset: var.stack_offset,
                        size: var.ty.size(),
                    }
                }
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                let callee = self.compile_expression(callee);
                let b = self.ir_bump;
                let args = arguments
                    .iter()
                    .map(|a| self.compile_expression(a))
                    .collect_in::<BumpVec<_>>(b)
                    .into_bump_slice();
                let result = self.allocate_on_stack(8); // TODO: Return value size
                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args,
                    result,
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

    pub fn compile_statement(&mut self, statement: &'ast Statement<'ast>) {
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
                    .map(|p| p.ty)
                    .collect_in::<BumpVec<_>>(self.ast_bump)
                    .into_bump_slice();
                let return_type = self.ast_bump.alloc(return_type.get());
                self.variables[parent_scope].insert(
                    name.name,
                    Variable {
                        ty: Type::Function(FunctionType {
                            return_type,
                            parameters: parameters_types,
                        }),
                        stack_offset: 0,
                    },
                );

                let mut offsets = BumpVec::with_capacity_in(parameters.len(), self.ir_bump);
                self.variables.push(HashMap::new());
                let function_scope = self.variables.len() - 1;
                for param in *parameters {
                    let stack_offset = self.allocate_on_stack(param.ty.size());
                    self.variables[function_scope].insert(
                        param.name.name,
                        Variable {
                            ty: param.ty,
                            stack_offset,
                        },
                    );
                    offsets.push(stack_offset);
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
                        params: offsets.into_bump_slice(),
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
            }) => {
                let var = Variable {
                    ty: Type::Function(FunctionType {
                        return_type: self.ast_bump.alloc(return_type.get()),
                        parameters,
                    }),
                    stack_offset: 0,
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
                let stack_offset = self.allocate_on_stack(ty.get().size());
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
                    ty: ty.get(),
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

                let stack_size = self.stack_size.count;
                let condition = self.compile_expression(condition);
                self.stack_size.count = stack_size;

                let else_label = self.allocate_label();
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
        }
    }

    pub fn compile_statements(&mut self, statements: &[&'ast Statement<'ast>]) {
        self.variables.push(HashMap::new());
        for statement in statements {
            self.compile_statement(statement);
        }
    }
}
