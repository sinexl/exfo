use crate::analysis::analyzer::Resolutions;
use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{FunctionDeclaration, Statement, StatementKind, VariableDeclaration};
use crate::common::Stack;
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::Bump;
use std::collections::HashMap;

pub struct Compiler<'ir, 'ast> {
    bump: &'ir Bump,
    current_function: Option<Vec<Opcode<'ir>>>,
    variables: Stack<HashMap<&'ast str, Variable>>,

    resolutions: Resolutions<'ast>,
    pub ir: &'ir mut IntermediateRepresentation<'ir>,

    stack_size: StackSize,
}

pub struct Variable {
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
    pub fn new(bump: &'ir Bump, resolutions: Resolutions<'ast>) -> Compiler<'ir, 'ast> {
        Self {
            bump,
            ir: bump.alloc(IntermediateRepresentation::new(bump)),
            current_function: None,
            stack_size: StackSize::zero(),
            resolutions,
            variables: Stack::new(),
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
    pub fn compile_expression(&mut self, expression: &Expression<'ast>) -> Arg<'ir> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind } => {
                let left = self.compile_expression(left);
                let right = self.compile_expression(right);
                let result = self.allocate_on_stack(8);
                self.push_opcode(Opcode::Binop {
                    left,
                    right,
                    result,
                    kind: *kind,
                });
                Arg::StackOffset(result)
            }
            ExpressionKind::Unary { item, operator } => {
                let item = self.compile_expression(item);
                match operator {
                    UnaryKind::Negation => {
                        let result = self.allocate_on_stack(8);
                        self.push_opcode(Opcode::Negate { item, result });
                        Arg::StackOffset(result)
                    }
                }
            }
            ExpressionKind::Assignment { target, value } => {
                let target = self.compile_expression(target);
                let arg = self.compile_expression(value);
                match target {
                    Arg::StackOffset(offset) => {
                        self.push_opcode(Opcode::Assign {
                            result: offset,
                            arg,
                        });
                        Arg::StackOffset(offset)
                    }
                    _ => todo!(),
                }
            }
            ExpressionKind::Literal(l) => Arg::Literal(*l),
            ExpressionKind::VariableAccess(n) => Arg::ExternalFunction(n.clone_into(self.bump)),
            ExpressionKind::FunctionCall { callee, arguments } => {
                let callee = self.compile_expression(callee);
                let args = arguments
                    .iter()
                    .map(|a| self.compile_expression(a))
                    .collect::<Vec<_>>();
                let result = self.allocate_on_stack(8);
                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args: self.bump.alloc_slice_clone(args.as_slice()),
                    result,
                });
                Arg::StackOffset(result)
            }
        }
    }

    pub fn push_opcode(&mut self, opcode: Opcode<'ir>) {
        if let Some(current_function) = self.current_function.as_mut() {
            current_function.push(opcode);
            return;
        }
        todo!()
    }

    pub fn compile_statement(&mut self, statement: &Statement<'ast>) {
        match &statement.kind {
            StatementKind::ExpressionStatement(expr) => {
                // The result of compile_expression is always temporary unless we actually assign it.
                // That means, we can reuse stack space for each new expression.
                let stack_size = self.stack_size.count;
                self.compile_expression(expr);
                self.stack_size.count = stack_size;
            }
            StatementKind::FunctionDeclaration(FunctionDeclaration { name, body }) => {
                self.current_function = Some(Vec::new());
                for statement in *body {
                    self.compile_statement(statement);
                }

                let code = self
                    .bump
                    .alloc_slice_clone(self.current_function.clone().unwrap().as_slice());
                self.ir.functions.insert(
                    name.clone_into(self.bump),
                    self.bump.alloc(Function {
                        name: name.clone_into(self.bump),
                        code,
                        stack_size: self.stack_size.max,
                    }),
                );

                self.current_function = None;
                self.stack_size = StackSize::zero();
            }
            StatementKind::Block(_) => todo!(),
            StatementKind::VariableDeclaration(VariableDeclaration { name, initializer }) => {
                let stack_offset = self.allocate_on_stack(8);
                let var = Variable { stack_offset };
                self.variables
                    .last_mut()
                    .expect("variable stack is empty")
                    .insert(name.name, var);

                if let Some(initializer) = initializer {
                    let arg = self.compile_expression(initializer);
                    self.push_opcode(Opcode::Assign {
                        result: stack_offset,
                        arg: arg,
                    })
                }
            }
        }
    }

    pub fn compile_statements(&mut self, statements: &[&'ast Statement<'ast>]) {
        for statement in statements {
            self.compile_statement(statement);
        }
    }
}
