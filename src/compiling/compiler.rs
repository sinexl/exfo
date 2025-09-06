use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{FunctionDeclaration, Statement, StatementKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::Bump;

pub struct Compiler<'a> {
    bump: &'a Bump,
    pub(crate) ir: &'a mut IntermediateRepresentation<'a>,
    current_function: Option<Vec<Opcode<'a>>>,
    stack_size: StackSize,
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

impl<'a> Compiler<'a> {
    pub fn new(bump: &'a Bump) -> Compiler<'a> {
        Self {
            bump,
            ir: bump.alloc(IntermediateRepresentation::new(bump)),
            current_function: None,
            stack_size: StackSize::zero(),
        }
    }
}

impl<'a, 'b> Compiler<'a> {
    pub fn allocate_on_stack(&mut self, size: usize) -> usize {
        self.stack_size.count += size;
        if self.stack_size.count >= self.stack_size.max {
            self.stack_size.max = self.stack_size.count;
        }
        self.stack_size.count
    }
    pub fn compile_expression(&mut self, expression: &Expression<'b>) -> Arg<'a> {
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
                        self.push_opcode(Opcode::Negate {
                            item,
                            result,
                        });
                        Arg::StackOffset(result)
                    }
                }
            },
            ExpressionKind::Assignment { .. } => todo!(),
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

    pub fn push_opcode(&mut self, opcode: Opcode<'a>) {
        if let Some(current_function) = self.current_function.as_mut() {
            current_function.push(opcode);
            return;
        }
        todo!()
    }

    pub fn compile_statement(&mut self, statement: &Statement<'b>) {
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
        }
    }

    pub fn compile_statements(&mut self, statements: &[&'a Statement<'a>]) {
        for statement in statements {
            self.compile_statement(statement);
        }
    }
}
