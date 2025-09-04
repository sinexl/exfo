use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{FunctionDeclaration, Statement, StatementKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::Bump;

pub struct Compiler<'a> {
    bump: &'a Bump,
    pub(crate) ir: &'a mut IntermediateRepresentation<'a>,
    current_function: Option<Vec<Opcode<'a>>>,
    stack_size: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(bump: &'a Bump) -> Compiler<'a> {
        Self {
            bump,
            ir: bump.alloc(IntermediateRepresentation::new(bump)),
            current_function: None,
            stack_size: 0,
        }
    }
}

impl<'a, 'b> Compiler<'a> {
    pub fn allocate_on_stack(&mut self, size: usize) -> usize {
        self.stack_size += size;
        self.stack_size

    }
    pub fn compile_to_arg(&mut self, expression: &Expression<'b>) -> Arg<'a> {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind } => {
                let left = self.compile_to_arg(left);
                let right = self.compile_to_arg(right);
                let result = self.allocate_on_stack(8);
                self.push_opcode(
                    Opcode::Binop {
                        left,
                        right,
                        result,
                        kind: *kind
                    }
                );
                Arg::StackOffset(result)

            },
            ExpressionKind::Unary { .. } => todo!(),
            ExpressionKind::Assignment { .. } => todo!(),
            ExpressionKind::Literal(l) => Arg::Literal(*l),
            ExpressionKind::VariableAccess(n) => Arg::ExternalFunction(n.clone_into(self.bump)),
            ExpressionKind::FunctionCall { callee, arguments } => {
                let callee = self.compile_to_arg(callee);
                let args = arguments
                    .iter()
                    .map(|a| self.compile_to_arg(a))
                    .collect::<Vec<_>>();
                let result = self.allocate_on_stack(8);
                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args: self.bump.alloc_slice_clone(args.as_slice()),
                    result
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
                self.compile_to_arg(expr);
            }
            StatementKind::FunctionDeclaration(FunctionDeclaration { name, body }) => {
                self.current_function = Some(Vec::new());
                for statement in *body  {
                    self.compile_statement(statement);
                }

                let code = self.bump.alloc_slice_clone(self.current_function.clone().unwrap().as_slice());
                self.ir.functions.insert(name.clone_into(self.bump), self.bump.alloc(Function {
                    name: name.clone_into(self.bump),
                    code,
                    stack_size: self.stack_size,
                }));

                self.current_function = None;
                self.stack_size = 0;
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
