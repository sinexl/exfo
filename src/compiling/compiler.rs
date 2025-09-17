use crate::analysis::analyzer::Resolutions;
use crate::analysis::get_at::GetAt;
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{FunctionDeclaration, Statement, StatementKind, VariableDeclaration};
use crate::common::{BumpVec, Stack};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::Bump;
use bumpalo::collections::CollectIn;
use std::collections::HashMap;

pub struct Compiler<'ir, 'ast> {
    bump: &'ir Bump,
    current_function: Option<BumpVec<'ir, Opcode<'ir>>>,
    variables: Stack<HashMap<&'ast str, Variable>>,

    resolutions: Resolutions<'ast>,
    pub ir: &'ir mut IntermediateRepresentation<'ir>,

    stack_size: StackSize,
}

#[derive(Debug, Copy, Clone)]
pub struct Variable {
    is_function: bool, // TODO:  this is not how this should be done.
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
                let size = left.r#type.get().size();
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
                let size = item.r#type.get().size();
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
                }
            }
            ExpressionKind::VariableAccess(n) => {
                let depth = *self.resolutions.get(expression).expect("Analysis failed");
                // TODO: this is kinda dumb hack to get things working.
                let var = self.variables.get_at(&n.name, depth);
                if var.is_function {
                    Arg::ExternalFunction(n.clone_into(self.bump))
                } else {
                    Arg::StackOffset {
                        offset: var.stack_offset,
                        size: 8, // TODO
                    }
                }
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                let callee = self.compile_expression(callee);
                let b = self.bump;
                let args = arguments
                    .iter()
                    .map(|a| self.compile_expression(a))
                    .collect_in::<BumpVec<_>>(b)
                    .into_bump_slice();
                let result = self.allocate_on_stack(8);
                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args,
                    result,
                });
                Arg::StackOffset {
                    offset: result,
                    size: 8, // TODO
                }
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
                self.current_function = Some(BumpVec::new_in(self.bump));
                self.variables.push(HashMap::new());
                for statement in *body {
                    self.compile_statement(statement);
                }

                let code = self.current_function.take().unwrap().into_bump_slice();
                self.ir.functions.insert(
                    name.clone_into(self.bump),
                    self.bump.alloc(Function {
                        name: name.clone_into(self.bump),
                        code,
                        stack_size: self.stack_size.max,
                    }),
                );

                self.current_function = None;
                self.variables.pop();
                self.stack_size = StackSize::zero();
            }
            StatementKind::Block(_) => todo!(),
            StatementKind::VariableDeclaration(VariableDeclaration { name, initializer }) => {
                let stack_offset = self.allocate_on_stack(8);
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
                    is_function: false,
                };
                self.variables
                    .last_mut()
                    .expect("variable stack is empty")
                    .insert(name.name, var);
            }
        }
    }

    pub fn compile_statements(&mut self, statements: &[&'ast Statement<'ast>]) {
        // TODO: Obviously, this is a hack.
        let mut globals = HashMap::new();
        globals.insert(
            "print_i64",
            Variable {
                is_function: true,
                stack_offset: 0,
            },
        );
        self.variables.push(globals);
        for statement in statements {
            self.compile_statement(statement);
        }
    }
}
