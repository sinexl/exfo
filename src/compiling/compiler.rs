use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{FunctionDeclaration, Statement, StatementKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use bumpalo::Bump;

pub struct Compiler<'a> {
    bump: &'a Bump,
    pub(crate) ir: &'a mut IntermediateRepresentation<'a>,
    current_function: Option<Vec<Opcode<'a>>>,
}

impl<'a> Compiler<'a> {
    pub fn new(bump: &'a Bump) -> Compiler<'a> {
        Self {
            bump,
            ir: bump.alloc(IntermediateRepresentation::new(bump)),
            current_function: None,
        }
    }
}

// impl<'a> StatementVisitor<&'a IntermediateRepresentation<'a>> for Compiler<'a> {
//     fn visit(&mut self, statement: &Statement) -> &'a IntermediateRepresentation<'a> {
//         self.compile_statement(statement);
//         &self.ir
//     }
// }

impl<'a, 'b> Compiler<'a> {
    pub fn compile_to_arg(&mut self, expression: &Expression<'b>) -> Arg<'a> {
        match &expression.kind {
            ExpressionKind::Binop { .. } => todo!(),
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
                self.push_opcode(Opcode::FunctionCall {
                    callee,
                    args: self.bump.alloc_slice_clone(args.as_slice()),
                });
                Arg::StackOffset(0)
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
                self.current_function = None;


                self.ir.functions.insert(name.clone_into(self.bump), self.bump.alloc(Function {
                    name: name.clone_into(self.bump),
                    code,
                }));
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
