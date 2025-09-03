use crate::ast::expression::AstLiteral;
use crate::common::Identifier;

#[derive(Clone, Debug)]
pub enum Opcode<'a> {
    FunctionCall {
        callee: Arg<'a>,
        args: &'a [Arg<'a>],
    }
}

#[derive(Clone, Debug)]
pub enum Arg<'a> {
    Literal(AstLiteral),
    ExternalFunction(Identifier<'a>),
    StackOffset(usize)
}