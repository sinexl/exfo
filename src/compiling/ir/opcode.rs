use crate::ast::binop::BinopKind;
use crate::ast::expression::AstLiteral;
use crate::common::Identifier;

#[derive(Clone, Debug)]
pub enum Opcode<'a> {
    FunctionCall {
        callee: Arg<'a>,
        args: &'a [Arg<'a>],
    },
    Binop {
        left: Arg<'a>,
        right: Arg<'a>,
        result: usize,
        kind: BinopKind,
    },
}

#[derive(Clone, Debug)]
pub enum Arg<'a> {
    Literal(AstLiteral),
    ExternalFunction(Identifier<'a>),
    StackOffset(usize),
}
