use crate::ast::binop::BinopKind;
use crate::common::Identifier;

#[derive(Clone, Debug)]
pub enum Opcode<'a> {
    FunctionCall {
        result: usize,
        callee: Arg<'a>,
        args: &'a [Arg<'a>],
    },
    Binop {
        left: Arg<'a>,
        right: Arg<'a>,
        result: usize,
        kind: BinopKind,
    },
    Negate {
        result: usize,
        item: Arg<'a>,
    },
    Assign {
        result: usize,
        arg: Arg<'a>,
    },
}

#[derive(Clone, Debug)]
pub enum Arg<'a> {
    Int64 { bits: [u8; 8], signed: bool },
    ExternalFunction(Identifier<'a>),
    StackOffset { offset: usize, size: usize },
}
