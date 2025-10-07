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
    Label {
        index: usize,
    },
    JmpIfNot {
        label: usize,
        condition: Arg<'a>,
    },
    Jmp {
        label: usize,
    },
    Return(Option<Arg<'a>>),
}

#[derive(Clone, Debug)]
pub enum Arg<'a> {
    Bool(bool),
    Int64 { bits: [u8; 8], signed: bool },
    String { index: usize }, // Index is in ir.strings
    ExternalFunction(Identifier<'a>),
    StackOffset { offset: usize, size: usize },
}
