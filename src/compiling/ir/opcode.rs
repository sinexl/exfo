use crate::common::Identifier;
use crate::compiling::ir::binop::Binop;

#[derive(Clone, Debug)]
pub enum Opcode<'a> {
    FunctionCall {
        result: usize,
        callee: Arg<'a>,
        args: &'a [Arg<'a>],
        is_variadic: bool,
    },
    Binop {
        left: Arg<'a>,
        right: Arg<'a>,
        result: usize,
        kind: Binop,
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
    String { index: usize },                    // Index is in ir.strings
    ExternalFunction(Identifier<'a>),           // Bool: is variadic
    StackOffset { offset: usize, size: usize }, // TODO: Utilize StackOffset::size and Argument::size
    Argument { index: usize, size: usize },     // Stores argument number (counting from 0)
                                                // and size of the argument
}

impl Arg<'_> {
    pub fn size(&self) -> usize {
        match self {
            Arg::Bool(_) => 1,
            Arg::Int64 { .. } => 8,
            Arg::String { .. } => 8,
            Arg::ExternalFunction(_) => 8,
            Arg::StackOffset { size, .. } => *size,
            Arg::Argument { index: _, size } => *size,
        }
    }
}
