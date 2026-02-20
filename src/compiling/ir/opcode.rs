use crate::common::Identifier;
use crate::compiling::ir::binop::Binop;

#[derive(Clone, Debug)]
pub enum Opcode<'ir> {
    // Operators.
    Binop {
        left: Arg<'ir>,
        right: Arg<'ir>,
        destination: usize,
        kind: Binop,
    },
    Negate {
        destination: usize,
        item: Arg<'ir>,
    },
    AddressOf {
        destination: Lvalue,
        // Must be lvalue
        lvalue: Arg<'ir>,
    },

    // Control Flow
    FunctionCall {
        result: usize,
        callee: Arg<'ir>,
        args: &'ir [Arg<'ir>],
        is_variadic: bool,
    },
    Label {
        index: usize,
    },
    JmpIfNot {
        label: usize,
        condition: Arg<'ir>,
    },
    Jmp {
        label: usize,
    },
    Return(Option<Arg<'ir>>),


    // Writes
    Assign {
        destination: usize,
        source: Arg<'ir>,
    },
    // Dereference assign
    Store {
        // Offset in memory containing address.
        destination: Lvalue,
        source: Arg<'ir>,
    },
    Load {
        destination: Lvalue,
        source: Arg<'ir>,
    }
}

// TODO #3: Split the Arg enum into Rvalue & Lvalue. For now, there will be extra redundant "copy" of StackOffset & Argument
#[derive(Clone, Copy, Debug)]
pub enum Lvalue {
    StackOffset { offset: usize, size: usize } ,
    Argument{ index: usize,  size: usize },
}

impl<'ir> Lvalue {
    pub fn to_arg(self) -> Arg<'ir> {
        match self {
            Lvalue::StackOffset { offset, size } => Arg::StackOffset { offset, size },
            Lvalue::Argument { index, size } => Arg::Argument {index, size },
        }
    }

    pub fn size(self) -> usize {
        match self {
            Lvalue::StackOffset { size, .. } => size,
            Lvalue::Argument { size, .. } => size
        }
    }
}

#[derive(Clone, Debug)]
pub enum Arg<'ir> {
    Bool(bool),
    Int64 { bits: [u8; 8], signed: bool },
    String { index: usize }, // Index is in ir.strings
    ExternalFunction(Identifier<'ir>),

    // TODO: Utilize StackOffset::size and Argument::size
    StackOffset { offset: usize, size: usize },

    // Stores argument index (counting from 0) and size of the argument
    Argument { index: usize, size: usize },
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
