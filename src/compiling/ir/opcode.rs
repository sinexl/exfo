use crate::common::Identifier;
use crate::compiling::ir::binop::Binop;

#[derive(Clone, Debug)]
pub enum Opcode<'ir> {
    // Operators.
    Binop {
        left: Arg<'ir>,
        right: Arg<'ir>,
        result: Lvalue,
        kind: Binop,
    },
    Negate {
        result: Lvalue,
        item: Arg<'ir>,
    },
    AddressOf {
        result: Lvalue,
        source: Lvalue,
    },

    // Control Flow
    FunctionCall {
        // Return value of the function
        result: Lvalue,
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
        result: Lvalue,
        source: Arg<'ir>,
    },
    // Dereference assign
    Store {
        // Offset in memory containing address.
        result: Lvalue,
        source: Arg<'ir>,
    },
    Load {
        result: Lvalue,
        source: Arg<'ir>,
    }
}


impl<'ir> Lvalue {
    pub fn size(self) -> usize {
        match self {
            Lvalue::StackOffset { size, .. } => size,
            Lvalue::Argument { size, .. } => size
        }
    }
}

impl Rvalue<'_> {
    pub fn size(&self) -> usize {
        match self {
            Rvalue::Bool(_) => 1,
            Rvalue::Int64 { .. } => 8,
            Rvalue::String { .. } => 8,
            Rvalue::ExternalFunction(_) => 8,
        }
    }
}



#[derive(Clone, Debug)]
pub enum Arg<'ir> {
    LValue(Lvalue),
    RValue(Rvalue<'ir>),
}


#[derive(Clone, Copy, Debug)]
pub enum Lvalue {
    // TODO: Utilize size in LValues
    StackOffset { offset: usize, size: usize } ,
    Argument{ index: usize,  size: usize },
}

#[derive(Clone, Debug)]
pub enum Rvalue<'ir> {
    Bool(bool),
    Int64 { bits: [u8; 8], signed: bool },
    String { index: usize }, // Index is in ir.strings
    ExternalFunction(Identifier<'ir>),
}

impl Arg<'_> {
    pub fn size(&self) -> usize {
        match self {
            // Arg::Bool(_) => 1,
            // Arg::Int64 { .. } => 8,
            // Arg::String { .. } => 8,
            // Arg::ExternalFunction(_) => 8,
            // Arg::StackOffset { size, .. } => *size,
            // Arg::Argument { index: _, size } => *size,
            Arg::RValue(rvalue) => {
                match rvalue {
                    Rvalue::Bool(_) => 1,
                    Rvalue::Int64 { .. } => 8,
                    Rvalue::String { .. } => 8,
                    Rvalue::ExternalFunction(_) => 8,
                }
            }

            Arg::LValue(lvalue) => lvalue.size(),
        }
    }
}
