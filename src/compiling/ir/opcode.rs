use crate::common::identifier::Identifier;
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
    Not {
        result: Lvalue,
        item: Arg<'ir>,
    },

    // Control Flow
    FunctionCall {
        // Return value of the function (None means function returns void)
        result: Option<Lvalue>,
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
    // a = b.
    Assign {
        result: Lvalue,
        source: Arg<'ir>,
    },
    // Dereference assign (Store)
    // *result = source;
    Store {
        // Offset in memory containing address.
        result: Lvalue,
        source: Arg<'ir>,
    },
    // Load.
    // result = *source;
    Load {
        result: Lvalue,
        source: Arg<'ir>,
    },
}

impl<'ir> Lvalue {
    pub fn size(self) -> usize {
        match self {
            Lvalue::StackOffset { size, .. } => size,
            Lvalue::Argument { size, .. } => size,
        }
    }
}

impl Rvalue<'_> {
    pub fn size(&self) -> usize {
        match self {
            Rvalue::Void => 0,
            Rvalue::Bool(_) => 1,
            Rvalue::Int64 { .. } => 8,
            Rvalue::String { .. } => 8,
            Rvalue::ExternalFunction(_) => 8,
        }
    }
    // needed because strings differ from other args depending on PIC/non-pic, just to make code simpler.
    pub fn is_string(&self) -> bool {
        if let Rvalue::String { .. } = self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug)]
#[derive(PartialEq)]
pub enum Arg<'ir> {
    LValue(Lvalue),
    RValue(Rvalue<'ir>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Lvalue {
    StackOffset { offset: usize, size: usize },
    //         Index in Codegen::arg_offsets.
    Argument { index: usize, size: usize },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Rvalue<'ir> {
    // Nothing. Needed for the functions returning void.
    Void,

    Bool(bool),
    Int64 { bits: [u8; 8], signed: bool },
    String { index: usize }, // Index is in ir.strings
    ExternalFunction(Identifier<'ir>),
}

impl Arg<'_> {
    pub fn size(&self) -> usize {
        match self {
            Arg::RValue(rvalue) => rvalue.size(),

            Arg::LValue(lvalue) => lvalue.size(),
        }
    }
}
