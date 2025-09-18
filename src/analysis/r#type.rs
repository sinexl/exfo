use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum Type<'ast> {
    Unknown,
    Int64,
    Float64,
    Function(FunctionType<'ast>),
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub struct FunctionType<'ast> {
    pub ret_type: &'ast Type<'ast>,
    pub parameters: &'ast [Type<'ast>],
}

impl<'ast> Display for Type<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "<unknown>")?,
            Type::Int64 => write!(f, "int64")?,
            Type::Float64 => write!(f, "float64")?,
            Type::Function(fun) => write!(f, "{}", fun)?,
        }
        Ok(())
    }
}

impl <'ast> Display for FunctionType<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "func() -> ")
    }
}

impl<'ast> Type<'ast> {
    pub fn size(&self) -> usize {
        match self {
            Type::Unknown => panic!(".size() should never be called on unknown"),
            Type::Int64 | Type::Float64 => 8,
            Type::Function(_) => 8,
        }
    }
}
