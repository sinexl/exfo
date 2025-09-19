use crate::common::Join;
use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum Type<'ast> {
    Unknown,
    Void,
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
            Type::Void => write!(f, "void")?, 
        }
        Ok(())
    }
}

impl <'ast> Display for FunctionType<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "func({}) -> {}", Join(self.parameters, ", "), self.ret_type)
    }
}

impl<'ast> Type<'ast> {
    pub fn size(&self) -> usize {
        match self {
            Type::Unknown => panic!(".size() should never be called on unknown"),
            Type::Int64 | Type::Float64 => 8,
            Type::Function(_) => 8,
            Type::Void => 0,
        }
    }
}
