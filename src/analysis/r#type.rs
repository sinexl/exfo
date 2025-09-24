use crate::common::Join;
use std::fmt::{Display, Formatter};


// TODO: 
//  Current representation of type is not suitable for the language. 
//  All types should by default allow internal mutability. 
//  Apart from that, all fields in intermediate representations (AST/3AC IR/etc.) 
//  should store references (or ids ) who point to the actual type.
#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum Type<'ast> {
    Unknown,
    Void,
    Int64,
    Float64,
    Bool, 
    CharPtr, 
    Function(FunctionType<'ast>),
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub struct FunctionType<'ast> {
    pub return_type: &'ast Type<'ast>,
    pub parameters: &'ast [Type<'ast>],
}

impl<'ast> Display for Type<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Type::Unknown => write!(f, "<unknown>")?,
            Type::Int64 => write!(f, "int64")?,
            Type::Float64 => write!(f, "float64")?,
            Type::Function(fun) => write!(f, "{}", fun)?,
            Type::Void => write!(f, "void")?,
            Type::Bool => write!(f, "bool")?,
            Type::CharPtr => write!(f, "char_ptr")?, 
        }
        Ok(())
    }
}

impl <'ast> Display for FunctionType<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "func({}): {}", Join(self.parameters, ", "), self.return_type)
    }
}

impl<'ast> Type<'ast> {
    pub fn size(&self) -> usize {
        match self {
            Type::Unknown => panic!(".size() should never be called on unknown"),
            Type::Int64 | Type::Float64 => 8,
            Type::Function(_) => 8,
            Type::Void => 0,
            Type::Bool => 1,
            Type::CharPtr => 8,
        }
    }
}
