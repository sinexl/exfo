use crate::common::Identifier;
use crate::compiling::ir::opcode::Opcode;
use std::collections::HashMap;

#[derive(Debug)]
pub struct IntermediateRepresentation<'a> {
    pub functions: HashMap<Identifier<'a>, &'a Function<'a>>,
}

impl<'a> IntermediateRepresentation<'a> {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Identifier<'a>,
    pub code: &'a [Opcode<'a>],
    pub stack_size: usize,
    // Stores stack offset of each parameter 
    pub params: &'a [usize]
}
