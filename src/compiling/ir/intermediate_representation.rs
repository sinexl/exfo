use crate::common::identifier::Identifier;
use crate::common::BumpVec;
use crate::compiling::ir::opcode::Opcode;
use bumpalo::Bump;
use std::collections::HashMap;

#[derive(Debug)]
pub struct IntermediateRepresentation<'a> {
    pub strings: BumpVec<'a, &'a str>,
    pub functions: HashMap<Identifier<'a>, &'a Function<'a>>,
}

impl<'a> IntermediateRepresentation<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        Self {
            functions: HashMap::new(),
            strings: BumpVec::new_in(bump),
        }
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Identifier<'a>,
    pub code: &'a [Opcode<'a>],
    pub stack_size: usize,
    pub params: &'a [usize], // Stores parameters' sizes. Number of a parameter is the index in the array
}
