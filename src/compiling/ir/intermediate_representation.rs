use crate::common::identifier::Identifier;
use crate::common::BumpVec;
use crate::compiling::ir::opcode::Opcode;
use bumpalo::Bump;

#[derive(Debug)]
pub struct IntermediateRepresentation<'ir> {
    pub strings: BumpVec<'ir, &'ir str>,
    pub functions: BumpVec<'ir, Function<'ir>>,
}

impl<'ir> IntermediateRepresentation<'ir> {
    pub fn new(ir_bump: &'ir Bump) -> Self {
        Self {
            functions: BumpVec::new_in(ir_bump),
            strings: BumpVec::new_in(ir_bump),
        }
    }
}

#[derive(Debug)]
pub struct Function<'ir> {
    pub name: Identifier<'ir>,
    pub code: &'ir [Opcode<'ir>],
    pub stack_size: usize,
    pub params: &'ir [usize], // Stores parameters' sizes. Number of a parameter is the index in the array
}
