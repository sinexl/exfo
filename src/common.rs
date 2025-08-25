use std::rc::Rc;

#[derive(Debug)]
pub struct SourceLocation {
    pub line: usize,
    pub offset: usize,
    pub file: Rc<str>,
}

impl SourceLocation {
    pub fn new(file: Rc<str>, line: usize, offset: usize) -> Self { SourceLocation { line, offset, file } }
}