use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Hash, Default, Clone)] 
pub struct SourceLocation {
    pub line: usize,
    pub offset: usize,
    pub file: Rc<str>,
}

impl SourceLocation {
    pub fn new(file: Rc<str>, line: usize, offset: usize) -> Self {
        SourceLocation { line, offset, file }
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.offset)
    }
}

pub trait CompilerError {
    fn location(&self) -> SourceLocation;
    fn message(&self) -> String;
    fn note(&self) -> Option<String> ;
}

impl Display for dyn CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{loc}: error: {desc}.",
            desc = self.message(),
            loc = self.location()
        )?;
        if let Some(note) = self.note() {
            writeln!(f, "\n\tnote: {}", note)?;
        }
        Ok(())
    }
}
