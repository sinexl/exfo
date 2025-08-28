use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use bumpalo::Bump;
use crate::lexer::token::Token;

#[derive(Debug, Hash, Default, Clone, Eq, PartialEq)]
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
    fn note(&self) -> Option<String>;
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

#[derive(Debug)]
pub(crate) struct Identifier<'a> {
    pub name: &'a str,
    pub location: SourceLocation,
}

impl<'a> Identifier<'a> {
    pub fn new(name: &'a str, location: SourceLocation) -> Self {
        Self { name, location }
    }

    pub fn from_token(token: Token, alloc: &'a Bump) -> Self {
        Self::new(alloc.alloc_str(&token.string), token.loc)
    }
}
impl<'a> From<Identifier<'a>> for &'a str {
    fn from(id: Identifier<'a>) -> &'a str {
        id.name
    }
}

