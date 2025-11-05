use crate::lexing::token::Token;
use bumpalo::Bump;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

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

pub trait CompilerWarning {
    fn location(&self) -> SourceLocation;
    fn message(&self) -> String;
    fn note(&self) -> Option<String>;
}

impl Display for dyn CompilerWarning {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let message = self.message();
        write!(
            f,
            "{loc}: warning: {desc}.",
            desc = message,
            loc = self.location()
        )?;
        if let Some(note) = self.note() {
            writeln!(f, "\n\tnote: {}", note)?;
        }
        Ok(())
    }
}

pub trait IntoBox<T> {
    fn into_box(self) -> Box<T>;
}

impl<T> IntoBox<T> for T {
    fn into_box(self) -> Box<T> {
        Box::new(self)
    }
}

impl Display for dyn CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let message = self.message();
        if message.is_empty() {
            return Ok(());
        }
        write!(
            f,
            "{loc}: error: {desc}.",
            desc = message,
            loc = self.location()
        )?;
        if let Some(note) = self.note() {
            writeln!(f, "\n\tnote: {}", note)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub(crate) struct IdentifierInner<T> {
    pub name: T,
    pub location: SourceLocation,
}

impl<T> IdentifierInner<T> {
    pub fn new(name: T, location: SourceLocation) -> Self {
        Self { name, location }
    }
}

impl<T> Display for IdentifierInner<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct Join<'a, It>(pub It, pub &'a str);

impl<'a, It, T> Display for Join<'a, It>
where
    It: IntoIterator<Item = T> + Clone,
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.clone().into_iter().peekable();
        while let Some(item) = iter.next() {
            write!(f, "{}", item)?;
            if iter.peek().is_some() {
                write!(f, "{}", self.1)?;
            }
        }
        Ok(())
    }
}

pub(crate) type IdentifierBox = IdentifierInner<Box<str>>;
pub type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;
pub type Stack<T> = Vec<T>;

impl IdentifierBox {
    pub fn from_borrowed(id: &Identifier<'_>) -> Self {
        Self {
            location: id.location.clone(),
            name: Box::from(id.name),
        }
    }
}
pub(crate) type Identifier<'a> = IdentifierInner<&'a str>;
impl<'a> Identifier<'a> {
    pub fn clone_into<'b>(&self, bump: &'b Bump) -> Identifier<'b> {
        let name_in_b = bump.alloc_str(self.name);
        Identifier {
            name: name_in_b,
            location: self.location.clone(),
        }
    }

    pub fn from_token(token: Token, alloc: &'a Bump) -> Self {
        Self::new(alloc.alloc_str(&token.string), token.loc)
    }
}
#[macro_export]
macro_rules! hashmap {
    ($($key:expr => $val:expr),* $(,)?) => {{
        let mut map = ::std::collections::HashMap::new();
        $( map.insert($key, $val); )*
        map
    }};
}
#[macro_export]
macro_rules! push_errors {
    ($compilation_errors:expr, $errors:expr) => {{
        $compilation_errors.reserve($errors.len());
        for e in $errors {
            $compilation_errors.push(Box::new(e));
        }
    }};
}
