use crate::common::SourceLocation;
use crate::lexing::token::Token;
use bumpalo::Bump;
use std::fmt;
use std::fmt::{Display, Formatter};

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
pub(crate) type IdentifierBox = IdentifierInner<Box<str>>;

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
