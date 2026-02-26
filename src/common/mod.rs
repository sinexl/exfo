pub mod symbol_table;
pub mod errors_warnings;
pub mod identifier;

use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

pub type BumpVec<'a, T> = bumpalo::collections::Vec<'a, T>;

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


pub trait IntoBox<T> {
    fn into_box(self) -> Box<T>;
}

impl<T> IntoBox<T> for T {
    fn into_box(self) -> Box<T> {
        Box::new(self)
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
#[macro_export]
macro_rules! debug_scopes {
    ($scopes: expr) => {{
        for (index, scope) in $scopes.iter().rev().enumerate() {
            println!("---- Scope {index} ----");
            debug_scope!(scope);
        }
    }};
}

#[macro_export]
macro_rules! debug_scope {
    ($scope: expr) => {{
        for (name, resolutions) in $scope {
            println!("{name} = {resolutions:?}");
        }
    }};
}

