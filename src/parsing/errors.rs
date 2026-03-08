use crate::common::errors_warnings::CompilerError;
use crate::common::{Join, SourceLocation};
use crate::lexing::lexer::{Token, TokenType};
use std::fmt::Write;


#[derive(Debug, Eq, PartialEq)]
pub struct ParseError<'errors> {
    pub loc: SourceLocation,
    pub kind: ParseErrorKind<'errors>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParseErrorKind<'errors> {
    UnknownType(&'errors str),
    AtEof,
    UnbalancedParens,
    UnbalancedBraces,
    InvalidAssignment(&'errors str),
    InvalidAddressOf(&'errors str),
    UnexpectedToken {
        expected: &'errors [TokenType],
        got: TokenType,
        r#where: &'errors str, // where parser did expect the token(s)
        note: Option<&'errors str>,
    },
    UnknownToken(Token),
    UnknownExternKind(&'errors str),
    EmptyInput,
}

impl<'errors> CompilerError<()> for ParseError<'errors> {
    fn location(&self) -> SourceLocation {
        self.loc.clone()
    }

    fn display_message(&self, f: &mut impl Write, _ctx: ()) -> std::fmt::Result {
        use ParseErrorKind::*;
        match &self.kind {
            AtEof => write!(f, "Nothing to parse")?,
            UnbalancedParens => write!(f, "unbalanced parentheses")?,
            InvalidAssignment(name) => write!(f, "could not assign to {}", name)?,
            UnexpectedToken {
                expected,
                got,
                r#where,
                note: _,
            } => {
                let r#where = if r#where.is_empty() {
                    "".to_string()
                } else {
                    format!(" {where}")
                };
                write!(
                    f,
                    "Expected {}{where}, but got {}",
                    Join(*expected, "/"),
                    got
                )?;
            }
            UnbalancedBraces => write!(f, "unbalanced braces")?,
            UnknownType(b) => write!(f, "unknown type {}", b)?,
            UnknownExternKind(k) => write!(f, "unknown extern kind: \"{}\"", k)?,
            UnknownToken(tk) => write!(f, "unknown token: {tk}")?,
            EmptyInput => write!(f, "empty input: nothing to parse")?,
            InvalidAddressOf(name) => write!(f, "could not take address of {}", name)?,
        }
        Ok(())
    }

    fn display_note(&self, f: &mut impl Write, _ctx: ()) -> std::fmt::Result {
        use ParseErrorKind::*;
        match &self.kind {
            AtEof => Ok(()),
            UnbalancedParens => write!(f, "last parenthesis located here: {}", self.loc),
            InvalidAssignment(_) => Ok(()),
            UnexpectedToken {
                note: Some(note), ..
            } => write!(f, "{note}"),
            UnexpectedToken { .. } => Ok(()),
            UnbalancedBraces => write!(f, "last braces located here: {}", self.loc),
            UnknownType(_) => Ok(()),
            UnknownExternKind(_) => Ok(()),
            UnknownToken(_) => Ok(()),
            EmptyInput => Ok(()),
            InvalidAddressOf(_) => Ok(()),
        }
    }
}
