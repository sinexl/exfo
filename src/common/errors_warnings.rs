use crate::common::SourceLocation;
use std::fmt;
use std::fmt::{Display, Formatter};

pub trait CompilerError {
    fn location(&self) -> SourceLocation;
    fn message(&self) -> String;
    fn note(&self) -> Option<String>;
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
