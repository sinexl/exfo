use crate::common::SourceLocation;
use std::fmt;
use std::fmt::{Write, Display, Formatter};

pub trait CompilerError<Context> {
    fn location(&self) -> SourceLocation;
    fn display_message(&self, f: &mut dyn Write, ctx: Context) -> fmt::Result;
    // If there is no note, implementors should not write anything
    fn display_note(&self, f: &mut dyn Write, ctx: Context) -> fmt::Result;
}

struct DisplayError<'a, Context>(&'a dyn CompilerError<Context>, Context);

impl<'a, Context> Display for DisplayError<'a, Context> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let DisplayError(error, ctx) = self;
        let loc = error.location();
        write!(
            f,
            "{loc}: error: ",
        )?;
        error.display_message(f, *ctx)?;
        let mut note = String::new();
        error.display_note(&mut note, *ctx)?;
        if !note.is_empty() {
            write!(f, "\n\tnote: ")?;
            error.display_note(f, *ctx)?;
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
