use crate::common::SourceLocation;
use std::fmt;
use std::fmt::{Display, Formatter, Write};

pub trait CompilerError<Context>
where
    Context: Clone,
{
    fn location(&self) -> SourceLocation;
    fn display_message(&self, f: &mut impl Write, ctx: Context) -> fmt::Result;
    // If there is no note, implementors should not write anything
    fn display_note(&self, f: &mut impl Write, ctx: Context) -> fmt::Result;
}

pub mod display {
    use crate::common::errors_warnings::CompilerError;
    use std::fmt;
    use std::fmt::{Display, Formatter};

    pub struct DisplayError<'a, Context, E>(&'a E, Context)
    where
        E: CompilerError<Context>,
        Context: Clone;

    pub trait DisplayErrorExtension<Context, E>: CompilerError<Context>
    where
        E: CompilerError<Context>,
        Context: Clone,
    {
        fn display(&self, ctx: Context) -> DisplayError<Context, E>;
    }

    impl<Context, E> DisplayErrorExtension<Context, E> for E
    where
        E: CompilerError<Context>,
        Context: Clone,
    {
        fn display(&self, ctx: Context) -> DisplayError<Context, E> {
            DisplayError(self, ctx)
        }
    }

    impl<'a, Context, E> Display for DisplayError<'a, Context, E>
    where
        E: CompilerError<Context>,
        Context: Clone,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            let DisplayError(error, ctx) = self;
            let loc = error.location();
            write!(f, "{loc}: error: ",)?;
            error.display_message(f, ctx.clone())?;
            let mut note = String::new();
            error.display_note(&mut note, ctx.clone())?;
            if !note.is_empty() {
                write!(f, "\n\tnote: ")?;
                error.display_note(f, ctx.clone())?;
            }
            Ok(())
        }
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
