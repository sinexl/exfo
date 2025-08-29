use crate::ast::expression::{Expression, ExpressionKind};
use std::fmt::{Display, Formatter, Write};

pub fn print_ast(expr: &Expression<'_>, f: &mut impl Write, indent: usize) -> std::fmt::Result {
    let tab = " ".repeat((indent + 1) * 2);
    match &expr.kind {
        ExpressionKind::Literal(value) => writeln!(f, "Literal({})", value),
        ExpressionKind::VariableAccess(identifier) => {
            writeln!(f, "Access({})", identifier.name)
        }
        ExpressionKind::Binop { left, right, kind } => {
            writeln!(f, "{}", kind.name())?;
            write!(f, "{tab}left = {}", Print(left, indent + 1))?;
            write!(f, "{tab}right = {}", Print(right, indent + 1))
        }
        ExpressionKind::Assignment {
            target: taget,
            value,
        } => {
            writeln!(f, "Assignment")?;
            write!(f, "{tab}target = {}", Print(taget, indent + 1))?;
            write!(f, "{tab}value  = {}", Print(value, indent + 1))
        }
        ExpressionKind::Unary { item, operator } => {
            writeln!(f, "{}", operator.name())?;
            write!(f, "{tab}item = {}", Print(item, indent + 1))
        }
        &ExpressionKind::FunctionCall { callee, arguments } => {
            writeln!(f, "Call")?;
            write!(f, "{tab}callee = {}", Print(callee, indent + 1))?;
            writeln!(f, "{tab}arguments = ")?;
            let tab = " ".repeat((indent + 2) * 2);
            for arg in arguments {
                write!(f, "{tab}{}", Print(arg, indent + 2))?;
            }
            Ok(())
        }
    }
}

pub(crate) struct Print<'a>(pub &'a Expression<'a>, pub usize);
impl Display for Print<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Print(expr, ind) = self;
        print_ast(expr, f, *ind)
    }
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Print(self, 0).fmt(f)
    }
}
