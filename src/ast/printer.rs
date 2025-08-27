use crate::ast::{Expression, ExpressionKind};
use std::fmt::{Display, Formatter, Write};

pub fn print_ast(expr: &Expression<'_>, f: &mut impl Write, indent: usize) -> std::fmt::Result {
    let tab = " ".repeat((indent + 1) * 2);
    match &expr.kind {
        ExpressionKind::Literal(ident) => writeln!(f, "Literal({})", ident),
        ExpressionKind::Binop { left, right, kind } => {
            writeln!(f, "{}", kind.name())?;
            write!(f, "{tab}left = {}", Print(left, indent + 1))?;
            write!(f, "{tab}right = {}", Print(right, indent + 1))
        }
        ExpressionKind::Assignment { taget, value } => {
            writeln!(f, "Assignment")?;
            write!(f, "{tab}target = {}", Print(taget, indent + 1))
        }
        ExpressionKind::Unary { item, operator } => {
            writeln!(f, "{}", operator.name())?;
            write!(f, "{tab}item = {}", Print(item, indent + 1))
        }
        ExpressionKind::Grouping(inner) => {
            writeln!(f, "Grouping")?;
            write!(f, "{tab}{}", Print(inner, indent + 1))
        }
    }
}

struct Print<'a>(&'a Expression<'a>, usize);
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
