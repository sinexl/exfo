use crate::ast::{Expression, ExpressionKind, ExpressionVisitor};
use std::fmt::{Display, Formatter, Write};

pub(crate) struct AstPrinter {}

impl ExpressionVisitor<String> for AstPrinter {
    fn visit(&mut self, expression: &Expression) -> String {
        let mut str = String::new();
        self.print(expression, &mut str, 0).unwrap();
        str
    }
}

impl AstPrinter {
    #[allow(clippy::only_used_in_recursion)]
    pub fn print(&self, expr: &Expression<'_>, f: &mut impl Write, indent: usize) -> std::fmt::Result {
        let tab = " ".repeat((indent + 1) * 2);
        match &expr.kind {
            ExpressionKind::Literal(ident) => writeln!(f, "Literal({})", ident),
            ExpressionKind::Binop { left, right, kind } => {
                writeln!(f, "{}", kind.name())?;
                write!(f, "{tab}left = ")?;
                self.print(left, f, indent + 1)?;
                write!(f, "{tab}right = ")?;
                self.print(right, f, indent + 1)
            }
            ExpressionKind::Unary { item, operator } => {
                writeln!(f, "{}", operator.name())?;
                write!(f, "{tab}item = ")?;
                self.print(item, f, indent + 1)
            }
            ExpressionKind::Grouping(inner) => {
                writeln!(f, "Grouping")?;
                self.print(inner, f, indent + 1)
            }
        }
    }
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        AstPrinter{}.print(self, f, 0)
    }
}
