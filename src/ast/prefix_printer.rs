use crate::ast::binop::BinopKind;
use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use std::fmt::{Display, Formatter, Write};

pub fn prefix_print(expr: &Expression<'_>, f: &mut impl Write) -> std::fmt::Result {
    match &expr.kind {
        ExpressionKind::Binop { left, right, kind } => {
            let op = match kind {
                BinopKind::Addition => "+",
                BinopKind::Subtraction => "-",
                BinopKind::Multiplication => "*",
                BinopKind::Division => "/",
                BinopKind::Equality => "==",
                BinopKind::Inequality => "!=",
                BinopKind::GreaterThan => ">",
                BinopKind::GreaterEq => ">=",
                BinopKind::LessThan => "<",
                BinopKind::LessEq => "<=",
            };
            parenthesize(f, op, &[left, right])
        }
        ExpressionKind::Unary { item, operator } => {
            let op = match operator {
                UnaryKind::Negation => "-",
            };
            parenthesize(f, op, &[item])
        }
        ExpressionKind::Assignment { target, value } => parenthesize(f, "=", &[target, value]),
        ExpressionKind::Literal(lit) => write!(f, "{}", lit),
        ExpressionKind::VariableAccess(name) => write!(f, "{}", name.name),
        ExpressionKind::FunctionCall { callee, arguments } => {
            let mut temp = Vec::new();
            temp.push(*callee);
            temp.reserve(arguments.len());
            for arg in *arguments {
                temp.push(arg);
            }
            parenthesize(f, "call", temp.as_slice())
        }
    }
}

fn parenthesize(
    f: &mut impl Write,
    name: &str,
    expressions: &[&'_ Expression<'_>],
) -> std::fmt::Result {
    if name.is_empty() {
        write!(f, "(")?;
    } else {
        write!(f, "({} ", name)?;
    }

    for (i, expr) in expressions.iter().enumerate() {
        prefix_print(expr, f)?;
        if i != expressions.len() - 1 {
            write!(f, " ")?;
        }
    }
    write!(f, ")")
}

pub(crate) struct PrefixPrint<'a>(pub &'a Expression<'a>);
impl Display for PrefixPrint<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrefixPrint(expr) = self;
        prefix_print(expr, f)
    }
}
