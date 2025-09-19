use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{FunctionDeclaration, VariableDeclaration};
use crate::ast::statement::{Statement, StatementKind};
use std::fmt::{Display, Formatter, Write};
use crate::common::Join;

pub fn print_ast(expr: &Expression<'_>, f: &mut impl Write, indent: usize) -> std::fmt::Result {
    let tab = " ".repeat((indent + 1) * 2);
    write!(f, "<{:?}> ", expr.ty.get())?;
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

pub fn print_statement(
    stmt: &Statement<'_>,
    f: &mut impl Write,
    indent: usize,
) -> std::fmt::Result {
    let tab = " ".repeat((indent + 1) * 2);
    match &stmt.kind {
        StatementKind::ExpressionStatement(expr) => {
            writeln!(f, "Expression Statement")?;
            write!(f, "{tab}{}", Print(expr, indent + 1))?;
        }
        StatementKind::FunctionDeclaration(FunctionDeclaration { name, body, parameters, return_type }) => {
            writeln!(f, "Func `{}` ({}): {return_type}", name.name, Join(*parameters, ", "))?;
            for statement in *body {
                write!(f, "{tab}{}", PrintStatement(statement, indent + 1))?;
            }
        }
        StatementKind::Block(statements) => {
            writeln!(f, "Block")?;
            for statement in *statements {
                write!(f, "{tab}{}", PrintStatement(statement, indent + 1))?;
            }
        }
        StatementKind::VariableDeclaration(VariableDeclaration { name, initializer }) => {
            writeln!(f, "Variable `{}`", name.name)?;
            if let Some(init) = initializer {
                write!(f, "{tab}Initializer = {init}")?;
            }
        }
    }
    Ok(())
}

pub(crate) struct Print<'expr, 'ast>(pub &'expr Expression<'ast>, pub usize);
impl<'ast, 'expr> Display for Print<'ast, 'expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Print(expr, ind) = self;
        print_ast(expr, f, *ind)
    }
}

impl<'ast> Display for Expression<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Print(self, 0).fmt(f)
    }
}
pub(crate) struct PrintStatement<'ast, 'expr>(pub &'expr Statement<'ast>, pub usize);
impl<'ast, 'expr> Display for PrintStatement<'ast, 'expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrintStatement(statement, ind) = self;
        print_statement(statement, f, *ind)
    }
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        PrintStatement(self, 0).fmt(f)
    }
}
