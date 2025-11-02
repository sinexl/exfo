use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    ExternalFunction, FunctionDeclaration, Statement, StatementKind, VariableDeclaration,
};
use crate::common::Join;
use std::fmt::{Display, Formatter, Write};

pub const PREFIX_TAB: &str = " ";

pub fn prefix_print(expr: &Expression<'_>, f: &mut impl Write) -> std::fmt::Result {
    match &expr.kind {
        ExpressionKind::Binop { left, right, kind } => {
            let op = kind.operator();
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

pub fn prefix_print_statement(statement: &Statement<'_>, f: &mut impl Write) -> std::fmt::Result {
    let tab = PREFIX_TAB;
    match &statement.kind {
        StatementKind::ExpressionStatement(expr) => {
            prefix_print(expr, f)?;
            write!(f, ";")?;
        }
        StatementKind::FunctionDeclaration(FunctionDeclaration {
            name,
            body,
            parameters,
            return_type,
        }) => {
            write!(
                f,
                "(func `{}` ({}): {}",
                name.name,
                Join(*parameters, ", "),
                return_type.get()
            )?;
            if !body.is_empty() {
                writeln!(f)?;
                for statement in *body {
                    writeln!(f, "{tab}{}", PrefixPrintStatement(statement))?;
                }
            }
            writeln!(f, ")")?;
        }

        StatementKind::Extern(ExternalFunction {
            name,
            kind,
            parameters,
            return_type,
        }) => {
            write!(
                f,
                "(extern \"{kind:?}\" `{}` ({}): {}",
                name.name,
                Join(*parameters, ", "),
                return_type.get()
            )?;
        }
        StatementKind::Block(body) => {
            writeln!(f, "(block")?;
            if !body.is_empty() {
                writeln!(f)?;
                for statement in *body {
                    writeln!(f, "{tab}{}", PrefixPrintStatement(statement))?;
                }
            }
            writeln!(f, ")")?;
        }
        StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            initializer,
            ty,
        }) => {
            write!(f, "(`{}`: {} ", name.name, ty.get())?;
            if let Some(initializer) = initializer {
                write!(f, "= {}", PrefixPrint(initializer))?;
            }
            writeln!(f, ")")?;
        }
        StatementKind::Return(ret) => {
            write!(f, "(return")?;
            if let Some(ret) = ret {
                write!(f, " {ret}")?;
            }
            writeln!(f, ")")?;
        }
        StatementKind::If {
            condition,
            then,
            r#else,
        } => {
            write!(f, "(if {condition}: {then}")?;
            if let Some(r#else) = r#else {
                write!(f, "else: {r}", r = r#else)?;
            }
            writeln!(f, ")")?;
        }
        StatementKind::While {
            condition,
            body,
            name,
            id,
        } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            write!(f, "(({id}{name}): while {condition}: {body}", id = id.get())?;
            writeln!(f, ")")?;
        }
        StatementKind::Break { name, id } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            writeln!(f, "(break {id}{name})", id = id.get())?;
        }
        StatementKind::Continue { name, id } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            writeln!(f, "(continue {id}{name})", id = id.get())?;
        }
    }

    Ok(())
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

pub(crate) struct PrefixPrintStatement<'a>(pub &'a Statement<'a>);
impl Display for PrefixPrintStatement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrefixPrintStatement(statement) = self;
        prefix_print_statement(statement, f)
    }
}
