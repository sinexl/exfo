use crate::analysis::r#type::{DisplayType, TypeCtx};
use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    DisplayFunctionParameter, ExternalFunction, FunctionDeclaration, Statement, StatementKind,
    VariableDeclaration,
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
            let mut temp: Vec<&Expression> = Vec::with_capacity(arguments.len() + 1);
            temp.push(*callee);
            for arg in arguments.iter() {
                temp.push(unsafe { arg.as_ref().expect("FunctionCall: null argument") });
            }
            parenthesize(f, "call", temp.as_slice())
        }
    }
}

pub fn prefix_print_statement<'ast, 'types>(
    statement: &Statement<'ast, 'types>,
    types: &'types TypeCtx<'types>,
    f: &mut impl Write,
) -> std::fmt::Result {
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
                Join(
                    parameters
                        .iter()
                        .map(|e| DisplayFunctionParameter(e, types)),
                    ", "
                ),
                DisplayType(return_type.inner(), types)
            )?;
            if !body.is_empty() {
                writeln!(f)?;
                for statement in *body {
                    writeln!(f, "{tab}{}", PrefixPrintStatement(statement, types))?;
                }
            }
            writeln!(f, ")")?;
        }

        StatementKind::Extern(ExternalFunction {
            name,
            kind,
            parameters,
            return_type,
            is_variadic,
        }) => {
            write!(
                f,
                "(extern \"{kind:?}\" `{}` ({}{}): {}",
                name.name,
                Join(parameters.iter().map(|e| DisplayType(e.inner(), types)), ", "),
                if *is_variadic { ", ..." } else { "" },
                DisplayType(return_type.inner(), types)
            )?;
        }
        StatementKind::Block(body) => {
            writeln!(f, "(block")?;
            if !body.is_empty() {
                writeln!(f)?;
                for statement in *body {
                    writeln!(f, "{tab}{}", PrefixPrintStatement(statement, types))?;
                }
            }
            writeln!(f, ")")?;
        }
        StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            initializer,
            ty,
        }) => {
            write!(f, "(`{}`: {} ", name.name, DisplayType(ty.inner(), types))?;
            if let Some(initializer) = initializer {
                write!(f, "= {}", PrefixPrint(initializer))?;
            }
            writeln!(f, ")")?;
        }
        StatementKind::Return(ret) => {
            write!(f, "(return")?;
            if let Some(ret) = ret {
                write!(f, " {ret}", ret = PrefixPrint(ret))?;
            }
            writeln!(f, ")")?;
        }
        StatementKind::If {
            condition,
            then,
            r#else,
        } => {
            write!(
                f,
                "(if {condition}: {then}",
                condition = PrefixPrint(condition),
                then = PrefixPrintStatement(then, types)
            )?;
            if let Some(r#else) = r#else {
                write!(f, "else: {r}", r = PrefixPrintStatement(r#else, types))?;
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
            write!(
                f,
                "(({id}{name}): while {condition}: {body}",
                condition = PrefixPrint(condition),
                body = PrefixPrintStatement(body, types),
                id = id.get()
            )?;
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

pub(crate) struct PrefixPrintStatement<'ast, 'types>(
    pub &'ast Statement<'ast, 'types>,
    pub &'types TypeCtx<'types>,
);
impl<'ast, 'types> Display for PrefixPrintStatement<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrefixPrintStatement(statement, types) = self;
        prefix_print_statement(statement, types, f)
    }
}
