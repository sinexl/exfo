use crate::analysis::r#type::DisplayType;
use crate::analysis::type_context::TypeCtx;
use crate::ast::expression::{Expression, ExpressionKind, UnaryKind};
use crate::ast::statement::{
    DisplayFunctionParameter, ExternalFunction, FunctionDeclaration, Statement, StatementKind,
    VariableDeclaration,
};
use crate::common::Join;
use std::fmt::{Display, Formatter, Write};


pub fn prefix_print(expr: &Expression<'_>, f: &mut impl Write) -> std::fmt::Result {
    match &expr.kind {
        ExpressionKind::Binop { left, right, kind } => {
            let op = kind.operator();
            parenthesize(f, op, &[left, right])
        }
        ExpressionKind::Unary { item, operator } => {
            let op = match operator {
                UnaryKind::Negation => "-",
                UnaryKind::Dereferencing => "*",
                UnaryKind::AddressOf => "&",
            };
            parenthesize(f, op, &[item])
        }
        ExpressionKind::Assignment { target, value } => parenthesize(f, "=", &[target, value]),
        ExpressionKind::Literal(lit) => write!(f, "{}", lit),
        ExpressionKind::VariableAccess(name, id) => write!(f, "({}<{id}>)", name.name),
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

pub const PREFIX_TAB: &str = "  ";

pub fn prefix_print_statement<'ast, 'types>(
    statement: &Statement<'ast, 'types>,
    types: &'types TypeCtx<'types>,
    depth: usize,
    f: &mut impl Write,
) -> std::fmt::Result {
    let tab = PREFIX_TAB.repeat(depth);
    match &statement.kind {
        StatementKind::ExpressionStatement(expr) => {
            write!(f, "{tab}{}", PrefixPrint(expr))?;
        }
        StatementKind::FunctionDeclaration(
            FunctionDeclaration {
                name,
                body,
                parameters,
                return_type,
            },
            id,
        ) => {
            write!(
                f,
                "{tab}(func `{}`({id}) ({}): {}",
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
                    writeln!(f, "{}", PrefixPrintStatementIndented(statement, types, depth + 1))?;
                }
            }
            write!(f, ")")?;
        }

        StatementKind::Extern(
            ExternalFunction {
                name,
                kind,
                parameters,
                return_type,
                is_variadic,
            },
            id,
        ) => {
            write!(
                f,
                "{tab}(extern \"{kind:?}\" `{}`({id}) ({}{}): {}",
                name.name,
                Join(
                    parameters.iter().map(|e| DisplayType(e.inner(), types)),
                    ", "
                ),
                if *is_variadic { ", ..." } else { "" },
                DisplayType(return_type.inner(), types)
            )?;
        }
        StatementKind::Block(body) => {
            writeln!(f, "{tab}(block")?;
            if !body.is_empty() {
                for statement in *body {
                    writeln!(f, "{}", PrefixPrintStatementIndented(statement, types, depth + 1))?;
                }
            }
            write!(f, "{tab})")?;
        }
        StatementKind::VariableDeclaration(
            VariableDeclaration {
                name,
                initializer,
                ty,
            },
            id,
        ) => {
            write!(
                f,
                "{tab}(`{}`({id}): {} ",
                name.name,
                DisplayType(ty.inner(), types)
            )?;
            if let Some(initializer) = initializer {
                write!(f, "= {}", PrefixPrint(initializer))?;
            }
            write!(f, ")")?;
        }
        StatementKind::Return(ret) => {
            write!(f, "{tab}(return")?;
            if let Some(ret) = ret {
                write!(f, " {ret}", ret = PrefixPrint(ret))?;
            }
            write!(f, ")")?;
        }
        StatementKind::If {
            condition,
            then,
            r#else,
        } => {
            writeln!(
                f,
                "{tab}(if {condition}\n{then}",
                condition = PrefixPrint(condition),
                then = PrefixPrintStatementIndented(then, types, depth + 1),
            )?;
            if let Some(r#else) = r#else {
                writeln!(f, "{tab}else\n{r}", r = PrefixPrintStatementIndented(r#else, types, depth + 1))?;
            }
            write!(f, "{tab})")?;
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
                "{tab}(({id}{name}): while {condition}: {body}",
                condition = PrefixPrint(condition),
                body = PrefixPrintStatement(body, types),
                id = id.get()
            )?;
            write!(f, ")")?;
        }
        StatementKind::Break { name, id } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            writeln!(f, "{tab}(break {id}{name})", id = id.get())?;
        }
        StatementKind::Continue { name, id } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            writeln!(f, "{tab}(continue {id}{name})", id = id.get())?;
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


pub(crate) struct PrefixPrintStatementIndented<'ast, 'types>(
    pub &'ast Statement<'ast, 'types>,
    pub &'types TypeCtx<'types>,
    pub usize,
);
impl<'ast, 'types> Display for PrefixPrintStatementIndented<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrefixPrintStatementIndented(statement, types, indent) = self;
        prefix_print_statement(statement, types, *indent, f)
    }
}
pub(crate) struct PrefixPrintStatement<'ast, 'types>(
    pub &'ast Statement<'ast, 'types>,
    pub &'types TypeCtx<'types>,
);
impl<'ast, 'types> Display for PrefixPrintStatement<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", PrefixPrintStatementIndented(self.0, self.1, 0))
    }
}
