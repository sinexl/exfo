use crate::analysis::r#type::DisplayType;
use crate::analysis::type_context::TypeCtx;
use crate::ast::expression::{Expression, ExpressionKind};
use crate::ast::statement::{
    DisplayFunctionParameter, ExternalFunction, FunctionDeclaration, VariableDeclaration,
};
use crate::ast::statement::{Statement, StatementKind};
use crate::common::Join;
use std::fmt::{Display, Formatter, Write};

pub fn print_ast<'ast, 'types>(
    expr: &'ast Expression<'ast>,
    types: &'types TypeCtx<'types>,
    f: &mut impl Write,
    indent: usize,
) -> std::fmt::Result {
    let tab = " ".repeat((indent + 1) * 2);
    write!(f, "<{}> ", DisplayType(expr.ty.inner(), types))?;
    match &expr.kind {
        ExpressionKind::Literal(value) => writeln!(f, "Literal({})", value),
        ExpressionKind::VariableAccess(identifier, id) => {
            writeln!(f, "Access({}({id}))", identifier.name)
        }
        ExpressionKind::Binop { left, right, kind } => {
            writeln!(f, "{}", kind.name())?;
            write!(f, "{tab}left = {}", PrintExpression(left, types, indent + 1))?;
            write!(f, "{tab}right = {}", PrintExpression(right, types, indent + 1))
        }
        ExpressionKind::Assignment {
            target: taget,
            value,
        } => {
            writeln!(f, "Assignment")?;
            write!(f, "{tab}target = {}", PrintExpression(taget, types, indent + 1))?;
            write!(f, "{tab}value  = {}", PrintExpression(value, types, indent + 1))
        }
        ExpressionKind::Unary { item, operator } => {
            writeln!(f, "{}", operator.name())?;
            write!(f, "{tab}item = {}", PrintExpression(item, types, indent + 1))
        }
        ExpressionKind::FunctionCall { callee, arguments } => {
            writeln!(f, "Call")?;
            write!(f, "{tab}callee = {}", PrintExpression(callee, types, indent + 1))?;
            writeln!(f, "{tab}arguments = ")?;
            let tab = " ".repeat((indent + 2) * 2);
            for arg in *arguments {
                write!(
                    f,
                    "{tab}{}",
                    PrintExpression(
                        unsafe { arg.as_ref().expect("FunctionCall: null argument") },
                        types,
                        indent + 2
                    )
                )?;
            }
            Ok(())
        }
    }
}

pub fn print_statement<'ast, 'types>(
    stmt: &Statement<'ast, 'types>,
    types: &'types TypeCtx<'types>,
    f: &mut impl Write,
    indent: usize,
) -> std::fmt::Result {
    let tab = " ".repeat((indent + 1) * 2);
    match &stmt.kind {
        StatementKind::ExpressionStatement(expr) => {
            writeln!(f, "Expression Statement")?;
            write!(f, "{tab}{}", PrintExpression(expr, types, indent + 1))?;
        }
        StatementKind::FunctionDeclaration(FunctionDeclaration {
            name,
            body,
            parameters,
            return_type,
        }, id) => {
            writeln!(
                f,
                "Func `{}`({id}) ({}): {}",
                name.name,
                Join(
                    parameters
                        .iter()
                        .map(|e| DisplayFunctionParameter(e, types)),
                    ", "
                ),
                DisplayType(return_type.inner(), types)
            )?;
            for statement in *body {
                write!(f, "{tab}{}", PrintStatement(statement, types, indent + 1))?;
            }
        }

        StatementKind::Extern(ExternalFunction {
            name,
            kind,
            parameters,
            return_type,
            is_variadic,
        }, id) => {
            writeln!(
                f,
                "Extern \"{kind:?}\" `{}`({id}) ({}{}): {}",
                name.name,
                Join(parameters.iter().map(|e| DisplayType(e.inner(), types)), ", "),
                if *is_variadic { ", ..." } else { "" },
                DisplayType(return_type.inner(), types)
            )?;
        }
        StatementKind::Block(statements) => {
            writeln!(f, "Block")?;
            for statement in *statements {
                write!(f, "{tab}{}", PrintStatement(statement, types, indent + 1))?;
            }
        }
        StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            initializer,
            ty,
        }, id) => {
            writeln!(f, "Variable `{}`({id}): {}", name.name, DisplayType(ty.inner(), types))?;
            if let Some(init) = initializer {
                write!(f, "{tab}Initializer = {init}", init = TreePrintExpression(init, types))?;
            }
        }
        StatementKind::Return(ret) => {
            writeln!(f, "Return")?;
            if let Some(ret) = ret {
                write!(f, "{tab}{ret}", ret = TreePrintExpression(ret, types))?;
            }
        }
        StatementKind::If {
            condition,
            then,
            r#else,
        } => {
            writeln!(f, "If")?;
            write!(f, "{tab}condition = {condition}", condition = TreePrintExpression(condition, types))?;
            write!(
                f,
                "{tab}do = \n{then}",
                then = PrintStatement(then, types, indent + 1)
            )?;

            if let Some(r#else) = r#else {
                write!(f, "else = \n{}", PrintStatement(r#else, types, indent + 1))?;
            }
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
            writeln!(f, "While ({id}{name})", id = id.get())?;
            write!(f, "{tab}condition = {condition}", condition = TreePrintExpression(condition, types))?;
            write!(
                f,
                "{tab}do = \n{then}",
                then = PrintStatement(body, types, indent + 1)
            )?;
        }
        StatementKind::Break { name, id } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            writeln!(f, "Break ({id}{name})", id = id.get())?;
        }
        StatementKind::Continue { name, id } => {
            let name = if let Some(name) = name {
                format!(", {name}")
            } else {
                "".to_string()
            };
            writeln!(f, "Continue ({id}{name})", id = id.get())?
        }
    }
    Ok(())
}

pub(crate) struct PrintExpression<'ast, 'types>(pub &'ast Expression<'ast>, pub &'types TypeCtx<'types>, pub usize);
impl<'ast,  'types> Display for PrintExpression<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrintExpression(expr, types, ind) = self;
        print_ast(expr, types, f, *ind)
    }
}

struct TreePrintExpression<'ast, 'types>(pub &'ast Expression<'ast>, pub &'types TypeCtx<'types>);

impl<'ast, 'types> Display for TreePrintExpression<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        PrintExpression(self.0, self.1, 0).fmt(f)
    }
}
pub(crate) struct PrintStatement<'ast, 'expr, 'types>(
    pub &'expr Statement<'ast, 'types>,
    pub &'types TypeCtx<'types>,
    pub usize,
);
impl<'ast, 'expr, 'types> Display for PrintStatement<'ast, 'expr, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let PrintStatement(statement, types, ind) = self;
        print_statement(statement, types, f, *ind)
    }
}

pub struct DisplayStatement<'ast, 'types>(
    pub &'ast Statement<'ast, 'types>,
    pub &'types TypeCtx<'types>,
);
impl<'ast, 'types> Display for DisplayStatement<'ast, 'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        PrintStatement(self.0, self.1, 0).fmt(f)
    }
}
