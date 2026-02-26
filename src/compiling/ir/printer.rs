use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Lvalue, Opcode, Rvalue};
use std::fmt::{Display, Formatter, Write};

pub fn print_ir(ir: &IntermediateRepresentation, f: &mut impl Write) -> std::fmt::Result {
    if !&ir.functions.is_empty() {
        for (_, func) in &ir.functions {
            print_function(func, f)?;
        }
    } else {
        writeln!(f, "    <empty>")?;
    }

    Ok(())
}

pub fn print_function(function: &Function<'_>, f: &mut impl Write) -> std::fmt::Result {
    writeln!(
        f,
        "{name}: stack size = {stack_size}, params = {params} (size = {params_size})",
        name = function.name.name,
        stack_size = function.stack_size,
        params = function.params.len(),
        params_size = function.params.iter().sum::<usize>(),
    )?;
    for opcode in function.code {
        print_opcode(opcode, f, 1)?;
    }

    Ok(())
}

pub fn print_opcode(opcode: &Opcode, f: &mut impl Write, indent: usize) -> std::fmt::Result {
    let tab = "    ".repeat(indent);
    match opcode {
        Opcode::FunctionCall {
            callee,
            args,
            result,
            is_variadic,
        } => {
            let args_str = args
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            match callee {
                Arg::RValue(Rvalue::ExternalFunction(id)) => {
                    write!(f, "{tab}")?;
                    if let Some(result) = result {
                        write!(f, "{result} = ")?;
                    }
                    write!(
                        f,
                        "{var}call(\"{name}\"",
                        var = if *is_variadic { "var" } else { "" },
                        name = id.name
                    )?
                }
                arg => write!(
                    f,
                    "{tab}{}call({arg}",
                    if *is_variadic { "var" } else { "" }
                )?,
            }
            if !args.is_empty() {
                write!(f, ", {args_str}")?;
            }
            writeln!(f, ")")?;
        }
        Opcode::Binop {
            left,
            right,
            kind,
            result,
        } => writeln!(
            f,
            "{tab}{result} = {left} {op} {right}",
            op = kind.to_ast_binop().operator()
        )?,
        Opcode::Negate { result, item } => writeln!(f, "{tab}{result} = -{item}")?,

        Opcode::Assign { result, source } => writeln!(f, "{tab}{result} = {source}")?,
        Opcode::Return(ret) => {
            write!(f, "{tab}return")?;
            if let Some(ret) = ret {
                write!(f, " {ret}")?;
            }
            writeln!(f, ";")?;
        }
        Opcode::Label { index } => {
            writeln!(f, ".label{index}:")?;
        }
        Opcode::JmpIfNot { label, condition } => {
            writeln!(f, "{tab}jmp_if_not {condition} -> .label{label}")?
        }
        Opcode::Jmp { label } => writeln!(f, "{tab}jmp -> .label{label}")?,
        Opcode::AddressOf { result, source } => writeln!(f, "{tab}{result} = &{source}")?,
        Opcode::Store { result, source } => writeln!(f, "{tab}*{result} = {source}")?,
        Opcode::Load { result, source } => writeln!(f, "{tab}{result} = *{source}")?,
    }

    Ok(())
}

pub fn print_lvalue(lvalue: &Lvalue, f: &mut impl Write) -> std::fmt::Result {
    match lvalue {
        Lvalue::StackOffset { offset, size: _ } => write!(f, "stack[{offset}]")?,
        Lvalue::Argument { index, size } => write!(f, "argument[{index}]")?,
    };
    Ok(())
}
pub fn print_rvalue(rvalue: &Rvalue, f: &mut impl Write) -> std::fmt::Result {
    match rvalue {
        Rvalue::Int64 { bits, signed } => {
            let literal = match *signed {
                true => i64::from_le_bytes(*bits).to_string(),
                false => u64::from_le_bytes(*bits).to_string(),
            };
            write!(f, "{literal}")?;
        }
        Rvalue::Bool(bool) => write!(f, "{}", bool)?,
        Rvalue::ExternalFunction(id) => {
            write!(f, "external fn(\"{name}\")", name = id.name)?;
        }
        Rvalue::String { index } => write!(f, "string[{index}]")?,
        Rvalue::Void => write!(f, "void")?,
    };
    Ok(())
}
pub fn print_arg(arg: &Arg, f: &mut impl Write) -> std::fmt::Result {
    match arg {
        Arg::LValue(lvalue) => print_lvalue(lvalue, f)?,
        Arg::RValue(rvalue) => print_rvalue(rvalue, f)?,
    }

    Ok(())
}

impl<'a> Display for Arg<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_arg(self, f)
    }
}

impl<'a> Display for Lvalue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_lvalue(self, f)
    }
}

impl<'a> Display for Rvalue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_rvalue(self, f)
    }
}

impl<'a> Display for Opcode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_opcode(self, f, 0)
    }
}
impl<'a> Display for Function<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_function(self, f)
    }
}

impl<'a> Display for IntermediateRepresentation<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_ir(self, f)
    }
}
