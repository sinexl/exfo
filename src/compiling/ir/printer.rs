use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
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
    writeln!(f, "{name}:", name = function.name.name)?;
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
        } => {
            let args_str = args
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            match callee {
                Arg::ExternalFunction(id) => {
                    write!(f, "{tab}stack[{result}] = call(\"{name}\"", name = id.name)?
                }
                arg => write!(f, "{tab}call({arg}")?,
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
            "{tab}stack[{}] = {left} {op} {right}",
            result,
            op = kind.operator()
        )?,
        Opcode::Negate { result, item } => writeln!(f, "{tab}stack[{result}] = -{item}")?,

        Opcode::Assign { result, arg: item } => writeln!(f, "{tab}stack[{result}] = {item}")?,
        Opcode::Return(ret) => {
            write!(f, "{tab}return")?;
            if let Some(ret) = ret {
                write!(f, " {ret}")?;
            }
            writeln!(f, ";")?;
        }
    }

    Ok(())
}

pub fn print_arg(arg: &Arg, f: &mut impl Write) -> std::fmt::Result {
    match arg {
        Arg::Int64 { bits, signed } => {
            let literal = match *signed {
                true => i64::from_le_bytes(*bits).to_string(),
                false => u64::from_le_bytes(*bits).to_string(),
            };
            write!(f, "{literal}")?;
        }
        Arg::ExternalFunction(id) => {
            write!(f, "external fn(\"{name}\")", name = id.name)?;
        }
        Arg::StackOffset { offset, size: _ } => {
            write!(f, "stack[{offset}]")?;
        }
    }

    Ok(())
}

impl<'a> Display for Arg<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_arg(self, f)
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
