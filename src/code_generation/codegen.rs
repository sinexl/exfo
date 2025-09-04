use crate::ast::expression::AstLiteral;
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use std::fmt::Write;
use crate::ast::binop::BinopKind;

pub const CALL_REGISTERS: &[&'static str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
pub struct Codegen<'a> {
    ir: &'a IntermediateRepresentation<'a>,
    output: String,
}

macro_rules! asm {
    ($dst:expr, $($arg:tt)*) => {
        $dst.output.write_fmt(format_args!($($arg)*)).unwrap()
    };
}

//noinspection SpellCheckingInspection
impl<'a> Codegen<'a> {
    pub fn new(ir: &'a IntermediateRepresentation<'a>) -> Self {
        Self {
            ir,
            output: String::new(),
        }
    }

    pub fn generate_function(&mut self, function: &Function<'a>) {
        let name = function.name.name;
        asm!(self, ".global {name}");
        asm!(self, ".global {name}");
        asm!(self, ".p2align 4, 0x90"); // 0x90 is nop
        asm!(self, "{}:", name);
        asm!(self, "  pushq %rbp");
        asm!(self, "  movq %rsp, %rbp");
        if function.stack_size > 0 {
            asm!(self, "  subq ${size}, %rsp", size = function.stack_size);
        }
        for opcode in function.code {
            match opcode {
                Opcode::FunctionCall { callee, args } => {
                    if args.len() > CALL_REGISTERS.len() {
                        todo!()
                    }

                    for (i, arg) in args.iter().enumerate() {
                        self.load_arg_to_reg(arg, CALL_REGISTERS[i]);
                    }

                    self.call_arg(callee)
                }
                Opcode::Binop {
                    left,
                    right,
                    result,
                    kind,
                } => {
                    self.load_arg_to_reg(left, "rax");
                    self.load_arg_to_reg(right, "rcx");
                    match kind {
                        BinopKind::Addition => asm!(self, "  addq %rcx, %rax"),
                        BinopKind::Subtraction => asm!(self, "  subq %rcx, %rax"),

                        BinopKind::Multiplication | BinopKind::Division | BinopKind::Equality
                        | BinopKind::Inequality | BinopKind::GreaterThan | BinopKind::GreaterEq |
                        BinopKind::LessThan | BinopKind::LessEq => todo!()
                    }
                    asm!(self, "  movq %rax, -{result}(%rbp)");
                }
            }
        }
        asm!(self, "  movq %rbp, %rsp");
        asm!(self, "  popq %rbp");
        asm!(self, "  ret");
    }

    pub fn load_arg_to_reg(&mut self, arg: &Arg<'a>, reg: &str) {
        match arg {
            Arg::Literal(literal) => match literal {
                AstLiteral::Integral(value) => {
                    asm!(self, "  movq ${}, %{}", value, reg);
                }
                AstLiteral::FloatingPoint(_) => todo!(),
            },
            Arg::ExternalFunction(_) => todo!(),
            Arg::StackOffset(offset) => {
                asm!(self, "  movq -{offset}(%rbp), %{reg}");
            }
        }
    }

    pub fn generate(mut self) -> String {
        asm!(self, ".section .text");
        for (k, v) in &self.ir.functions {
            self.generate_function(v);
        }
        self.output
    }

    fn call_arg(&mut self, arg: &Arg<'a>) {
        match arg {
            Arg::ExternalFunction(name) => {
                asm!(self, "  call {}", name.name);
            }
            _ => todo!(),
        }
    }
}
