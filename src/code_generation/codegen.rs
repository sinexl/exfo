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
        writeln!(self.output, ".global {name}").unwrap();
        writeln!(self.output, ".p2align 4, 0x90").unwrap(); // 0x90 is nop
        writeln!(self.output, "{}:", name).unwrap();
        writeln!(self.output, "  pushq %rbp").unwrap();
        writeln!(self.output, "  movq %rsp, %rbp").unwrap();
        if function.stack_size > 0 {
            writeln!(self.output, "  subq ${size}, %rsp", size = function.stack_size).unwrap();
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
                        BinopKind::Addition => writeln!(self.output, "  addq %rcx, %rax").unwrap(),
                        BinopKind::Subtraction => writeln!(self.output, "  subq %rcx, %rax").unwrap(),

                        BinopKind::Multiplication | BinopKind::Division | BinopKind::Equality
                        | BinopKind::Inequality | BinopKind::GreaterThan | BinopKind::GreaterEq |
                        BinopKind::LessThan | BinopKind::LessEq => todo!()
                    }
                    writeln!(self.output, "  movq %rax, -{result}(%rbp)").unwrap();
                }
            }
        }
        writeln!(self.output, "  movq %rbp, %rsp").unwrap();
        writeln!(self.output, "  popq %rbp").unwrap();
        writeln!(self.output, "  ret").unwrap();
    }

    pub fn load_arg_to_reg(&mut self, arg: &Arg<'a>, reg: &str) {
        match arg {
            Arg::Literal(literal) => match literal {
                AstLiteral::Integral(value) => {
                    writeln!(self.output, "  movq ${}, %{}", value, reg).unwrap();
                }
                AstLiteral::FloatingPoint(_) => todo!(),
            },
            Arg::ExternalFunction(_) => todo!(),
            Arg::StackOffset(offset) => {
                writeln!(self.output, "  movq -{offset}(%rbp), %{reg}").unwrap();
            }
        }
    }

    pub fn generate(mut self) -> String {
        writeln!(self.output, ".section .text").unwrap();
        for (k, v) in &self.ir.functions {
            self.generate_function(v);
        }
        self.output
    }

    fn call_arg(&mut self, arg: &Arg<'a>) {
        match arg {
            Arg::ExternalFunction(name) => {
                writeln!(self.output, "  call {}", name.name).unwrap();
            }
            _ => todo!(),
        }
    }
}
