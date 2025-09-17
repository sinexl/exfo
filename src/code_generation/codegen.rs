use crate::ast::binop::BinopKind;
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use std::fmt::Write;

pub const CALL_REGISTERS: &[&'static str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

macro_rules! asm {
    ($dst:expr, $($fmt:tt)*) => {
        writeln!($dst.output, $($fmt)*).unwrap()
    };
}

macro_rules! comment {
    ($dst:expr, $($fmt:tt)*) => {{
        write!($dst.output, "  // ").unwrap();
        writeln!($dst.output, $($fmt)*).unwrap();
    }};

    ($dst:expr) => {
        writeln!($dst.output).unwrap()
    };
}

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
        asm!(self, ".global {name}");
        asm!(self, ".global {name}");
        asm!(self, ".p2align 4, 0x90"); // 0x90 is nop
        asm!(self, "{}:", name);
        comment!(self, "Function Prologue");
        asm!(self, "  pushq %rbp");
        asm!(self, "  movq %rsp, %rbp");
        if function.stack_size > 0 {
            asm!(self, "  subq ${size}, %rsp", size = function.stack_size);
        }
        comment!(self);
        for opcode in function.code {
            match opcode {
                Opcode::FunctionCall {
                    callee,
                    args,
                    result,
                } => {
                    comment!(self, "Function call");
                    if args.len() > CALL_REGISTERS.len() {
                        todo!()
                    }

                    for (i, arg) in args.iter().enumerate() {
                        self.load_arg_to_reg(arg, CALL_REGISTERS[i]);
                    }

                    self.call_arg(callee);
                    asm!(self, "  movq %rax, -{}(%rbp)", result);
                }
                Opcode::Binop {
                    left,
                    right,
                    result,
                    kind,
                } => {
                    comment!(self, "Binop ({})", kind.operator());
                    self.load_arg_to_reg(left, "rax");
                    self.load_arg_to_reg(right, "rcx");
                    match kind {
                        BinopKind::Addition => asm!(self, "  addq %rcx, %rax"),
                        BinopKind::Subtraction => asm!(self, "  subq %rcx, %rax"),
                        BinopKind::Multiplication => asm!(self, "  imulq %rcx, %rax"),
                        BinopKind::Division => {
                            asm!(self, "  cqto");
                            asm!(self, "  idivq %rcx");
                        }

                        BinopKind::Equality
                        | BinopKind::Inequality
                        | BinopKind::GreaterThan
                        | BinopKind::GreaterEq
                        | BinopKind::LessThan
                        | BinopKind::LessEq => todo!(),
                    }
                    asm!(self, "  movq %rax, -{result}(%rbp)");
                }
                Opcode::Negate { result, item } => {
                    comment!(self, "Negate");
                    self.load_arg_to_reg(item, "rax");
                    asm!(self, "  negq %rax");
                    asm!(self, "  movq %rax, -{result}(%rbp)");
                }
                Opcode::Assign { result, arg: item } => {
                    comment!(self, "Assign");
                    self.load_arg_to_reg(item, "rax");
                    asm!(self, "  movq %rax, -{result}(%rbp)");
                }
            }
            comment!(self);
        }
        comment!(self, "Function Epilogue");
        asm!(self, "  movq %rbp, %rsp");
        asm!(self, "  popq %rbp");
        asm!(self, "  ret");
    }

    pub fn load_arg_to_reg(&mut self, arg: &Arg<'a>, reg: &str) {
        match arg {
            Arg::Int64{bits, signed} => {
                assert!(signed); // TODO 
                let value = i64::from_le_bytes(*bits); 
                asm!(self, "  movq ${}, %{}", value, reg);
            }
            Arg::ExternalFunction(_) => todo!(),
            Arg::StackOffset { offset, size: _ } => {
                asm!(self, "  movq -{offset}(%rbp), %{reg}");
            }
        }
    }

    pub fn generate(mut self) -> String {
        asm!(self, ".section .text");
        for (_, v) in &self.ir.functions {
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
