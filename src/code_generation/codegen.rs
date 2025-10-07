use crate::ast::binop::BinopKind;
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
use std::fmt::Write;

pub const CALL_REGISTERS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

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
    pic: bool,
    output: String,
}

//noinspection SpellCheckingInspection
impl<'a> Codegen<'a> {
    pub fn new(ir: &'a IntermediateRepresentation<'a>, pic: bool) -> Self {
        Self {
            ir,
            output: String::new(),
            pic,
        }
    }

    pub fn generate_function(&mut self, function: &Function<'a>) {
        let name = function.name.name;
        asm!(self, ".global {name}");
        asm!(self, ".p2align 4, 0x90"); // 0x90 is nop
        asm!(self, "{}:", name);
        comment!(self, "Function Prologue");
        asm!(self, "  pushq %rbp");
        asm!(self, "  movq %rsp, %rbp");
        if function.stack_size > 0 {
            asm!(self, "  subq ${size}, %rsp", size = function.stack_size);
        }

        if !function.params.is_empty() {
            comment!(self, "Function Arguments");
        }
        assert!(function.params.len() <= CALL_REGISTERS.len()); // TODO: more function parameters 
        for (i, offset) in function.params.iter().enumerate() {
            asm!(self, "  movq %{}, -{}(%rbp)", CALL_REGISTERS[i], offset);
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
                        todo!("More function parameters")
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
                    match (kind, kind.is_logical()) {
                        (kind, false) => {
                            match kind {
                                BinopKind::Addition => asm!(self, "  addq %rcx, %rax"),
                                BinopKind::Subtraction => asm!(self, "  subq %rcx, %rax"),
                                BinopKind::Multiplication => asm!(self, "  imulq %rcx, %rax"),
                                BinopKind::Division => {
                                    asm!(self, "  cqto");
                                    asm!(self, "  idivq %rcx");
                                }
                                _ => unreachable!("is_logical() should be updated."),
                            }
                            asm!(self, "  movq %rax, -{result}(%rbp)");
                        }

                        (kind, true) => {
                            asm!(self, "  cmpq %rcx, %rax");
                            match kind {
                                BinopKind::Equality => asm!(self, "  sete %al"),
                                BinopKind::Inequality => asm!(self, "  setne %al"),
                                BinopKind::GreaterThan => asm!(self, "  setg %al"),
                                BinopKind::GreaterEq => asm!(self, "  setge %al"),
                                BinopKind::LessThan => asm!(self, "  setl %al"),
                                BinopKind::LessEq => asm!(self, "  setle %al"),

                                BinopKind::Addition
                                | BinopKind::Subtraction
                                | BinopKind::Multiplication
                                | BinopKind::Division => {
                                    unreachable!("is_logical() should be updated")
                                }
                            }
                            asm!(self, "  movb %al, -{result}(%rbp)");
                        }
                    }
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
                Opcode::Return(ret) => {
                    if let Some(ret) = ret {
                        self.load_arg_to_reg(ret, "rax");
                    }
                    asm!(self, "  movq %rbp, %rsp");
                    asm!(self, "  popq %rbp");
                    asm!(self, "  ret");
                }
                Opcode::Label { index } => {
                    asm!(self, ".label_{index}:")
                }
                Opcode::JmpIfNot { label, condition } => { 
                    comment!(self, "JmpIfNot");
                    self.load_arg_to_reg(condition, "rax");
                    asm!(self, "  test %rax, %rax"); 
                    asm!(self, "  jnz .label_{label}")
                }
                Opcode::Jmp { label } => {
                    comment!(self, "Jmp");
                    asm!(self, "  jmp .label_{label}");
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
            Arg::Int64 { bits, signed } => {
                assert!(signed); // TODO 
                let value = i64::from_le_bytes(*bits);
                asm!(self, "  movq ${}, %{}", value, reg);
            }
            Arg::Bool(bool) => {
                asm!(self, "  movq ${}, %{}", *bool as i32, reg);
            }
            Arg::ExternalFunction(name) => {
                if self.pic {
                    asm!(self, "  movq {}@GOTPCREL(%rip), %{reg}", name.name)
                } else {
                    asm!(self, "  movq {}, %{reg}", name.name,);
                }
            }
            Arg::StackOffset { offset, size: _ } => {
                asm!(self, "  movq -{offset}(%rbp), %{reg}");
            }
            Arg::String { index } => {
                if self.pic { 
                    asm!(self, "  leaq .STR{index}(%rip), %{reg}")
                } else { 
                    asm!(self, "  movq $.STR{index}, %{reg}")
                }
            }
        }
    }

    pub fn generate(mut self) -> String {
        asm!(self, ".section .text");
        for v in self.ir.functions.values() {
            self.generate_function(v);
        } 
        self.generate_data(); 
        self.output
    }

    fn generate_data(&mut self) {
        comment!(self, "--- Data Section ---");
        asm!(self, ".section .rodata"); 
        for (i, s) in self.ir.strings.iter().enumerate() { 
            asm!(self, ".STR{i}:");
            comment!(self, "{:?}", s);
            write!(self.output, "  .byte ").unwrap(); 
            for c in s.chars() { 
                write!(self.output, "0x{:02X}, ", c as u8).unwrap(); 
            }
            asm!(self, "0x00"); 
        }
    }
    
    fn call_arg(&mut self, arg: &Arg<'a>) {
        match arg {
            Arg::ExternalFunction(name) => {
                let name = if self.pic {
                    format!("{}@PLT", name.name)
                } else {
                    name.name.to_string()
                };
                asm!(self, "  call {}", name);
            }
            _ => {
                self.load_arg_to_reg(arg, "rax"); 
                asm!(self, "  call *%rax"); 
            }
        }
    }
}
