use crate::code_generation::register::Register;
use crate::code_generation::register::Register::*;
use crate::compiling::ir::binop::{ArithmeticKind, Binop, BinopKind, BitwiseBinop, BitwiseKind};
use crate::compiling::ir::binop::{IntegerBinop, OrderingKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Lvalue, Opcode};
use std::cmp;
use std::fmt::Write;

const CALL_REGISTERS: &[Register] = &[Rdi, Rsi, Rdx, Rcx, R8, R9];

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

pub struct Codegen<'ir> {
    ir: &'ir IntermediateRepresentation<'ir>,
    pic: bool,
    output: String,

    current_function_args_offsets: Option<Vec<usize>>,
}

//noinspection SpellCheckingInspection
impl<'ir> Codegen<'ir> {
    pub fn new(ir: &'ir IntermediateRepresentation<'ir>, pic: bool) -> Self {
        Self {
            ir,
            output: String::new(),
            pic,
            current_function_args_offsets: None,
        }
    }

    pub fn generate_function(&mut self, function: &Function<'ir>) {
        let name = function.name.name;
        asm!(self, ".global {name}");
        asm!(self, ".p2align 4, 0x90"); // 0x90 is nop
        asm!(self, "{}:", name);
        comment!(self, "Function Prologue");
        asm!(self, "  pushq %rbp");
        asm!(self, "  movq %rsp, %rbp");
        let total_param_size = function.params.iter().sum::<usize>();
        let total_stack_size = function.stack_size + total_param_size;
        if total_stack_size > 0 {
            let final_stack_size = (total_stack_size.div_ceil(16) * 16) + 8;
            comment!(
                self,
                "Stack size: {total_stack_size}. Padding: {}",
                final_stack_size - total_stack_size
            );
            asm!(self, "  subq ${final_stack_size}, %rsp");
        }
        if !function.params.is_empty() {
            comment!(self, "Function Arguments");
        }
        // Fucntion argument initialization.

        // Function argument allocation.
        // Arguments are received at the end of function stack frame.
        // first argument counting from the right is last passed parameter
        // second argument counting from the right is second-to-last parameter.
        // etc.

        let mut function_args_offsets = Vec::with_capacity(function.params.len());
        let mut arg_offset = total_stack_size - total_param_size;
        let reg_arg_count = cmp::min(function.params.len(), CALL_REGISTERS.len()); // Amount of parameters passed by register.

        let reg_params = &function.params[..reg_arg_count];

        // Register parameters.
        for (i, arg_size) in reg_params.iter().enumerate() {
            let arg_size = *arg_size;
            arg_offset += arg_size;
            let p = Register::prefix_from_size(arg_size);
            asm!(
                self,
                "  mov{p} {}, -{}(%rbp)",
                CALL_REGISTERS[i].lower_bytes_register(arg_size),
                arg_offset
            );
            function_args_offsets.push(arg_offset);
        }

        // Stack Parameters.
        let stack_params = &function.params[reg_arg_count..];
        // X86-64 Stack layout:
        // -16       -8         0         8         16
        //  | * * * * | * * * * | * * * * | * * * * | * * * * | * * * * ...
        //                      ^         ^         ^
        //                   rbp       old rbp   ret addr
        //                             [0 - 8]   arg_read_offset
        //                                       [8 - 16]
        let mut arg_read_offset = 16;
        for arg_size in stack_params {
            arg_offset += arg_size;
            asm!(self, "  movq {arg_read_offset}(%rbp), {Rax}",);

            asm!(self, "  movq {Rax}, -{}(%rbp)", arg_offset);

            function_args_offsets.push(arg_offset);
            arg_read_offset += arg_size;
        }

        comment!(self);

        self.current_function_args_offsets = Some(function_args_offsets);
        for opcode in function.code {
            self.compile_opcode(opcode);
        }
        self.current_function_args_offsets = None;
        comment!(self, "Function Epilogue");
        asm!(self, "  movq %rbp, %rsp");
        asm!(self, "  popq %rbp");
        asm!(self, "  ret");
    }
    fn compile_opcode(&mut self, opcode: &Opcode<'ir>) {
        match opcode {
            Opcode::FunctionCall {
                callee,
                args,
                result,
                is_variadic,
            } => {
                comment!(self, "Function call");
                let register_args = args.iter().take(CALL_REGISTERS.len());

                for (i, arg) in register_args.enumerate() {
                    self.load_arg_to_reg(arg, CALL_REGISTERS[i]);
                }

                let stack_args = &args[cmp::min(CALL_REGISTERS.len(), args.len())..];
                for i in stack_args.iter().rev() {
                    self.push_arg(i);
                }

                if *is_variadic {
                    asm!(self, "  movb $0, %al"); // SysV requires to put amount of floating point arguments into %al when calling variadic function
                }
                self.call_arg(callee);
                asm!(self, "  movq %rax, -{}(%rbp)", result);
            }
            Opcode::Binop {
                left,
                right,
                destination: result,
                kind,
            } => {
                comment!(self, "Binop ({})", kind.to_ast_binop().operator());

                match kind {
                    Binop::Integer(IntegerBinop { signed, size, kind }) => {
                        self.load_arg_to_reg(left, Rax);
                        self.load_arg_to_reg(right, Rcx);
                        match kind {
                            BinopKind::Arithmetic(c) => {
                                use ArithmeticKind::*;
                                match c {
                                    Addition => asm!(self, "  addq %rcx, %rax"),
                                    Subtraction => asm!(self, "  subq %rcx, %rax"),
                                    Multiplication => asm!(self, "  imulq %rcx, %rax"),
                                    Division => {
                                        asm!(self, "  cqto");
                                        asm!(self, "  idivq %rcx");
                                    }
                                }
                                asm!(self, "  movq %rax, -{result}(%rbp)");
                            }
                            BinopKind::Ordering(c) => {
                                asm!(self, "  cmpq %rcx, %rax");
                                use OrderingKind::*;
                                match c {
                                    Equality => asm!(self, "  sete %al"),
                                    Inequality => asm!(self, "  setne %al"),
                                    GreaterThan => asm!(self, "  setg %al"),
                                    GreaterEq => asm!(self, "  setge %al"),
                                    LessThan => asm!(self, "  setl %al"),
                                    LessEq => asm!(self, "  setle %al"),
                                }
                                asm!(self, "  movb %al, -{result}(%rbp)");
                            }
                        }
                    }
                    Binop::Bitwise(BitwiseBinop {
                        kind,
                        is_logical_with_short_circuit,
                    }) => {
                        // In some scenarios compiler may load one operator before another. Like short-citcuiting
                        if !is_logical_with_short_circuit {
                            self.load_arg_to_reg(left, Al);
                        }
                        self.load_arg_to_reg(right, Bl);
                        let p = Register::prefix_from_size(left.size());
                        match kind {
                            BitwiseKind::Or => asm!(self, "  or{p} {Al}, {Bl}"),
                            BitwiseKind::And => asm!(self, "  and{p} {Al}, {Bl}"),
                        }
                        asm!(self, "mov{p} {Bl}, -{result}(%rbp)");
                    }
                }
            }

            Opcode::Negate {
                destination: result,
                item,
            } => {
                comment!(self, "Negate");
                self.load_arg_to_reg(item, Rax);
                asm!(self, "  negq %rax");
                asm!(self, "  movq %rax, -{result}(%rbp)");
            }
            Opcode::Assign {
                destination: result,
                source: item,
            } => {
                comment!(self, "Assign");
                let register = match item.size() {
                    8 => Rax, // TODO: introduce function for this.
                    1 => Al,
                    _ => unreachable!("unsupported size of operation"),
                };
                let p = register.prefix();
                self.load_arg_to_reg(item, register);
                asm!(self, "  mov{p} {register}, -{result}(%rbp)");
            }
            Opcode::Return(ret) => {
                if let Some(ret) = ret {
                    self.load_arg_to_reg(ret, Rax);
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
                let reg = match condition.size() {
                    8 => Rax,
                    1 => Al,
                    _ => unreachable!("unsupported size of operation"),
                };
                let p = reg.prefix();
                self.load_arg_to_reg(condition, reg);
                asm!(self, "  test{p} {reg}, {reg}");
                asm!(self, "  jz .label_{label}")
            }
            Opcode::Jmp { label } => {
                comment!(self, "Jmp");
                asm!(self, "  jmp .label_{label}");
            }
            Opcode::AddressOf {
                destination: result,
                lvalue,
            } => {
                comment!(self, "AddressOf");
                match lvalue {
                    Arg::Bool(_) | Arg::Int64 { .. } | Arg::String { .. } => panic!(
                        "COMPILER BUG: Could not take address of r-value. Typechecker failed."
                    ),
                    Arg::ExternalFunction(_) => todo!("Indirect functions"),

                    Arg::StackOffset { offset, size } => {
                        asm!(self, "  leaq -{offset}(%rbp), {Rax}");
                        self.store_reg_to_lvalue(*result, Rax)
                    }
                    Arg::Argument { index, size } => {
                        let offset = self.current_function_args_offsets.as_ref().unwrap()[*index];
                        asm!(self, "  leaq -{offset}(%rbp), {Rax}");
                        self.store_reg_to_lvalue(*result, Rax)
                    }
                }
            }
            Opcode::Store {
                destination,
                source,
            } => {
                comment!(self, "Store");
                let val_reg = Rax.lower_bytes_register(source.size());
                let p = val_reg.prefix();
                self.load_arg_to_reg(source, val_reg);
                self.load_arg_to_reg(&destination.to_arg(), Rcx);
                asm!(self, "  mov{p} {val_reg}, ({Rcx})");
            }
            Opcode::Load {
                destination,
                source,
            } => {
                comment!(self, "Load");
                assert_eq!(
                    source.size(),
                    8,
                    "COMPILER BUG: Could not dereference {e} byte address",
                    e = source.size()
                );
                let reg = Rax.lower_bytes_register(source.size());
                let p = reg.prefix();
                self.load_arg_to_reg(source, Rax);
                asm!(self, "  mov{p} ({Rax}), {reg}");
                self.store_reg_to_lvalue(*destination, reg);
            }
        }
        comment!(self);
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

    fn call_arg(&mut self, arg: &Arg<'ir>) {
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
                self.load_arg_to_reg(arg, Rax);
                asm!(self, "  call *%rax");
            }
        }
    }
}

impl<'ir> Codegen<'ir> {
    // TODO: Factor out common logic between load_arg_to_reg() and push_arg()

    pub fn store_reg_to_lvalue(&mut self, lvalue: Lvalue, reg: Register) {
        let p = reg.prefix();
        match lvalue {
            Lvalue::StackOffset { offset, size } => {
                asm!(self, "  mov{p} {reg},  -{offset}(%rbp)")
            }
            Lvalue::Argument { index, size } => {
                let offset = self
                    .current_function_args_offsets
                    .as_ref()
                    .expect("COMPILER BUG: Invalid Argument Lvalue")[index];
                asm!(self, "  mov{p} {reg},  -{offset}(%rbp)")
            }
        }
    }
    pub fn load_arg_to_reg(&mut self, arg: &Arg<'ir>, reg: Register) {
        let p = reg.prefix();
        match arg {
            Arg::Int64 { bits, signed } => {
                assert!(signed, "TODO: Unsigned."); // TODO
                let value = i64::from_le_bytes(*bits);
                asm!(self, "  mov{p} ${}, {reg}", value);
            }
            Arg::Bool(bool) => {
                asm!(self, "  mov{p} ${}, {reg}", *bool as i32);
            }
            Arg::ExternalFunction(name) => {
                if self.pic {
                    asm!(self, "  mov{p} {}@GOTPCREL(%rip), {reg}", name.name)
                } else {
                    asm!(self, "  mov{p} {}, {reg}", name.name,);
                }
            }
            Arg::StackOffset { offset, size } => {
                asm!(self, "  mov{p} -{offset}(%rbp), {reg}");
            }
            Arg::String { index } => {
                if self.pic {
                    asm!(self, "  lea{p} .STR{index}(%rip), {reg}")
                } else {
                    asm!(self, "  mov{p} $.STR{index}, {reg}")
                }
            }
            Arg::Argument { index, size } => {
                let offset = self.current_function_args_offsets.as_ref().unwrap()[*index];
                asm!(self, "  mov{p} -{offset}(%rbp), {reg}");
            }
        }
    }

    fn push_arg(&mut self, arg: &Arg<'ir>) {
        let p = Register::prefix_from_size(arg.size());
        match arg {
            Arg::Int64 { bits, signed } => {
                assert!(signed, "TODO: Unsigned."); // TODO
                let value = i64::from_le_bytes(*bits);
                asm!(self, "  push{p} ${}", value);
            }
            Arg::Bool(bool) => {
                asm!(self, "  push{p} ${}", *bool as i32);
            }
            Arg::ExternalFunction(name) => {
                let name = name.name;
                if self.pic {
                    asm!(self, "  push{p} {name}@GOTPCREL(%rip)")
                } else {
                    asm!(self, "  push{p} {name}");
                }
            }
            Arg::StackOffset { offset, size } => {
                asm!(self, "  push{p} -{offset}(%rbp)");
            }
            Arg::String { index } => {
                if self.pic {
                    asm!(self, "  lea{p} .STR{index}(%rip), {Rax}");
                    asm!(self, "  push{p} {Rax}")
                } else {
                    asm!(self, "  push{p} $.STR{index}")
                }
            }
            Arg::Argument { index, size } => {
                let offset = self.current_function_args_offsets.as_ref().unwrap()[*index];
                asm!(self, "  push{p} -{offset}(%rbp)");
            }
        }
    }
}
