use crate::ast::binop::BinopKind;
use crate::code_generation::register::Register;
use crate::code_generation::register::Register::*;
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Opcode};
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

pub struct Codegen<'a> {
    ir: &'a IntermediateRepresentation<'a>,
    pic: bool,
    output: String,

    current_function_args_offsets: Option<Vec<usize>>,
}

//noinspection SpellCheckingInspection
impl<'a> Codegen<'a> {
    pub fn new(ir: &'a IntermediateRepresentation<'a>, pic: bool) -> Self {
        Self {
            ir,
            output: String::new(),
            pic,
            current_function_args_offsets: None,
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
        let total_stack_size = function.stack_size + function.params_.iter().sum::<usize>();
        if total_stack_size > 0 {
            asm!(self, "  subq ${total_stack_size}, %rsp");
        }
        if !function.params_.is_empty() {
            comment!(self, "Function Arguments");
        }
        let mut function_args_offsets = Vec::with_capacity(function.params_.len());
        let mut stack_initializer_offset = 0;
        let reg_arg_count = cmp::min(function.params_.len(), CALL_REGISTERS.len()); // Amount of registers passed by register.
        #[allow(clippy::needless_range_loop)]
        for i in 0..reg_arg_count {
            stack_initializer_offset += function.params_[i];
            asm!(
                self,
                "  movq {}, -{}(%rbp)",
                CALL_REGISTERS[i],
                stack_initializer_offset
            );
            function_args_offsets.push(stack_initializer_offset);
        }
        dbg!(&function.params_, &function.params_.len());
        dbg!(&CALL_REGISTERS.len());
        let stack_params = &function.params_[reg_arg_count..];
        dbg!(stack_params);

        // -16       -8         0         8         16
        //  | * * * * | * * * * | * * * * | * * * * | * * * * | * * * * ...
        //                      ^         ^         ^
        //                   rbp       old rbp   ret addr
        //                             (0 - 8)   (8 - 16)
        let mut callarg_read_offset = 16;
        for i in stack_params {
            stack_initializer_offset += i;
            asm!(self, "  movq {callarg_read_offset}(%rbp), {Rax}",);

            asm!(self, "  movq {Rax}, -{}(%rbp)", stack_initializer_offset);
            function_args_offsets.push(stack_initializer_offset);
            callarg_read_offset += i;
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
    fn compile_opcode(&mut self, opcode: &Opcode<'a>) {
        match opcode {
            Opcode::FunctionCall {
                callee,
                args,
                result,
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
                self.load_arg_to_reg(left, Rax);
                self.load_arg_to_reg(right, Rcx);
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
                self.load_arg_to_reg(item, Rax);
                asm!(self, "  negq %rax");
                asm!(self, "  movq %rax, -{result}(%rbp)");
            }
            Opcode::Assign { result, arg: item } => {
                comment!(self, "Assign");
                let register = match item.size() {
                    8 => Rax,
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
                self.load_arg_to_reg(condition, Register::Al);
                asm!(self, "  test %rax, %rax");
                asm!(self, "  jz .label_{label}")
            }
            Opcode::Jmp { label } => {
                comment!(self, "Jmp");
                asm!(self, "  jmp .label_{label}");
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
                self.load_arg_to_reg(arg, Rax);
                asm!(self, "  call *%rax");
            }
        }
    }
}

impl<'a> Codegen<'a> {
    // TODO: Factor out common logic between load_arg_to_reg() and push_arg()
    pub fn load_arg_to_reg(&mut self, arg: &Arg<'a>, reg: Register) {
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
                asm!(self, "  mov{p} -{offset}(%rbp), {Rax}");
                asm!(self, "  mov{p} {Rax}, {reg}");
            }
        }
    }

    fn push_arg(&mut self, arg: &Arg<'a>) {
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
