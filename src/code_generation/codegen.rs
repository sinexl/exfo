use crate::code_generation::register::Register;
use crate::code_generation::register::Register::*;
use crate::compiling::ir::binop::{ArithmeticKind, Binop, BinopKind, BitwiseBinop, BitwiseKind};
use crate::compiling::ir::binop::{IntegerBinop, OrderingKind};
use crate::compiling::ir::intermediate_representation::{Function, IntermediateRepresentation};
use crate::compiling::ir::opcode::{Arg, Lvalue, Opcode, Rvalue};
use std::cmp;
use std::fmt::{Display, Formatter, Write};

const CALL_REGISTERS: &[Register] = &[Rdi, Rsi, Rdx, Rcx, R8, R9];

macro_rules! assert_same {
    ($msg:expr, $first:expr $(, $rest:expr)+ $(,)?) => {{
        let first = $first;
        $(
            assert!(
                $first == $rest,
                "COMPILER BUG: Codegen: {} ({} != {})",
                $msg,
                $first,
                $rest
            );
        )+
        first
    }};
}

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

    // Stores parameters' offsets of the function
    arg_offsets: Option<Vec<usize>>,
}

//noinspection SpellCheckingInspection
impl<'ir> Codegen<'ir> {
    pub fn new(ir: &'ir IntermediateRepresentation<'ir>, pic: bool) -> Self {
        Self {
            ir,
            output: String::new(),
            pic,
            arg_offsets: None,
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
            if arg_size != 0 {
                let p = Register::prefix_from_size(arg_size);
                asm!(
                    self,
                    "  mov{p} {}, -{}(%rbp)",
                    CALL_REGISTERS[i].lower_bytes_register(arg_size),
                    arg_offset
                );
            }
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

        self.arg_offsets = Some(function_args_offsets);
        for opcode in function.code {
            self.compile_opcode(opcode);
        }
        self.arg_offsets = None;
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
                // TODO: Check for the function return value size.
                comment!(self, "Function call");
                let register_args = args.iter().take(CALL_REGISTERS.len());

                for (i, arg) in register_args.enumerate() {
                    self.load_arg_to_reg(arg, CALL_REGISTERS[i].lower_bytes_register(arg.size()));
                }

                let stack_args = &args[cmp::min(CALL_REGISTERS.len(), args.len())..];
                for i in stack_args.iter().rev() {
                    self.push_arg(i);
                }

                if *is_variadic {
                    asm!(self, "  movb $0, %al"); // SysV requires to put amount of floating point arguments into %al when calling variadic function
                }
                self.call_arg(callee);
                if let Some(result) = result {
                    let ret_arg = Rax.lower_bytes_register(result.size());
                    self.store_reg_to_lvalue(ret_arg, result);
                }
            }
            Opcode::Binop {
                left,
                right,
                result,
                kind,
            } => {
                comment!(self, "Binop ({})", kind.to_ast_binop().operator());

                match kind {
                    Binop::Integer(IntegerBinop { signed, size, kind }) => {
                        if !*signed {
                            todo!("Unsigned integer binop")
                        }
                        assert_same!(
                            "Binop::Integer: Result size and operation size must be the same",
                            *size,
                            result.size()
                        );
                        let size = assert_same!(
                            "Binop::Integer: Operands' sizes must be the same",
                            left.size(),
                            right.size(),
                        );
                        // Left hand side: Rax appropriate bytes, Right hand side: Rcx appropriate bytes.
                        let lhs = Rax.lower_bytes_register(size);
                        let rhs = Rcx.lower_bytes_register(size);
                        let p = lhs.prefix();
                        self.load_arg_to_reg(left, lhs);
                        self.load_arg_to_reg(right, rhs);
                        match kind {
                            BinopKind::Arithmetic(c) => {
                                use ArithmeticKind::*;
                                match c {
                                    Addition => asm!(self, "  add{p} {rhs}, {lhs}"),
                                    Subtraction => asm!(self, "  sub{p} {rhs}, {lhs}"),
                                    Multiplication => asm!(self, "  imul{p} {rhs}, {lhs}"),
                                    Division => {
                                        asm!(self, "  cqto");
                                        asm!(self, "  idiv{p} {rhs}");
                                    }
                                }
                                self.store_reg_to_lvalue(lhs, result);
                            }
                            BinopKind::Ordering(c) => {
                                asm!(self, "  cmp{p} {rhs}, {lhs}");
                                use OrderingKind::*;
                                match c {
                                    Equality => asm!(self, "  sete {Al}"),
                                    Inequality => asm!(self, "  setne {Al}"),
                                    GreaterThan => asm!(self, "  setg {Al}"),
                                    GreaterEq => asm!(self, "  setge {Al}"),
                                    LessThan => asm!(self, "  setl {Al}"),
                                    LessEq => asm!(self, "  setle {Al}"),
                                }
                                self.store_reg_to_lvalue(Al, result);
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
                        self.store_reg_to_lvalue(Bl, result);
                    }
                }
            }

            Opcode::Negate { result, item } => {
                let size = assert_same!(
                    "Opcode::Negate: destination and source cannot have different sizes",
                    result.size(),
                    item.size()
                );
                comment!(self, "Negate");
                let reg = Rax.lower_bytes_register(size);
                let p = reg.prefix();
                self.load_arg_to_reg(item, reg);
                asm!(self, "  neg{p} {reg}");
                self.store_reg_to_lvalue(reg, result);
            }
            Opcode::Assign { result, source } => {
                let size = assert_same!(
                    "Opcode::Asign: result & source must have the same size",
                    result.size(),
                    source.size(),
                );
                comment!(self, "Assign");
                let reg = Rax.lower_bytes_register(size);
                self.load_arg_to_reg(source, reg);
                self.store_reg_to_lvalue(reg, result);
            }
            Opcode::Return(ret) => {
                if let Some(ret) = ret {
                    let reg = Rax.lower_bytes_register(ret.size());
                    self.load_arg_to_reg(ret, reg);
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
                let reg = Rax.lower_bytes_register(condition.size());
                let p = reg.prefix();
                self.load_arg_to_reg(condition, reg);
                asm!(self, "  test{p} {reg}, {reg}");
                asm!(self, "  jz .label_{label}")
            }
            Opcode::Jmp { label } => {
                comment!(self, "Jmp");
                asm!(self, "  jmp .label_{label}");
            }
            Opcode::AddressOf { result, source } => {
                assert_same!(
                    "Opcode::AddressOf: Could not store adress into lvalue with size different from 8",
                    result.size(),
                    8
                );
                comment!(self, "AddressOf");
                asm!(
                    self,
                    "  leaq {lvalue}, {Rax}",
                    lvalue = DisplayLValue(
                        *source,
                        &self
                            .arg_offsets
                            .as_ref()
                            .expect("COMPILER BUG: Codegen: no current function")
                    )
                );
                self.store_reg_to_lvalue(Rax, result)
            }
            Opcode::Store { result, source } => {
                comment!(self, "Store");
                let val_reg = Rax.lower_bytes_register(source.size());
                let p = val_reg.prefix();
                self.load_arg_to_reg(source, val_reg);
                self.load_arg_to_reg(&Arg::LValue(*result), Rcx);
                asm!(self, "  mov{p} {val_reg}, ({Rcx})");
            }
            Opcode::Load { result, source } => {
                comment!(self, "Load");
                assert_same!(
                    format!("Could not dereference {e} byte adress", e = source.size()),
                    source.size(),
                    8
                );
                let ptr_reg = Rax;
                let value_reg = ptr_reg.lower_bytes_register(result.size());
                let p = value_reg.prefix();
                // Load pointer into the ptr_reg.
                self.load_arg_to_reg(source, ptr_reg);
                // Dereference the value from ptr_reg into value_reg.
                asm!(self, "  mov{p} ({ptr_reg}), {value_reg}");
                self.store_reg_to_lvalue(value_reg, result);
            }
        }
        comment!(self);
    }

    pub fn generate(mut self) -> String {
        asm!(self, ".section .text");
        for v in &self.ir.functions {
            self.generate_function(v);
        }
        self.generate_data();
        self.output
    }

    fn arg_offsets(&self) -> &Vec<usize> {
        self.arg_offsets
            .as_ref()
            .expect("COMPILER BUG: Codegen: trying to access unexisting function argument.")
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
            Arg::RValue(Rvalue::ExternalFunction(name)) => {
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
    pub fn store_reg_to_lvalue(&mut self, reg: Register, lvalue: &Lvalue) {
        if reg.size() == 0 || lvalue.size() == 0 {
            assert_same!("", reg.size(), lvalue.size(), 0);
            return;
        }
        let p = reg.prefix();
        let lvalue = DisplayLValue(*lvalue, self.arg_offsets()).to_string();
        asm!(self, "  mov{p} {reg}, {lvalue}");
    }
    pub fn load_arg_to_reg(&mut self, arg: &Arg<'ir>, reg: Register) {
        if arg.size() == 0 || reg == VoidReg {
            assert!(arg.size() == 0 && reg == VoidReg);
            return;
        };

        let p = reg.prefix();
        let offsets = self.arg_offsets();
        match arg {
            Arg::RValue(rvalue) => {
                let display = DisplayRValue(rvalue, self.pic);

                // Unfortunately strings are annoying in PIC and need to be handled separately.
                let instruction = if rvalue.is_string() && self.pic {
                    "lea"
                } else {
                    "mov"
                };
                asm!(self, "  {instruction}{p} {display}, {reg}");
            }
            Arg::LValue(lvalue) => {
                let display = DisplayLValue(*lvalue, offsets).to_string();
                asm!(self, "  mov{p} {display}, {reg}")
            }
        }
    }

    fn push_arg(&mut self, arg: &Arg<'ir>) {
        if arg.size() == 0 {
            return;
        };
        let p = Register::prefix_from_size(arg.size());
        let offsets = self.arg_offsets();
        match arg {
            Arg::RValue(rvalue) => {
                let display = DisplayRValue(rvalue, self.pic);
                match rvalue {
                    // Unfortunately strings are annoying in PIC and need to be handled separately.
                    Rvalue::String { .. } => {
                        if self.pic {
                            asm!(self, "  lea{p} {display}, {Rax}");
                            asm!(self, "  push{p} {Rax}")
                        } else {
                            asm!(self, "  push{p} {display}");
                        }
                    }
                    _ => {
                        asm!(self, "  push{p} {display}");
                    }
                }
            }
            Arg::LValue(lvalue) => {
                let display = DisplayLValue(*lvalue, offsets).to_string();
                asm!(self, "  push{p} {display}");
            }
        }
    }
}

struct DisplayLValue<'a>(pub Lvalue, pub &'a Vec<usize>);

// Bool: PIC
impl<'a> Display for DisplayLValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let DisplayLValue(lvalue, offsets) = self;
        let offset = match lvalue {
            Lvalue::StackOffset { offset, .. } => *offset,
            Lvalue::Argument { index, .. } => offsets[*index],
        };
        write!(f, "-{offset}(%rbp)")?;
        Ok(())
    }
}

struct DisplayRValue<'a>(&'a Rvalue<'a>, bool);
impl<'a> Display for DisplayRValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self(rvalue, pic) = self;
        match rvalue {
            Rvalue::Void => panic!(
                "COMPILER BUG: Codegen: DisplayRValue should never be called on Rvalue::Void"
            ),
            Rvalue::Bool(bool) => write!(f, "${}", *bool as i32)?,
            Rvalue::Int64 { bits, signed } => {
                match *signed {
                    true => write!(f, "${}", i64::from_le_bytes(*bits))?,
                    false => write!(f, "${}", u64::from_le_bytes(*bits))?,
                };
            }
            Rvalue::String { index } => {
                match *pic {
                    true => write!(f, ".STR{index}(%rip)")?,
                    false => write!(f, "$.STR{index}")?,
                };
            }
            Rvalue::ExternalFunction(name) => match *pic {
                true => write!(f, "{}@GOTPCREL(%rip)", name.name)?,
                false => write!(f, "{}", name.name)?,
            },
        };

        Ok(())
    }
}
