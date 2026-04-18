use crate::code_generation::x86_64::register::Register::{Eax, Ecx, Edi, Edx, Esi, R8d, R9d};
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Register {
    // Imaginary "void" register
    VoidReg,
    // General purpose registers
    // Function Parameter registers
    Rdi,
    Rsi,
    Rdx,
    Rcx,
    R8,
    R9,
    // Return value
    Rax,
    //
    // Lower bits (1)
    Dil, // Rdi
    Sil, // Rsi
    Dl,  // Rdx
    Cl,  // Rcx
    R8b, // R8
    R9b, // R9
    Al,  // Rax
    // Lower bits (4)
    Edi, // Rdi
    Esi, // Rsi
    Edx, // Rdx
    Ecx, // Rcx
    R8d, // R8
    R9d, // R9
    Eax, // Rax

    Bl, // Rbx
}

impl Register {
    pub fn size(&self) -> usize {
        use Register::*;
        match self {
            // General purpose registers
            Rax => 8,
            Rdi => 8,
            Rsi => 8,
            Rdx => 8,
            Rcx => 8,
            R8 => 8,
            R9 => 8,
            Edi => 4,
            Esi => 4,
            Edx => 4,
            Ecx => 4,
            R8d => 4,
            R9d => 4,
            Eax => 4,
            // Lower bits registers
            Al | Bl | Cl | Dl | Dil | Sil => 1,
            R8b | R9b => 1,

            VoidReg => 0,
        }
    }
    pub fn prefix(&self) -> &'static str {
        Self::prefix_from_size(self.size())
    }

    pub fn prefix_from_size(size: usize) -> &'static str {
        match size {
            8 => "q",
            4 => "l",
            1 => "b",

            _ => panic!("invalid register size"),
        }
    }
    // TODO: Smarter way of declaring registers and their lower bits registers
    pub fn lower_bytes_register(&self, bytes: usize) -> Register {
        use Register::*;
        if bytes == 0 {
            return VoidReg;
        }
        if self.size() != 8 {
            panic!("Only general-purpose 64-bit registers (excluding Rbp are supported)")
        }
        if bytes == 8 {
            return *self;
        }
        match self {
            Rax => match bytes {
                1 => Al,
                4 => Eax,
                _ => panic!("invalid register size"),
            },

            Rdi => match bytes {
                1 => Dil,
                4 => Edi,
                _ => panic!("invalid register size"),
            },
            Rsi => match bytes {
                1 => Sil,
                4 => Esi,
                _ => panic!("invalid register size"),
            },
            Rdx => match bytes {
                1 => Dl,
                4 => Edx,
                _ => panic!("invalid register size"),
            },
            Rcx => match bytes {
                1 => Cl,
                4 => Ecx,
                _ => panic!("invalid register size"),
            },
            R8 => match bytes {
                1 => R8b,
                4 => R8d,
                _ => panic!("invalid register size"),
            },
            R9 => match bytes {
                1 => R9b,
                4 => R9d,
                _ => panic!("invalid register size"),
            }, // Al | Bl | Rbp =>
            Edi | Esi | Edx | Ecx | R8d | R9d | Eax | Dil | Sil | Dl | Cl | R8b | R9b | Al | Bl
            | VoidReg => {
                panic!("Only general-purpose 64-bit registers (excluding Rbp are supported)")
            }
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Register::Rax => "%rax",
            Register::Rcx => "%rcx",
            Register::Rdi => "%rdi",
            Register::Rsi => "%rsi",
            Register::Rdx => "%rdx",
            Register::R8 => "%r8",
            Register::R9 => "%r9",
            Register::Al => "%al",
            Register::Bl => "%bl",
            Register::Dil => "%dil",
            Register::Sil => "%sil",
            Register::Dl => "%dl",
            Register::Cl => "%cl",
            Register::R8b => "%r8b",
            Register::R9b => "%r9b",
            Register::VoidReg => {
                panic!("COMPILER BUG: Display should never be called on void 'register'")
            }
            Edi => "%edi",
            Esi => "%esi",
            Edx => "%edx",
            Ecx => "%ecx",
            R8d => "%r8d",
            R9d => "%r9d",
            Eax => "%eax",
        };
        write!(f, "{str}",)
    }
}
