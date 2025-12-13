use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy)]
pub enum Register {
    // General purpose registers
    // Call registers
    Rdi,
    Rsi,
    Rdx,
    Rcx,
    R8,
    R9,
    // Return value
    Rax,
    //
    // Lower bits
    Dil, // Rdi
    Sil, // Rsi
    Dl,  // Rdx
    Cl,  // Rcx
    R8b, // R8
    R9b, // R9
    Al,  // Rax

    Bl, // Rbx
    Rbp { offset: usize, size: usize },
}

impl Register {
    pub fn size(&self) -> usize {
        use Register::*;
        match self {
            // General purpose registers
            Rax => 8,
            Rcx => 8,
            Rdi => 8,
            Rsi => 8,
            Rdx => 8,
            R8 => 8,
            R9 => 8,
            // Lower bits registers
            Al | Bl | Cl | Dl | Dil | Sil => 1,
            R8b | R9b => 1,

            Rbp {
                offset: _offset,
                size,
            } => *size,
        }
    }
    pub fn prefix(&self) -> &'static str {
        Self::prefix_from_size(self.size())
    }

    pub fn prefix_from_size(size: usize) -> &'static str {
        match size {
            8 => "q",
            1 => "b",
            _ => panic!("invalid register size"),
        }
    }
    // TODO: Smarter way of declaring registers and their lower bits registers
    pub fn lower_bytes_register(&self, bytes: usize) -> Register {
        use Register::*;
        if self.size() != 8 {
            panic!("Only general-purpose 64-bit registers (excluding Rbp are supported)")
        }
        if bytes == 8 {
            return *self;
        }
        match self {
            Rax => match bytes {
                1 => Al,
                _ => panic!("invalid register size"),
            },

            Rcx => match bytes {
                1 => Cl,
                _ => panic!("invalid register size"),
            },

            Rdi => match bytes {
                1 => Dil,
                _ => panic!("invalid register size"),
            },
            Rsi => match bytes {
                1 => Dil,
                _ => panic!("invalid register size"),
            },
            Rdx => match bytes {
                1 => Dl,
                _ => panic!("invalid register size"),
            },
            R8 => match bytes {
                1 => R8b,
                _ => panic!("invalid register size"),
            },
            R9 => match bytes {
                1 => R9b,
                _ => panic!("invalid register size"),
            }, // Al | Bl | Rbp =>
            Dil | Sil | Dl | Cl | R8b | R9b | Al | Bl | Rbp { .. } => {
                panic!("Only general-purpose 64-bit registers (excluding Rbp are supported)")
            }
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Register::Rax => "%rax".into(),
                Register::Rcx => "%rcx".into(),
                Register::Rdi => "%rdi".into(),
                Register::Rsi => "%rsi".into(),
                Register::Rdx => "%rdx".into(),
                Register::R8 => "%r8".into(),
                Register::R9 => "%r9".into(),
                Register::Al => "%al".into(),
                Register::Rbp {
                    offset,
                    size: _size,
                } => {
                    match *offset {
                        0 => "%rbp".into(),
                        _ => format!("{offset}(%rbp)"),
                    }
                }
                Register::Bl => "%bl".into(),
                Register::Dil => "%dil".into(),
                Register::Sil => "%sil".into(),
                Register::Dl => "%dl".into(),
                Register::Cl => "%cl".into(),
                Register::R8b => "%r8b".into(),
                Register::R9b => "%r9b".into(),
            }
        )
    }
}
