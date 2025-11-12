use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy)]
pub enum Register {
    Rax,
    Rcx,
    Rdi,
    Rsi,
    Rdx,
    R8,
    R9,
    Al,
    Rbp { offset: usize, size: usize },
}

impl Register {
    pub fn size(&self) -> usize {
        use Register::*;
        match self {
            Rax => 8,
            Rcx => 8,
            Rdi => 8,
            Rsi => 8,
            Rdx => 8,
            R8 => 8,
            R9 => 8,
            Al => 1,
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
            }
        )
    }
}
