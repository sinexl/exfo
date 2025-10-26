use std::fmt::{Display, Formatter};

macro_rules! reg_enum {
    ($enum_name:ident
        { $($name:ident => $size:expr),* $(,)?}
    ) => {
        #[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
        pub(crate) enum $enum_name {
            $($name),*
        }

        impl $enum_name {
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$name => stringify!($name)),*
                }
            }

            pub fn size(&self) -> usize {
                match self {
                    $(Self::$name => $size),*
                }
            }
        }
    };
}

reg_enum! {
    Register {
        Rax => 8,
        Rcx => 8,
        Rdi => 8,
        Rsi => 8,
        Rdx => 8,
        R8 => 8, R9 => 8,

        Al => 1,
    }
}

impl Register {
    pub fn prefix(&self) -> &'static str {
        match self.size () {
            8 => "q",
            1 => "b",
            _ => panic!("invalid register size"),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}