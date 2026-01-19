use crate::lexing::token::TokenType;

macro_rules! binop_enum {
    ($name:ident
        { $($variant:ident => $token:ident),* $(,)?}
    ) => {
        #[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
        pub(crate) enum $name {
            $($variant),*
        }

        impl $name {
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => stringify!($variant)),*
                }
            }

            pub fn from_operator(token: TokenType) -> Option<Self> {
                match token {
                    $(TokenType::$token => Some(Self::$variant)),*,
                    _ => None
                }
            }
        }
    };
}

binop_enum! {
    BinopKind {
        // Arithmetic
        Addition       => Plus,
        Subtraction    => Minus,
        Multiplication => Star,
        Division       => Slash,
        // Comparison
        Equality       => EqualEqual,
        Inequality     => BangEqual,
        GreaterThan    => Greater,
        GreaterEq      => GreaterEqual,
        LessThan       => Less,
        LessEq         => LessEqual,
        And            => DoubleAmpersand,
        Or             => DoubleBar
    }
}
/// higher index = higher precedence/binding power
pub const PRECEDENCE: &[&[BinopKind]] = &[
    &[BinopKind::Or],
    &[BinopKind::And],
    &[BinopKind::Equality, BinopKind::Inequality],
    &[
        BinopKind::LessThan,
        BinopKind::LessEq,
        BinopKind::GreaterThan,
        BinopKind::GreaterEq,
    ],
    &[BinopKind::Addition, BinopKind::Subtraction],
    &[BinopKind::Multiplication, BinopKind::Division],
];

pub const MAX_PRECEDENCE: i32 = PRECEDENCE.len() as i32;

#[derive(PartialEq, Hash)]
pub enum BinopFamily {
    Arithmetic,
    Logical,
    Ordering,
}
impl BinopKind {
    pub fn precedence(&self) -> i32 {
        // level = precedence level
        // group = operations with the same precedence
        for (level, group) in PRECEDENCE.iter().enumerate() {
            for binop in *group {
                if *self == *binop {
                    return level as i32;
                }
            }
        }
        unreachable!(
            "if you reach this, it means that precedence table doesn't contain all of the possible operations"
        )
    }

    pub fn family(self) -> BinopFamily {
        use BinopFamily::*;
        match self {
            BinopKind::Addition => Arithmetic,
            BinopKind::Subtraction => Arithmetic,
            BinopKind::Multiplication => Arithmetic,
            BinopKind::Division => Arithmetic,
            // Ordering
            BinopKind::Equality => Ordering,
            BinopKind::Inequality => Ordering,
            BinopKind::GreaterThan => Ordering,
            BinopKind::GreaterEq => Ordering,
            BinopKind::LessThan => Ordering,
            BinopKind::LessEq => Ordering,
            // Logical
            BinopKind::And => Logical,
            BinopKind::Or => Logical,
        }
    }
    pub fn operator(self) -> &'static str {
        match self {
            BinopKind::Addition => "+",
            BinopKind::Subtraction => "-",
            BinopKind::Multiplication => "*",
            BinopKind::Division => "/",
            BinopKind::Equality => "==",
            BinopKind::Inequality => "!=",
            BinopKind::GreaterThan => ">",
            BinopKind::GreaterEq => ">=",
            BinopKind::LessThan => "<",
            BinopKind::LessEq => "<=",
            BinopKind::And => "&&",
            BinopKind::Or => "||",
        }
    }
}
