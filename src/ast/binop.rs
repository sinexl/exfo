use crate::lexing::token::TokenType;

macro_rules! binop_enum {
    ($name:ident
        { $($variant:ident => $token:ident),* $(,)?}
    ) => {
        #[derive(Hash, Eq, PartialEq, Debug, )]
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
    }
}
/// higher index = higher precedence/binding power
pub const PRECEDENCE: &[&[BinopKind]] = &[
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
}
