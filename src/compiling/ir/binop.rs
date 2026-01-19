use crate::ast::binop as ast;

#[derive(Copy, Clone, Debug)]
pub enum Binop {
    Integer(IntegerBinop),
    Bitwise(BitwiseBinop),
}

#[derive(Clone, Debug, Copy)]
pub struct IntegerBinop {
    pub signed: bool,
    pub size: usize,
    pub kind: BinopKind,
}

#[derive(Clone, Debug, Copy)]
pub struct BitwiseBinop {
    pub kind: BitwiseKind,
    pub is_logical_with_short_circuit: bool, // Needed for codegen. If the binop is logical, it means that left argument of operation is already loaded
}

#[derive(Clone, Debug, Copy)]
pub enum BitwiseKind {
    Or,
    And,
}

#[derive(Clone, Debug, Copy)]
pub enum BinopKind {
    Arithmetic(ArithmeticKind),
    Ordering(OrderingKind),
}

#[derive(Clone, Debug, Copy)]
pub enum ArithmeticKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}
#[derive(Clone, Debug, Copy)]
pub enum OrderingKind {
    Equality,
    Inequality,
    GreaterThan,
    GreaterEq,
    LessThan,
    LessEq,
}

impl IntegerBinop {
    pub fn from_ast(signed: bool, size: usize, kind: ast::BinopKind) -> Binop {
        use ArithmeticKind::*;
        use OrderingKind::*;
        let kind: BinopKind = match kind {
            ast::BinopKind::Addition => BinopKind::Arithmetic(Addition),
            ast::BinopKind::Subtraction => BinopKind::Arithmetic(Subtraction),
            ast::BinopKind::Multiplication => BinopKind::Arithmetic(Multiplication),
            ast::BinopKind::Division => BinopKind::Arithmetic(Division),
            ast::BinopKind::Equality => BinopKind::Ordering(Equality),
            ast::BinopKind::Inequality => BinopKind::Ordering(Inequality),
            ast::BinopKind::GreaterThan => BinopKind::Ordering(GreaterThan),
            ast::BinopKind::GreaterEq => BinopKind::Ordering(GreaterEq),
            ast::BinopKind::LessThan => BinopKind::Ordering(LessThan),
            ast::BinopKind::LessEq => BinopKind::Ordering(LessEq),
            ast::BinopKind::And | ast::BinopKind::Or => {
                panic!("Compiler bug: IntegerBinop::from_ast should never be on logical operators")
            }
        };

        Binop::Integer(Self { size, signed, kind })
    }
}

impl Binop {
    pub fn to_ast_binop(self) -> ast::BinopKind {
        match self {
            Binop::Integer(IntegerBinop { kind, .. }) => match kind {
                BinopKind::Arithmetic(c) => match c {
                    ArithmeticKind::Addition => ast::BinopKind::Addition,
                    ArithmeticKind::Subtraction => ast::BinopKind::Subtraction,
                    ArithmeticKind::Multiplication => ast::BinopKind::Multiplication,
                    ArithmeticKind::Division => ast::BinopKind::Division,
                },
                BinopKind::Ordering(ord) => match ord {
                    OrderingKind::Equality => ast::BinopKind::Equality,
                    OrderingKind::Inequality => ast::BinopKind::Inequality,
                    OrderingKind::GreaterThan => ast::BinopKind::GreaterThan,
                    OrderingKind::GreaterEq => ast::BinopKind::GreaterEq,
                    OrderingKind::LessThan => ast::BinopKind::LessThan,
                    OrderingKind::LessEq => ast::BinopKind::LessEq,
                },
            },
            Binop::Bitwise(BitwiseBinop { kind, is_logical_with_short_circuit }) => {
                if !is_logical_with_short_circuit {
                    todo!("Bitwise operators")
                }
                match kind {
                    BitwiseKind::Or => ast::BinopKind::Or,
                    BitwiseKind::And => ast::BinopKind::And,
                }
            } ,
        }
    }
}
