use crate::analysis::r#type::Type;
use crate::ast::binop as ast;

#[derive(Copy, Clone, Debug)]
pub enum Binop {
    IntegerBinop(IntegerBinop),
}

#[derive(Clone, Debug, Copy)]
pub struct IntegerBinop {
    pub signed: bool,
    pub size: usize,
    pub kind: BinopKind,
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
    // Comparison
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

impl Binop {
    pub fn from_ast_binop(kind: ast::BinopKind, ty: &Type) -> Self {
        if *ty != Type::Int64 {
            todo!("Floating point operations");
        }
        let size = ty.size();
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
        };
        Self::IntegerBinop(IntegerBinop { signed: true, size, kind } )
    }

    pub fn to_ast_binop(self) -> ast::BinopKind {
        let Binop::IntegerBinop(IntegerBinop { kind, .. }) = self;

        match kind {
            BinopKind::Arithmetic(c) => match c {
                ArithmeticKind::Addition =>  ast::BinopKind::Addition,
                ArithmeticKind::Subtraction => ast::BinopKind::Subtraction,
                ArithmeticKind::Multiplication => ast::BinopKind::Multiplication,
                ArithmeticKind::Division => ast::BinopKind::Division,
            }
            BinopKind::Ordering(ord) => match ord {
                OrderingKind::Equality => ast::BinopKind::Equality,
                OrderingKind::Inequality => ast::BinopKind::Inequality,
                OrderingKind::GreaterThan => ast::BinopKind::GreaterThan,
                OrderingKind::GreaterEq => ast::BinopKind::GreaterEq,
                OrderingKind::LessThan => ast::BinopKind::LessThan,
                OrderingKind::LessEq => ast::BinopKind::LessEq,
            }
        }
    }
}
