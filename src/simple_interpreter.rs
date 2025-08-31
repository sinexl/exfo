use crate::ast::binop::BinopKind;
use crate::ast::expression::{AstLiteral, Expression, ExpressionKind, UnaryKind};
use crate::ast::traits::ExpressionVisitor;

pub struct Interpreter {}

impl ExpressionVisitor<f64> for Interpreter {
    fn visit(&mut self, expression: &Expression) -> f64 {
        self.evaluate(expression)
    }
}

impl Interpreter {
    #[allow(clippy::only_used_in_recursion)]
    fn evaluate(&mut self, expression: &Expression) -> f64 {
        match &expression.kind {
            ExpressionKind::Binop { left, right, kind } => {
                let left = self.evaluate(left);
                let right = self.evaluate(right);
                let res = match kind {
                    BinopKind::Addition => left + right,
                    BinopKind::Subtraction => left - right,
                    BinopKind::Multiplication => left * right,
                    BinopKind::Division => left / right,
                    _ => todo!(),
                };
                res
            }
            ExpressionKind::Unary { item, operator } => match operator {
                UnaryKind::Negation => -self.evaluate(item),
            },
            ExpressionKind::Literal(x) => {
                match x {
                    AstLiteral::Integral(i) => *i as f64,
                    AstLiteral::FloatingPoint(f) => *f,
                }
            },
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::expression::AstLiteral::FloatingPoint;
    use crate::ast::expression::ExpressionKind::{Binop, Literal};
    use crate::common::SourceLocation;
    use bumpalo::Bump;

    #[test]
    pub fn test_simple_interpreter() {
        let zero: SourceLocation = Default::default();
        let alloc = Bump::new();
        let expression = Expression {
            kind: Binop {
                left: alloc.alloc::<Expression>(Expression {
                    kind: Literal(FloatingPoint(10f64)),
                    loc: zero.clone(),
                }),
                right: alloc.alloc::<Expression>(Expression {
                    kind: Literal(FloatingPoint(10f64)),
                    loc: zero.clone(),
                }),
                kind: BinopKind::Addition,
            },
            loc: zero,
        };

        let mut interpreter = Interpreter {};
        let result = interpreter.evaluate(&expression);
        dbg!(result);
    }
}
