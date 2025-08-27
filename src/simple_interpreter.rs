use crate::ast::{Expression, ExpressionKind, ExpressionVisitor, UnaryKind};
use crate::ast::binop::BinopKind;

pub struct Interpreter {}

impl ExpressionVisitor<f32> for Interpreter {
    fn visit(&mut self, expression: &Expression) -> f32 {
        self.evaluate(expression)
    }
}

impl Interpreter {
    fn evaluate(&mut self, expression: &Expression) -> f32 {
        match &expression.kind {
            ExpressionKind::Binop{left, right, kind}  => {
                let left = self.evaluate(left);
                let right = self.evaluate(right);
                let res = match kind {
                    BinopKind::Addition => left + right,
                    BinopKind::Subtraction => left - right,
                    BinopKind::Multiplication => left * right,
                    BinopKind::Division => left / right,
                };
                res
            }
            ExpressionKind::Unary { item, operator } => {
                let result = match operator {
                    UnaryKind::Negation => -self.evaluate(&item),
                };
                result
            }
            ExpressionKind::Grouping(inner) => self.evaluate(inner),
            &ExpressionKind::Literal(x) => x,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ExpressionKind::{Binop, Literal};
    use crate::common::SourceLocation;
    use bumpalo::Bump;

    #[test]
    pub fn test_simple_interpreter() {
        let zero: SourceLocation = Default::default();
        let alloc = Bump::new();
        let expression = Expression {
            kind: Binop{
                left: alloc.alloc::<Expression>(Expression {
                    kind: Literal(10f32),
                    loc: zero.clone(),
                }),
                right: alloc.alloc::<Expression>(Expression {
                    kind: Literal(10f32),
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
