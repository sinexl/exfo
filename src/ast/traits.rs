use crate::ast::expression::Expression;
use crate::ast::statement::Statement;

pub trait ExpressionVisitor<R> {
    fn visit(&mut self, expression: &Expression) -> R;
}

impl Expression<'_> {
    pub fn accept<R>(&self, mut visitor: impl ExpressionVisitor<R>) -> R {
        visitor.visit(self)
    }
}
pub trait StatementVisitor<R> {
    fn visit(&mut self, statement: &Statement) -> R;
}

impl Statement<'_> {
    pub fn accept<R>(&self, mut visitor: impl StatementVisitor<R>) -> R {
        visitor.visit(self)
    }
}
