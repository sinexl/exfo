use crate::analysis::resolver::ResolverError;
use crate::analysis::type_system::type_context::TypeCtx;
use crate::analysis::type_system::typechecker::TypeError;
use crate::common::BumpVec;
use crate::common::errors_warnings::display::DisplayErrorExtension;
use crate::lexing::lexer::LexerError;
use bumpalo::Bump;
use std::fmt::Write;
use crate::parsing::errors::ParseError;

pub struct StaticErrors<'errors> {
    lexer: BumpVec<'errors, LexerError>,
    parser: BumpVec<'errors, ParseError<'errors>>,
    resolver: BumpVec<'errors, ResolverError<'errors>>,
    typechecker: BumpVec<'errors, TypeError>,
}
impl<'errors> StaticErrors<'errors> {
    pub fn new(bump: &'errors Bump) -> Self {
        Self {
            lexer: BumpVec::new_in(bump),
            parser: BumpVec::new_in(bump),
            resolver: BumpVec::new_in(bump),
            typechecker: BumpVec::new_in(bump),
        }
    }

    pub fn len(&self) -> usize {
        self.lexer.len() + self.parser.len() + self.resolver.len() + self.typechecker.len()
    }

    pub fn lexer(&mut self, errors: Vec<LexerError>) {
        self.lexer.extend(errors);
    }
    pub fn parser(&mut self, errors: BumpVec<'errors, ParseError<'errors>>) {
        self.parser.extend(errors);
    }

    pub fn resolver(&mut self, errors: Vec<ResolverError<'errors>>) {
        self.resolver.extend(errors);
    }
    pub fn typechecker(&mut self, errors: BumpVec<'errors, TypeError>) {
        self.typechecker.extend(errors);
    }

    pub fn print<'types>(&self, f: &mut impl Write, types: &TypeCtx<'types>) -> std::fmt::Result {
        for l in &self.lexer {
            writeln!(f, "{}", l.display(()))?;
        }
        for p in &self.parser {
            writeln!(f, "{}", p.display(()))?;
        }

        for r in &self.resolver {
            writeln!(f, "{}", r.display(()))?;
        }

        for r in &self.typechecker {
            writeln!(f, "{}", r.display(types))?;
        }
        Ok(())
    }
}
