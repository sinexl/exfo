use crate::analysis::resolver::{Resolver, ResolverError};
use crate::analysis::type_system::type_context::TypeCtx;
use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::fmt::Write;
use std::ptr::addr_of_mut;

pub const PATH: &str = "PATH";
// Required machinery for compiler built-in tests for Resolver.

pub fn success(src: &str) -> String {
    let (tokens, errors) = Lexer::new(src, PATH).accumulate();
    assert!(errors.is_empty());
    let ast_alloc = Bump::new();
    let type_alloc = Bump::new();
    let err_alloc = Bump::new();
    let mut types = TypeCtx::new(&type_alloc);

    let mut parser = Parser::new(tokens.into(), &ast_alloc, &err_alloc, addr_of_mut!(types));
    let (ast, errors) = parser.parse_program();
    assert!(errors.is_empty());

    let mut resolver = Resolver::new(&err_alloc);
    let errors = resolver.resolve_statements(ast);
    assert_eq!(errors.len(), 0);
    let mut result = String::new();
    for i in ast {
        write!(result, "{}", PrefixPrintStatement(i, &types)).unwrap();
    }
    result.trim().to_string()
}

pub fn error<'errors>(e: &'errors Bump, src: &str) -> ResolverError<'errors> {
    let errors = errors(e, src);
    assert_eq!(errors.len(), 1);
    errors[0].clone()
}

pub fn errors<'errors>(e: &'errors Bump, src: &str) -> Vec<ResolverError<'errors>> {
    let (tokens, errors) = Lexer::new(src, PATH).accumulate();
    assert!(errors.is_empty());

    let ast_alloc = Bump::new();
    let type_alloc = Bump::new();
    let mut type_ctx = TypeCtx::new(&type_alloc);

    let mut parser = Parser::new(tokens.into(), &ast_alloc, e, addr_of_mut!(type_ctx));
    let (ast, errors) = parser.parse_program();
    assert!(errors.is_empty());

    let mut resolver = Resolver::new(e);
    resolver.resolve_statements(ast).to_vec()
}
