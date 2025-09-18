use crate::analysis::analyzer::{AnalysisError, Analyzer, ResolverError, ResolverErrorKind};
use crate::common::{IdentifierBox, SourceLocation};
use crate::hashmap;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::collections::HashMap;
use std::rc::Rc;

const PATH: &str = "PATH";

#[test]
pub fn undeclared_variable() {
    let file = Rc::from(PATH);
    assert_eq!(
        error("func main () { a;}").kind,
        ResolverErrorKind::UndeclaredIdentifier {
            usage: IdentifierBox::new(
                "a".to_owned().into_boxed_str(),
                SourceLocation {
                    line: 1,
                    offset: 16,
                    file,
                }
            ),
        }
    );
}

#[test]
pub fn reading_from_initializer() {
    let file = Rc::from(PATH);
    assert_eq!(
        error("func main () { b := b; }").kind,
        ResolverErrorKind::ReadingFromInitializer {
            read: IdentifierBox::new(
                "b".to_owned().into_boxed_str(),
                SourceLocation {
                    line: 1,
                    offset: 21,
                    file,
                }
            ),
        }
    );
}

#[test]
pub fn simple_resolution() {
    assert_eq!(
        success("func main () { a:=10; a; }"),
        hashmap! {
            (1, 23)  => 0,
        }
    )
}

#[test]
pub fn with_blocks() {
    let resolutions = success("func main () { a := 10; a; { a; a := 15; a;} } ");
    assert_eq!(
        resolutions,
        hashmap! {
            (1,25) => 0,
            (1, 30) => 1,
            (1, 42) => 0,
        }
    )
}

#[test]
pub fn shadowing() {
    let resolutions = success("func main() { a := 10; a; a := a + 10; a; }");
    assert_eq!(
        resolutions,
        hashmap! {
            (1, 24) => 0,
            (1, 32) => 0,
            (1, 40) => 0,
        }
    );
}

fn success(src: &str) -> HashMap<(usize, usize), usize> {
    let (tokens, errors) = Lexer::new(src, PATH).accumulate();
    assert!(errors.is_empty());
    let ast_alloc = Bump::new();

    let mut parser = Parser::new(tokens.into(), &ast_alloc);
    let (ast, errors) = parser.parse_program();
    assert!(errors.is_empty());

    let mut analyzer = Analyzer::new(&ast_alloc);
    let errors = analyzer.resolve_statements(ast);
    assert_eq!(errors.len(), 0);
    analyzer
        .resolutions
        .iter()
        .map(|(expr, depth)| ((expr.loc.line, expr.loc.offset), *depth))
        .collect()
}

fn error(src: &str) -> ResolverError {
    let errors = errors(src);
    assert_eq!(errors.len(), 1);
    errors[0].clone()
}

fn errors(src: &str) -> Vec<ResolverError> {
    let (tokens, errors) = Lexer::new(src, PATH).accumulate();
    assert!(errors.is_empty());
    let ast_alloc = Bump::new();

    let mut parser = Parser::new(tokens.into(), &ast_alloc);
    let (ast, errors) = parser.parse_program();
    assert!(errors.is_empty());

    let mut analyzer = Analyzer::new(&ast_alloc);
    analyzer
        .resolve_statements(ast)
        .iter()
        .filter_map(|e| {
            if let AnalysisError::ResolverError(e) = e {
                return Some(e);
            }
            None
        })
        .cloned()
        .collect()
}
