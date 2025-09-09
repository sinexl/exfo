use crate::analysis::analyzer::{AnalysisError, Analyzer, ResolverError, ResolverErrorKind};
use crate::common::{IdentifierBox, SourceLocation};
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
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


fn error(src: &str) -> ResolverError {
    let errors = errors(src);
    assert_eq!(errors.len(), 1);
    errors[0].clone()
}

#[allow(irrefutable_let_patterns)]
fn errors(src: &str) -> Vec<ResolverError> {
    let (tokens, errors) = Lexer::new(src, PATH).accumulate();
    assert!(errors.is_empty());
    let ast_alloc = Bump::new();

    let mut parser = Parser::new(tokens.into(), &ast_alloc);
    let (ast, errors) = parser.parse_program();
    assert!(errors.is_empty());

    let mut analyzer = Analyzer::new();
    let errors = analyzer.analyze_statements(ast);
    errors
        .iter()
        .filter_map(|e| {
            if let AnalysisError::ResolverError(e) = e {
                return Some(e.clone());
            }
            return None;
        })
        .collect()
}
