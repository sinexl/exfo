use crate::analysis::resolver::{Resolver, ResolverError, ResolverErrorKind};
use crate::analysis::tests::machinery::{error, success, PATH};
use crate::analysis::type_system::type_context::TypeCtx;
use crate::ast::statement::{Statement, StatementKind};
use crate::common::identifier::Identifier;
use crate::common::SourceLocation;
use crate::hashmap;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::collections::HashMap;
use std::ptr::addr_of_mut;
use std::rc::Rc;

#[test]
pub fn undeclared_variable() {
    let file = Rc::from(PATH);
    let e = &Bump::new();

    assert_eq!(
        error(e, "func main () { a;}").kind,
        ResolverErrorKind::UndeclaredIdentifier {
            usage: Identifier::new(
                "a",
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
    let e = &Bump::new();
    assert_eq!(
        error(e, "func main () { b := b; }").kind,
        ResolverErrorKind::ReadingFromInitializer {
            read: Identifier::new(
                "b",
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
    let p = r#"(func `main`(#1) (): unknown_t
  (`a`(#0): unknown_t = 10)
  (a<#0>)
)"#;
    assert_eq!(success("func main () { a:=10; a; }"), p,)
}

#[test]
pub fn with_blocks() {
    let resolutions = success("func main () { a := 10; a; { a; a := 15; a;} } ");
    let p = r#"(func `main`(#2) (): unknown_t
  (`a`(#0): unknown_t = 10)
  (a<#0>)
  (block
    (a<#0>)
    (`a`(#1): unknown_t = 15)
    (a<#1>)
  )
)"#;
    assert_eq!(resolutions, p,)
}

#[test]
pub fn shadowing() {
    let resolutions = success("func main() { a := 10; a; a := a + 10; a; }");
    let p = r#"(func `main`(#2) (): unknown_t
  (`a`(#0): unknown_t = 10)
  (a<#0>)
  (`a`(#1): unknown_t = (+ (a<#0>) 10))
  (a<#1>)
)"#;
    assert_eq!(resolutions, p)
}

#[test]
pub fn undefined_loop_label() {
    let e = &Bump::new();
    let loc = SourceLocation {
        line: 1,
        offset: 26,
        file: Rc::from(PATH),
    };
    assert_eq!(
        error(e, "i := 0; while true break unknown_label;"),
        ResolverError {
            loc: loc.clone(),
            kind: ResolverErrorKind::UndeclaredLoopLabel {
                name: Identifier {
                    name: "unknown_label",
                    location: loc
                },
            },
        }
    );
}

#[test]
pub fn loop_label_redefinition() {
    let loc = SourceLocation {
        line: 1,
        offset: 24,
        file: Rc::from(PATH),
    };
    let e = &Bump::new();
    assert_eq!(
        error(e, "i := 0; a: while true {a: while true 1 + 1;}"),
        ResolverError {
            loc: loc.clone(),
            kind: ResolverErrorKind::LoopLabelRedefinition {
                original_name: Identifier {
                    name: "a",
                    location: SourceLocation {
                        line: 1,
                        offset: 9,
                        file: Rc::from(PATH),
                    }
                },
            },
        }
    );
}

#[test]
pub fn break_continue_outside_of_corresponding_block() {
    let e = &Bump::new();
    assert_eq!(
        error(e, "break;"),
        ResolverError {
            loc: SourceLocation {
                line: 1,
                offset: 1,
                file: Rc::from(PATH)
            },
            kind: ResolverErrorKind::BreakOutsideOfLoop
        }
    );

    assert_eq!(
        error(e, "continue;"),
        ResolverError {
            loc: SourceLocation {
                line: 1,
                offset: 1,
                file: Rc::from(PATH)
            },
            kind: ResolverErrorKind::ContinueOutsideOfLoop
        }
    );

    assert_eq!(
        error(e, "return;"),
        ResolverError {
            loc: SourceLocation {
                line: 1,
                offset: 1,
                file: Rc::from(PATH)
            },
            kind: ResolverErrorKind::TopLevelReturn
        }
    );
}

#[test]
pub fn proper_label_resolution() {
    let src = "name: while true {
        while false break;
        while true break name;
        while true {
            break;
        }
    }";

    let (tokens, errors) = Lexer::new(src, PATH).accumulate();
    assert!(errors.is_empty());
    let ast_alloc = Bump::new();
    let type_alloc = Bump::new();
    let err_alloc = Bump::new();
    let mut type_ctx = TypeCtx::new(&type_alloc);

    let mut parser = Parser::new(
        tokens.into(),
        &ast_alloc,
        &err_alloc,
        addr_of_mut!(type_ctx),
    );
    let (ast, errors) = parser.parse_program();
    assert!(errors.is_empty());

    let mut resolver = Resolver::new(&err_alloc);
    let errors = resolver.resolve_statements(ast);
    assert!(errors.is_empty());

    let mut map: HashMap<(usize, usize), (usize, usize)> = HashMap::new();

    for stmt in ast.iter().copied() {
        if let StatementKind::While { body, .. } = &stmt.kind {
            let while_loc = (stmt.loc.line, stmt.loc.offset);
            fn collect<'ast, 'types>(
                stmt: &'ast Statement<'ast, 'types>,
                parent_loc: (usize, usize),
                map: &mut HashMap<(usize, usize), (usize, usize)>,
            ) {
                match &stmt.kind {
                    StatementKind::Break { .. } | StatementKind::Continue { .. } => {
                        map.insert((stmt.loc.line, stmt.loc.offset), parent_loc);
                    }
                    StatementKind::While { body, .. } => {
                        let inner_loc = (stmt.loc.line, stmt.loc.offset);
                        collect(body, inner_loc, map);
                    }
                    StatementKind::Block(st) => {
                        for s in *st {
                            collect(s, parent_loc, map);
                        }
                    }
                    _ => {}
                }
            }

            collect(body, while_loc, &mut map);
        }
    }

    assert_eq!(
        map,
        hashmap! {
            (5, 13) => (4, 9) ,
            (3, 20) => (3, 9),
            (2, 21) => (2, 9),
        }
    );
}
