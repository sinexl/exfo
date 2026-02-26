// Machinery for built-in tests for Parser.
pub mod expression {
    use crate::analysis::type_context::TypeCtx;
    use crate::ast::prefix_printer::PrefixPrint;
    use crate::lexing::lexer::Lexer;
    use crate::parsing::parser::{ParseError, Parser};
    use crate::parsing::tests::test_precedence::PATH;
    use bumpalo::Bump;
    use std::ptr::addr_of_mut;

    pub fn single(expr: &str) -> String {
        let (tokens, errors) = Lexer::new(expr, PATH).accumulate();
        assert_eq!(errors.len(), 0);
        let test_alloc = Bump::new();
        let type_alloc = Bump::new();
        let mut type_ctx = TypeCtx::new(&type_alloc);
        let v = Parser::new(tokens.into(), &test_alloc, addr_of_mut!(type_ctx))
            .parse_expression()
            .unwrap();
        format!("{}", PrefixPrint(v))
    }
    pub fn fail(expr: &str) -> ParseError {
        let (tokens, errors) = Lexer::new(expr, PATH).accumulate();
        assert_eq!(errors.len(), 0);
        let test_alloc = Bump::new();
        let mut type_ctx = TypeCtx::new(&test_alloc);
        Parser::new(tokens.into(), &test_alloc, addr_of_mut!(type_ctx))
            .parse_expression()
            .expect_err("should fail")
    }
}

pub mod statement {
    use crate::analysis::type_context::TypeCtx;
    use crate::ast::prefix_printer::PrefixPrintStatement;
    use crate::lexing::lexer::Lexer;
    use crate::parsing::parser::Parser;
    use crate::parsing::tests::test_precedence::PATH;
    use bumpalo::Bump;
    use std::ptr::addr_of_mut;

    pub fn single(input: &str) -> String {
        let (t, e) = Lexer::new(input, PATH).accumulate();
        assert_eq!(e.len(), 0);

        let ast_bump = Bump::new();
        let type_bump = Bump::new();
        let mut type_ctx = TypeCtx::new(&type_bump);
        let mut p = Parser::new(t.into(), &ast_bump,  addr_of_mut!(type_ctx));
        let (statements, e) = p.parse_program();
        assert_eq!(e.len(), 0);
        assert_eq!(statements.len(), 1);
        let statement = statements[0];

        let type_ctx = TypeCtx::new(&type_bump);
        format!("{}", PrefixPrintStatement(statement, &type_ctx)).trim().to_string()
    }
}


