use crate::lexing::lexer::Lexer;

#[test]
pub fn test_lexer() {
    let code = r#"1 hello * /*comment*/ (2 + 3)
  hello /* nested /* multi
line */ comment */ 123 helo
      // this is a comment
      /* this is also
            a comment */
      (( )){} // grouping stuff
  !*+-/=<> <= == // operators
      "String Literal" 474 og
//"#;
    let (tokens, errors) = Lexer::new(code, "<testcase>").accumulate();
    assert!(errors.is_empty());
    let types = tokens
        .iter()
        .map(|e| (e.kind, e.loc.line, e.loc.offset))
        .collect::<Vec<_>>();

    use crate::lexing::lexer::TokenType::*;
    assert_eq!(
        types.as_slice(),
        &[
            (Integer, 1, 1),
            (Id, 1, 3),
            (Star, 1, 9),
            (OpenParen, 1, 23),
            (Integer, 1, 24),
            (Plus, 1, 26),
            (Integer, 1, 28),
            (CloseParen, 1, 29),
            (Id, 2, 3),
            (Integer, 3, 20),
            (Id, 3, 24),
            (OpenParen, 7, 7),
            (OpenParen, 7, 8),
            (CloseParen, 7, 10),
            (CloseParen, 7, 11),
            (OpenBrace, 7, 12),
            (CloseBrace, 7, 13),
            (Bang, 8, 3),
            (Star, 8, 4),
            (Plus, 8, 5),
            (Minus, 8, 6),
            (Slash, 8, 7),
            (Equal, 8, 8),
            (Less, 8, 9),
            (Greater, 8, 10),
            (LessEqual, 8, 12),
            (EqualEqual, 8, 15),
            (String, 9, 7),
            (Integer, 9, 24),
            (Id, 9, 28),
            (Eof, 10, 2)
        ]
    );
}
