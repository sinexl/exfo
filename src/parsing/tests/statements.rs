use crate::parsing::tests::machinery::statement::*;

#[test]
pub fn simple_expression_statements() {
    assert_eq!(single("a = 10;"), "(= (a<#?>) 10)");
}

#[test]
pub fn if_capturing() {
    let code = r#"if true
    puts("A");
    puts("B");"#;
    let expected =
r#"(if true
  (call (puts<#?>) A)
)
(call (puts<#?>) B)"#;
    assert_eq!(multiple(code), expected)
}

#[test]
pub fn nested_no_braces() {
    let code =
r#"
if false
        if true
            puts("H1");
        else
            puts("H2");
    else
        puts("H3");"#;
    let expected =
r#"(if false
  (if true
    (call (puts<#?>) H1)
  else
    (call (puts<#?>) H2)
  )
else
  (call (puts<#?>) H3)
)"#;
    assert_eq!(multiple(code), expected)
}

#[test]
pub fn triple_nested() {
    let code =
r#"if true
    if true
        if false
            puts("I1");
        else
            puts("I2");"#;
    let expected =
r#"(if true
  (if true
    (if false
      (call (puts<#?>) I1)
    else
      (call (puts<#?>) I2)
    )
  )
)"#;
    assert_eq!(multiple(code), expected);
}


