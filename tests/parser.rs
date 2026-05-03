use fhia::diagnostics::Severity;
use fhia::parser::{
    self,
    expr::{Expr, Ty},
};

fn parse_ok(input: &str) -> Vec<Expr<'_>> {
    let output = parser::parse(input);
    assert!(
        output.diagnostics.is_empty(),
        "expected no diagnostics, got: {:?}",
        output.diagnostics
    );
    output.exprs
}

fn parse_err(input: &str) -> Vec<fhia::diagnostics::Diagnostic> {
    let output = parser::parse(input);
    assert!(
        !output.diagnostics.is_empty(),
        "expected diagnostics, got none"
    );
    output.diagnostics
}

fn parse_with_error(input: &str) {
    let diagnostics = parse_err(input);
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error));
}

// =============================================================================
// Success cases
// =============================================================================

#[test]
fn parse_whitespace_flexibility() {
    // spacing around `:`, `=`, and the type should not affect parsing
    let inputs = [
        "let x:i64 = 1",
        "let x: i64 = 1",
        "let x :i64 = 1",
        "let x : i64 = 1",
        "let x:i64= 1",
        "let x:i64=1",
    ];
    for input in inputs {
        let exprs = parse_ok(input);
        assert_eq!(exprs.len(), 1, "failed on: {input}");
        assert!(
            matches!(&exprs[0], Expr::Declaration { name: "x", ty: Ty::I64, .. }),
            "failed on: {input}"
        );
    }
}

#[test]
fn parse_declaration_i64() {
    let exprs = parse_ok("let x: i64 = 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { name, ty, expr } => {
            assert_eq!(*name, "x");
            assert_eq!(*ty, Ty::I64);
            assert!(matches!(expr.as_ref(), Expr::I64(42)));
        }
        _ => panic!("expected declaration"),
    }
}

#[allow(clippy::approx_constant)]
#[test]
fn parse_declaration_f64() {
    let exprs = parse_ok("let x: f64 = 3.14");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { name, ty, expr } => {
            assert_eq!(*name, "x");
            assert_eq!(*ty, Ty::F64);
            assert!(matches!(expr.as_ref(), Expr::F64(f) if (*f - 3.14).abs() < 1e-9));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_ident_in_rhs() {
    // an identifier on the RHS is valid syntax; its type is Unknown at parse time
    let exprs = parse_ok("let x: i64 = y");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => match expr.as_ref() {
            Expr::Ident { name, ty } => {
                assert_eq!(*name, "y");
                assert_eq!(*ty, Ty::Unknown);
            }
            _ => panic!("expected ident"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_cast_expression() {
    let exprs = parse_ok("let x: u32 = u32 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => match expr.as_ref() {
            Expr::Cast(ty, inner) => {
                assert_eq!(*ty, Ty::U32);
                assert!(matches!(inner.as_ref(), Expr::I64(42)));
            }
            _ => panic!("expected cast"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_nested_cast() {
    // cast wrapping another cast — both with and without parens
    let check = |exprs: Vec<Expr<'_>>| {
        assert_eq!(exprs.len(), 1);
        match &exprs[0] {
            Expr::Declaration { expr, .. } => match expr.as_ref() {
                Expr::Cast(outer_ty, inner) => {
                    assert_eq!(*outer_ty, Ty::I64);
                    assert!(matches!(inner.as_ref(), Expr::Cast(Ty::U32, _)));
                }
                _ => panic!("expected outer cast"),
            },
            _ => panic!("expected declaration"),
        }
    };
    check(parse_ok("let x: i64 = i64 (u32 42)"));
    check(parse_ok("let x: i64 = i64 u32 42"));
}

#[test]
fn parse_parenthesized_expression() {
    let exprs = parse_ok("let x: i64 = (42)");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => {
            assert!(matches!(expr.as_ref(), Expr::I64(42)));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_braced_expression() {
    let exprs = parse_ok("let x: i64 = {42}");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => {
            assert!(matches!(expr.as_ref(), Expr::I64(42)));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_nested_grouping() {
    // braces and parens are interchangeable grouping delimiters
    for input in ["let x: i64 = ((42))", "let x: i64 = {(42)}", "let x: i64 = ({42})"] {
        let exprs = parse_ok(input);
        assert!(
            matches!(&exprs[0], Expr::Declaration { expr, .. } if matches!(expr.as_ref(), Expr::I64(42))),
            "failed on: {input}"
        );
    }
}

#[test]
fn parse_multiple_declarations() {
    let exprs = parse_ok("let a: i64 = 1  let b: f64 = 2.");
    assert_eq!(exprs.len(), 2);
    assert!(matches!(&exprs[0], Expr::Declaration { name: "a", ty: Ty::I64, .. }));
    assert!(matches!(&exprs[1], Expr::Declaration { name: "b", ty: Ty::F64, .. }));
}

// =============================================================================
// Failure cases
// =============================================================================

#[test]
fn parse_missing_assign() {
    parse_with_error("let x: i64 1");
}

#[test]
fn parse_missing_colon() {
    parse_with_error("let x i64 = 1");
    parse_with_error("let xi64 = 1");
}

#[test]
fn parse_missing_type() {
    parse_with_error("let x: = 1");
    parse_with_error("let x = 1");
}

#[test]
fn parse_unclosed_paren() {
    parse_with_error("let x: i64 = (1");
}

#[test]
fn parse_unclosed_brace() {
    parse_with_error("let x: i64 = {1");
}

#[test]
fn parse_invalid_token() {
    parse_with_error("let x: i64 = @");
}

#[test]
fn parse_keyword_as_value() {
    // `let` is not a valid expression
    parse_with_error("let x: i64 = let");
}

#[test]
fn parse_keyword_as_name() {
    // `let` cannot be used as an identifier name
    parse_with_error("let let: i64 = 1");
}

#[test]
fn parse_incomplete_cast() {
    // a type token with no following expression is not valid
    parse_with_error("let x: i64 = i64");
}
