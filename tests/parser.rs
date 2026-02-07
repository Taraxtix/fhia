use fhia::diagnostics::{self, Severity};
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

#[test]
fn parse_single_declaration() {
    let exprs1 = parse_ok("let x:i64 = 1");
    let exprs2 = parse_ok("let x: i64 = 1");
    let exprs3 = parse_ok("let x :i64 = 1");
    let exprs4 = parse_ok("let x : i64 = 1");
    let exprs5 = parse_ok("let x:i64= 1");
    let exprs6 = parse_ok("let x:i64=1");

    let expected = |exprs: Vec<_>| {
        assert_eq!(exprs.len(), 1);
        match &exprs[0] {
            Expr::Declaration { name, ty, expr } => {
                assert_eq!(*name, "x");
                assert!(matches!(ty, Ty::I64));
                assert!(matches!(expr.as_ref(), Expr::I64(1)));
            }
            _ => panic!("expected declaration"),
        }
    };

    expected(exprs1);
    expected(exprs2);
    expected(exprs3);
    expected(exprs4);
    expected(exprs5);
    expected(exprs6);
}

#[test]
fn parse_single_declaration_i64() {
    let exprs = parse_ok("let x:i64 = 1");

    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { name, ty, expr } => {
            assert_eq!(*name, "x");
            assert!(matches!(ty, Ty::I64));
            assert!(matches!(expr.as_ref(), Expr::I64(1)));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_single_declaration_f64() {
    let exprs = parse_ok("let x: f64 = 3.14");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => {
            assert!(matches!(expr.as_ref(), Expr::F64(f) if (*f - 3.14).abs() < 1e-9));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_cast_expression() {
    let exprs = parse_ok("let x: i64 = i64 1");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => match expr.as_ref() {
            Expr::Cast(ty, inner) => {
                assert!(matches!(ty, Ty::I64));
                assert!(matches!(inner.as_ref(), Expr::I64(1)));
            }
            _ => panic!("expected cast expression"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_parenthesized_expression() {
    let exprs = parse_ok("let x: i64 = (1)");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => {
            assert!(matches!(expr.as_ref(), Expr::I64(1)));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_braced_expression() {
    let exprs = parse_ok("let x: i64 = {1}");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { expr, .. } => {
            assert!(matches!(expr.as_ref(), Expr::I64(1)));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_multiple_declarations() {
    let exprs = parse_ok("let a:i64 = 1 let b: f64 = 2.");
    assert_eq!(exprs.len(), 2);
}

#[test]
fn parse_missing_assign_reports_error() {
    parse_with_error("let x: i64 1");
}

#[test]
fn parse_unclosed_paren_reports_error() {
    parse_with_error("let x: i64 = (1");
}

#[test]
fn parse_invalid_token_reports_error() {
    parse_with_error("let x: i64 = @");
}

#[test]
fn parse_keyword_in_place_of_expr() {
    parse_with_error("let x: i64 = let");
}

#[test]
fn parse_keyword_in_place_of_ident() {
    parse_with_error("let let: i64 = 1");
}

#[test]
fn parse_missing_colon_reports_error() {
    parse_with_error("let let i64 = 1");
    parse_with_error("let leti64 = 1");
}

#[test]
fn parse_missing_type_reports_error() {
    parse_with_error("let x: = 1");
    parse_with_error("let x = 1");
}
