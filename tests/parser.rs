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

#[test]
fn parse_single_declaration_i64() {
    let exprs = parse_ok("let x = 1");
    assert_eq!(exprs.len(), 1);
    match &exprs[0] {
        Expr::Declaration { name, ty, expr } => {
            assert_eq!(*name, "x");
            assert!(matches!(ty, Ty::Unknown));
            assert!(matches!(expr.as_ref(), Expr::I64(1)));
        }
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_single_declaration_f64() {
    let exprs = parse_ok("let x = 3.14");
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
    let exprs = parse_ok("let x = i64 1");
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
    let exprs = parse_ok("let x = (1)");
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
    let exprs = parse_ok("let x = {1}");
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
    let exprs = parse_ok("let a = 1 let b = 2");
    assert_eq!(exprs.len(), 2);
}

#[test]
fn parse_missing_assign_reports_error() {
    let diagnostics = parse_err("let x 1");
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error));
}

#[test]
fn parse_unclosed_paren_reports_error() {
    let diagnostics = parse_err("let x = (1");
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error));
}

#[test]
fn parse_invalid_token_reports_error() {
    let diagnostics = parse_err("let x = @");
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error));
}

#[test]
fn parse_keyword_in_place_of_expr() {
    let diagnostics = parse_err("let x = let");
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error))
}

#[test]
fn parse_keyword_in_place_of_ident() {
    let diagnostics = parse_err("let let = 1");
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error))
}
