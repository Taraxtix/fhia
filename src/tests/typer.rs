use crate::Spanned;
use crate::diagnostics::Severity;
use crate::parser::{
    self,
    expr::{Expr, Ty},
};

fn type_ok(input: &str) -> Vec<Spanned<Expr<'_>>> {
    let parse_output = parser::parse(input);
    assert!(
        parse_output.diagnostics.is_empty(),
        "expected no parse diagnostics, got: {:?}",
        parse_output.diagnostics
    );
    let typed_output = parse_output.type_check();
    assert!(
        typed_output.diagnostics.is_empty(),
        "expected no type diagnostics, got: {:?}",
        typed_output.diagnostics
    );
    typed_output.exprs
}

fn type_err(input: &str) -> Vec<crate::diagnostics::Diagnostic> {
    let parse_output = parser::parse(input);
    assert!(
        parse_output.diagnostics.is_empty(),
        "expected no parse diagnostics, got: {:?}",
        parse_output.diagnostics
    );
    let typed_output = parse_output.type_check();
    assert!(
        !typed_output.diagnostics.is_empty(),
        "expected type diagnostics, got none"
    );
    typed_output.diagnostics
}

fn type_with_error(input: &str) {
    let diagnostics = type_err(input);
    assert!(diagnostics.iter().any(|d| d.severity == Severity::Error));
}

// =============================================================================
// Success cases
// =============================================================================

#[test]
fn type_i64_declaration() {
    let exprs = type_ok("let x: i64 = 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { name, ty, expr }, _) =>
        {
            assert_eq!(*name, "x");
            assert_eq!(*ty, Ty::I64);
            assert!(matches!(expr.as_ref(), Spanned(Expr::I64(42), _)));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_f64_declaration() {
    let exprs = type_ok("let x: f64 = 1.");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { name, ty, expr }, _) =>
        {
            assert_eq!(*name, "x");
            assert_eq!(*ty, Ty::F64);
            assert!(matches!(expr.as_ref(), Spanned(Expr::F64(_), _)));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_all_numeric_types_with_cast() {
    // every numeric type is valid as a cast target; spot-checks a few
    type_ok("let a: u8   = u8   42");
    type_ok("let b: u16  = u16  42");
    type_ok("let c: u32  = u32  42");
    type_ok("let d: u64  = u64  42");
    type_ok("let e: u128 = u128 42");
    type_ok("let f: i8   = i8   42");
    type_ok("let g: i16  = i16  42");
    type_ok("let h: i32  = i32  42");
    type_ok("let i: i128 = i128 42");
    type_ok("let j: isize = isize 42");
    type_ok("let k: usize = usize 42");
    type_ok("let l: f32  = f32  1.");
}

#[test]
fn type_cast_cross_numeric_types() {
    // all casts are currently accepted regardless of inner expression type
    type_ok("let x: f64 = f64 42"); // i64 → f64
    type_ok("let x: i32 = i32 1."); // f64 → i32
    type_ok("let x: u8  = u8  -5"); // signed → unsigned
}

#[test]
fn type_ident_resolved_from_env() {
    // the ident's type should be filled in from the environment
    let exprs = type_ok("let x: i64 = 42  let y: i64 = x");
    assert_eq!(exprs.len(), 2);
    match &exprs[1]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(Expr::Ident { name, ty }, _) =>
            {
                assert_eq!(*name, "x");
                assert_eq!(*ty, Ty::I64);
            },
            _ => panic!("expected ident inside declaration"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_ident_chain_resolved() {
    // z depends on y depends on x; all should resolve without error
    let exprs = type_ok("let x: i64 = 42  let y: i64 = x  let z: i64 = y");
    assert_eq!(exprs.len(), 3);
    // all idents in the chain carry the resolved type
    match &exprs[2]
    {
        Spanned(Expr::Declaration { expr, .. }, _) =>
        {
            assert!(matches!(
                expr.as_ref(),
                Spanned(Expr::Ident { ty: Ty::I64, .. }, _)
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_cast_wrapping_ident() {
    // cast of a resolved ident — both the ident resolution and the cast should succeed
    type_ok("let x: i64 = 42  let y: i64 = i64 x");
}

#[test]
fn type_forward_reference() {
    // x references y which is declared after it — must succeed
    let exprs = type_ok("let x: i64 = y  let y: i64 = 42");
    assert_eq!(exprs.len(), 2);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) =>
        {
            assert!(matches!(
                expr.as_ref(),
                Spanned(
                    Expr::Ident {
                        name: "y",
                        ty:   Ty::I64,
                    },
                    _
                )
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_forward_reference_chain() {
    // x → z, y → x, z is a literal: all declared out of dependency order
    type_ok("let x: i64 = z  let y: i64 = x  let z: i64 = 42");
}

// =============================================================================
// Failure cases
// =============================================================================

#[test]
fn type_mismatch_float_literal_in_int_decl() { type_with_error("let x: i64 = 1."); }

#[test]
fn type_mismatch_int_literal_in_float_decl() { type_with_error("let x: f64 = 42"); }

#[test]
fn type_mismatch_cast_target_differs_from_decl() {
    // cast produces u32, but declaration expects i64
    type_with_error("let x: i64 = u32 42");
}

#[test]
fn type_mismatch_ident_type_differs_from_decl() {
    // x is i64, but y expects f64
    type_with_error("let x: i64 = 42  let y: f64 = x");
}

#[test]
fn type_undefined_variable() {
    let diags = type_err("let x: i64 = y");
    assert!(diags.iter().any(|d| d.message.contains("Undefined")));
}

#[test]
fn type_duplicate_declaration() {
    // same name twice in the same scope is an error regardless of type
    let diags = type_err("let x: i64 = 1  let x: i64 = 2");
    assert!(diags.iter().any(|d| d.message.contains("already declared")));

    let diags = type_err("let x: i64 = 42  let x: f64 = 1.");
    assert!(diags.iter().any(|d| d.message.contains("already declared")));
}

#[test]
fn type_undefined_propagates_through_cast() {
    // undefined variable inside a cast still produces an error
    type_with_error("let x: i64 = i64 y");
}

#[test]
fn type_all_errors_reported() {
    // both declarations are ill-typed; both errors must be reported, not just the first
    let diags = type_err("let x: i64 = 1.  let y: i64 = 1.");
    assert!(diags.len() >= 2);
    assert!(diags.iter().all(|d| d.severity == Severity::Error));
}
