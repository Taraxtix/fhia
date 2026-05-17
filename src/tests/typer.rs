use std::collections::VecDeque;
use std::num::NonZero;

use crate::Spanned;
use crate::diagnostics::{Diagnostic, ErrorCode};
use crate::parser::{
    self,
    expr::{Expr, Ty},
};
use crate::tests::int_ty;

fn type_ok(input: &str) -> VecDeque<Spanned<Expr<'_>>> {
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

fn type_err(input: &str) -> Vec<Diagnostic> {
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

// =============================================================================
// Success cases
// =============================================================================

#[test]
fn type_i64_declaration() {
    let exprs = type_ok("let main: i32 = i32 0  let x: i64 = 42");
    assert_eq!(exprs.len(), 2);
    match &exprs[1]
    {
        Spanned(Expr::Declaration { name, ty, expr, .. }, _) =>
        {
            assert_eq!(*name, "x");
            assert_eq!(*ty, int_ty(true, 64));
            assert!(matches!(
                expr.as_ref(),
                Spanned(Expr::IntLit { value: 42, .. }, _)
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_f64_declaration() {
    let exprs = type_ok("let main: i32 = i32 0  let x: f64 = 1.");
    assert_eq!(exprs.len(), 2);
    match &exprs[1]
    {
        Spanned(Expr::Declaration { name, ty, expr, .. }, _) =>
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
    type_ok("let main: i32 = i32 0  let a: u8   = u8   42");
    type_ok("let main: i32 = i32 0  let b: u16  = u16  42");
    type_ok("let main: i32 = i32 0  let c: u32  = u32  42");
    type_ok("let main: i32 = i32 0  let d: u64  = u64  42");
    type_ok("let main: i32 = i32 0  let e: u128 = u128 42");
    type_ok("let main: i32 = i32 0  let f: i8   = i8   42");
    type_ok("let main: i32 = i32 0  let g: i16  = i16  42");
    type_ok("let main: i32 = i32 0  let h: i32  = i32  42");
    type_ok("let main: i32 = i32 0  let i: i128 = i128 42");
    type_ok("let main: i32 = i32 0  let j: isize = isize 42");
    type_ok("let main: i32 = i32 0  let k: usize = usize 42");
    type_ok("let main: i32 = i32 0  let l: f32  = f32  1.");
}

#[test]
fn type_cast_cross_numeric_types() {
    // all casts are currently accepted regardless of inner expression type
    type_ok("let main: i32 = i32 0  let x: f64 = f64 42"); // i64 → f64
    type_ok("let main: i32 = i32 0  let x: i32 = i32 1."); // f64 → i32
    // TODO: Bring back this test once `UnaryMinus` is on // type_ok("let main: i32 = i32 0  let x: u8  = u8  -5"); // signed → unsigned
}

#[test]
fn type_ident_resolved_from_env() {
    // the ident's type should be filled in from the environment
    let exprs = type_ok("let main: i32 = i32 0  let x: i64 = 42  let y: i64 = x");
    assert_eq!(exprs.len(), 3);
    match &exprs[2]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(Expr::Ident { name, ty }, _) =>
            {
                assert_eq!(*name, "x");
                assert_eq!(*ty, int_ty(true, 64));
            },
            _ => panic!("expected ident inside declaration"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_ident_chain_resolved() {
    // z depends on y depends on x; all should resolve without error
    let exprs = type_ok("let main: i32 = i32 0  let x: i64 = 42  let y: i64 = x  let z: i64 = y");
    assert_eq!(exprs.len(), 4);
    // all idents in the chain carry the resolved type
    match &exprs[3]
    {
        Spanned(Expr::Declaration { expr, .. }, _) =>
        {
            assert!(matches!(
                expr.as_ref(),
                Spanned(
                    Expr::Ident {
                        ty:
                            Ty::Int {
                            signed: true,
                            width,
                        } ,
                        ..
                    },
                    _,
                ) if width == &NonZero::new(64).unwrap()
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_cast_wrapping_ident() {
    // cast of a resolved ident — both the ident resolution and the cast should succeed
    type_ok("let main: i32 = i32 0  let x: i64 = 42  let y: i64 = i64 x");
}

#[test]
fn type_forward_reference() {
    // x references y which is declared after it — must succeed
    let exprs = type_ok("let main: i32 = i32 0  let x: i64 = y  let y: i64 = 42");
    assert_eq!(exprs.len(), 3);
    match &exprs[1]
    {
        Spanned(Expr::Declaration { expr, .. }, _) =>
        {
            assert!(matches!(
                expr.as_ref(),
                Spanned(
                    Expr::Ident {
                        name: "y",
                        ty:   Ty::Int{signed:true, width},
                    },
                    _
                )if width == &NonZero::new(64).unwrap()
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn type_forward_reference_chain() {
    // x → z, y → x, z is a literal: all declared out of dependency order
    type_ok("let main: i32 = i32 0  let x: i64 = z  let y: i64 = x  let z: i64 = 42");
}

// =============================================================================
// Failure cases
// =============================================================================

#[test]
fn type_mismatch_float_literal_in_int_decl() {
    let diags = type_err("let main: i32 = i32 0  let x: i64 = 1.");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::TypeMismatch as u32))
    );
}

#[test]
fn type_mismatch_int_literal_in_float_decl() {
    let diags = type_err("let main: i32 = i32 0  let x: f64 = 42");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::TypeMismatch as u32))
    );
}

#[test]
fn type_mismatch_cast_target_differs_from_decl() {
    // cast produces u32, but declaration expects i64
    let diags = type_err("let main: i32 = i32 0  let x: i64 = u32 42");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::TypeMismatch as u32))
    );
}

#[test]
fn type_mismatch_ident_type_differs_from_decl() {
    // x is i64, but y expects f64
    let diags = type_err("let main: i32 = i32 0  let x: i64 = 42  let y: f64 = x");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::TypeMismatch as u32))
    );
}

#[test]
fn type_undefined_variable() {
    let diags = type_err("let main: i32 = i32 0  let x: i64 = y");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::UndefinedVariable as u32))
    );
}

#[test]
fn type_duplicate_declaration() {
    // same name twice in the same scope is an error regardless of type
    let diags = type_err("let main: i32 = i32 0  let x: i64 = 1  let x: i64 = 2");
    dbg!(&diags);
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DuplicateDeclaration as u32))
    );

    let diags = type_err("let main: i32 = i32 0  let x: i64 = 42  let x: f64 = 1.");
    dbg!(&diags);
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DuplicateDeclaration as u32))
    );
}

#[test]
fn type_undefined_propagates_through_cast() {
    // undefined variable inside a cast still produces an error
    let diags = type_err("let main: i32 = i32 0  let x: i64 = i64 y");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::UndefinedVariable as u32))
    );
}

#[test]
fn type_all_errors_reported() {
    // both declarations are ill-typed; both errors must be reported, not just the first
    let diags = type_err("let main: i32 = i32 0  let x: i64 = 1.  let y: i64 = 1.");
    assert!(diags.len() >= 2);
    assert!(
        diags
            .iter()
            .all(|d| d.code == Some(ErrorCode::TypeMismatch as u32))
    );
}

#[test]
fn type_missing_main() {
    let diags = type_err("let x: i64 = 42");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::MissingMain as u32))
    );
}

#[test]
fn type_main_any_int() {
    // main accepts any integer type
    type_ok("let main: i32  = i32  0");
    type_ok("let main: u32  = u32  0");
    type_ok("let main: i64  = i64  0");
    type_ok("let main: u8   = u8   0");
    type_ok("let main: i3   = i3   0");
    type_ok("let main: isize = isize 0");
}

#[test]
fn type_incorrect_main_type() {
    // only integer types are accepted for main
    let diags = type_err("let main: f64 = 1.");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::IncorrectMainType as u32))
    );
}

#[test]
fn type_int_lit_fits_arbitrary_width() {
    type_ok("let main: i32 = i32 0  let x: u3  = 7");
    type_ok("let main: i32 = i32 0  let x: i3  = 3");
    type_ok("let main: i32 = i32 0  let x: u43 = 42");
}

#[test]
fn type_int_lit_out_of_range() {
    let diags = type_err("let main: i32 = i32 0  let x: u3 = 8");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::IntLiteralOutOfRange as u32))
    );
    let diags = type_err("let main: i32 = i32 0  let x: i3 = 4");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::IntLiteralOutOfRange as u32))
    );
}

#[test]
fn type_cast_allows_out_of_range_lit() {
    // explicit cast bypasses the fit check — truncation is intentional
    type_ok("let main: i32 = i32 0  let x: i8 = i8 1000");
    type_ok("let main: i32 = i32 0  let x: u3 = u3 255");
}
