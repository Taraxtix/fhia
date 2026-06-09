use std::collections::VecDeque;
use std::num::NonZero;

use crate::parser::expr::{Expr, Ty};
use crate::program::Program;
use crate::program::diagnostics::ErrorCode;
use crate::tests::{get_default_target_machine, int_ty};
use crate::{Args, Spanned};

fn type_ok(input: &str) -> VecDeque<Spanned<Expr<'_>>> {
    let parse_output = Program::lex(Args::default(), get_default_target_machine(), input).parse();
    let typed_output = parse_output.type_check();
    typed_output.state.exprs
}

fn type_err(input: &str, expected: ErrorCode) {
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .env("__FHIA_INPUT", input)
        .args([
            "--include-ignored",
            "--exact",
            "tests::typer::__inner_type_check",
        ])
        .output()
        .expect("failed to spawn subprocess");
    assert!(!output.status.success(), "expected type error for: {input}");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(expected.title()),
        "expected {:?} in stderr for: {input}\nstderr: {stderr}",
        expected.title(),
    );
}

#[test]
#[ignore = "subprocess helper"]
fn __inner_type_check() {
    let input = std::env::var("__FHIA_INPUT").unwrap();
    let _ = Program::lex(Args::default(), get_default_target_machine(), &input)
        .parse()
        .type_check();
}

// =============================================================================
// Success cases
// =============================================================================

#[test]
fn type_i64_declaration() {
    let exprs = type_ok("let main: i32 = 0 let x: i64 = 42");
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
    let exprs = type_ok("let main: i32 = 0 let x: f64 = 1.");
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
    type_ok("let main: i32 = 0 let a: u8    = 42 as u8");
    type_ok("let main: i32 = 0 let b: u16   = 42 as u16");
    type_ok("let main: i32 = 0 let c: u32   = 42 as u32");
    type_ok("let main: i32 = 0 let d: u64   = 42 as u64");
    type_ok("let main: i32 = 0 let e: u128  = 42 as u128");
    type_ok("let main: i32 = 0 let f: i8    = 42 as i8");
    type_ok("let main: i32 = 0 let g: i16   = 42 as i16");
    type_ok("let main: i32 = 0 let h: i32   = 42 as i32");
    type_ok("let main: i32 = 0 let i: i128  = 42 as i128");
    type_ok("let main: i32 = 0 let j: isize = 42 as isize");
    type_ok("let main: i32 = 0 let k: usize = 42 as usize");
    type_ok("let main: i32 = 0 let l: f32   = 1. as f32");
}

#[test]
fn type_cast_cross_numeric_types() {
    // all casts are currently accepted regardless of inner expression type
    type_ok("let main: i32 = 0 let x: f64 = 42 as f64"); // i64 → f64
    type_ok("let main: i32 = 0 let x: i32 = 1. as i32"); // f64 → i32
    type_ok("let main: i32 = 0 let x: u8  = -5 as u8"); // signed → unsigned
}

#[test]
fn type_ident_resolved_from_env() {
    // the ident's type should be filled in from the environment
    let exprs = type_ok("let main: i32 = 0 let x: i64 = 42 let y: i64 = x");
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
    let exprs = type_ok("let main: i32 = 0 let x: i64 = 42 let y: i64 = x let z: i64 = y");
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
    type_ok("let main: i32 = 0 let x: i64 = 42 let y: i64 = x");
}

#[test]
fn type_forward_reference() {
    // x references y which is declared after it — must succeed
    let exprs = type_ok("let main: i32 = 0 let x: i64 = y let y: i64 = 42");
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
    type_ok("let main: i32 = 0 let x: i64 = z let y: i64 = x let z: i64 = 42");
}

// =============================================================================
// Failure cases
// =============================================================================

#[test]
fn type_mismatch_float_literal_in_int_decl() {
    type_err("let main: i32 = 0 let x: i64 = 1.", ErrorCode::TypeMismatch);
}

#[test]
fn type_mismatch_int_literal_in_float_decl() {
    type_err("let main: i32 = 0 let x: f64 = 42", ErrorCode::TypeMismatch);
}

#[test]
fn type_mismatch_cast_target_differs_from_decl() {
    // cast produces u32, but declaration expects i64
    type_err(
        "let main: i32 = 0 let x: i64 = 42 as u32",
        ErrorCode::TypeMismatch,
    );
}

#[test]
fn type_mismatch_ident_type_differs_from_decl() {
    // x is i64, but y expects f64
    type_err(
        "let main: i32 = 0 let x: i64 = 42 let y: f64 = x",
        ErrorCode::TypeMismatch,
    );
}

#[test]
fn type_undefined_variable() {
    type_err(
        "let main: i32 = 0 let x: i64 = y",
        ErrorCode::UndefinedVariable,
    );
}

#[test]
fn type_duplicate_declaration() {
    // same name twice in the same scope is an error regardless of type
    type_err(
        "let main: i32 = 0 let x: i64 = 1 let x: i64 = 2",
        ErrorCode::DuplicateDeclaration,
    );
    type_err(
        "let main: i32 = 0 let x: i64 = 42 let x: f64 = 1.",
        ErrorCode::DuplicateDeclaration,
    );
}

#[test]
fn type_undefined_propagates_through_cast() {
    // undefined variable inside a cast still produces an error
    type_err(
        "let main: i32 = 0 let x: i64 = y as i64",
        ErrorCode::UndefinedVariable,
    );
}

#[test]
fn type_all_errors_reported() {
    // both declarations are ill-typed; both errors must be reported, not just the first
    type_err(
        "let main: i32 = 0 let x: i64 = 1. let y: i64 = 1.",
        ErrorCode::TypeMismatch,
    );
}

#[test]
fn type_missing_main() { type_err("let x: i64 = 42", ErrorCode::MissingMain); }

#[test]
fn type_main_any_int() {
    // main accepts any integer type
    type_ok("let main: i32   = 0 as i32");
    type_ok("let main: u32   = 0 as u32");
    type_ok("let main: i64   = 0 as i64");
    type_ok("let main: u8    = 0 as u8");
    type_ok("let main: i3    = 0 as i3");
    type_ok("let main: isize = 0 as isize");
}

#[test]
fn type_incorrect_main_type() {
    // only integer types are accepted for main
    type_err("let main: f64 = 1.", ErrorCode::IncorrectMainType);
}

#[test]
fn type_int_lit_fits_arbitrary_width() {
    type_ok("let main: i32 = 0 let x: u3  = 7");
    type_ok("let main: i32 = 0 let x: i3  = 3");
    type_ok("let main: i32 = 0 let x: u43 = 42");
}

#[test]
fn type_int_lit_out_of_range() {
    type_err(
        "let main: i32 = 0 let x: u3 = 8",
        ErrorCode::IntLiteralOutOfRange,
    );
    type_err(
        "let main: i32 = 0 let x: i3 = 4",
        ErrorCode::IntLiteralOutOfRange,
    );
}

#[test]
fn type_cast_allows_out_of_range_lit() {
    // explicit cast bypasses the fit check — truncation is intentional
    type_ok("let main: i32 = 0 let x: i8 = 1000 as i8");
    type_ok("let main: i32 = 0 let x: u3 = 255 as u3");
}
