use std::num::NonZero;

use crate::Spanned;
use crate::diagnostics::{Diagnostic, ErrorCode};
use crate::parser::{
    self,
    expr::{DeclKind, Expr, Ty},
};

fn int_ty(signed: bool, width: u32) -> Ty {
    Ty::Int {
        signed,
        width: NonZero::new(width).unwrap(),
    }
}

fn parse_ok(input: &str) -> Vec<Spanned<Expr<'_>>> {
    let output = parser::parse(input);
    assert!(
        output.diagnostics.is_empty(),
        "expected no diagnostics, got: {:?}",
        output.diagnostics
    );
    output.exprs
}

fn parse_err(input: &str) -> Vec<Diagnostic> {
    let output = parser::parse(input);
    assert!(
        !output.diagnostics.is_empty(),
        "expected diagnostics, got none"
    );
    output.diagnostics
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
    for input in inputs
    {
        let exprs = parse_ok(input);
        assert_eq!(exprs.len(), 1, "failed on: {input}");
        assert!(
            matches!(
                &exprs[0],
                Spanned(Expr::Declaration { name: "x", ty, .. }, _)
                    if *ty == int_ty(true, 64)
            ),
            "failed on: {input}"
        );
    }
}

#[test]
fn parse_declaration_i64() {
    let exprs = parse_ok("let x: i64 = 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
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
fn parse_declaration_let_mut() {
    let exprs = parse_ok("let mut x: i64 = 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(
            Expr::Declaration {
                kind,
                name,
                ty,
                expr,
            },
            _,
        ) =>
        {
            assert!(matches!(kind, DeclKind::Let { is_mut: true }));
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
fn parse_declaration_const() {
    let exprs = parse_ok("const x: i64 = 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(
            Expr::Declaration {
                kind,
                name,
                ty,
                expr,
            },
            _,
        ) =>
        {
            assert!(matches!(kind, DeclKind::Const));
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

#[allow(clippy::approx_constant)]
#[test]
fn parse_declaration_f64() {
    let exprs = parse_ok("let x: f64 = 3.14");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { name, ty, expr, .. }, _) =>
        {
            assert_eq!(*name, "x");
            assert_eq!(*ty, Ty::F64);
            assert!(matches!(expr.as_ref(), Spanned(Expr::F64(f), _) if (*f - 3.14).abs() < 1e-9));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_ident_in_rhs() {
    // an identifier on the RHS is valid syntax; its type is Unknown at parse time
    let exprs = parse_ok("let x: i64 = y");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(Expr::Ident { name, ty }, _) =>
            {
                assert_eq!(*name, "y");
                assert_eq!(*ty, Ty::Unknown);
            },
            _ => panic!("expected ident"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_cast_expression() {
    let exprs = parse_ok("let x: u32 = u32 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(Expr::Cast(ty, inner), _) =>
            {
                assert_eq!(*ty, int_ty(false, 32));
                assert!(matches!(
                    inner.as_ref(),
                    Spanned(Expr::IntLit { value: 42, .. }, _)
                ));
            },
            _ => panic!("expected cast"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_nested_cast() {
    // cast wrapping another cast — both with and without parens
    let check = |exprs: Vec<Spanned<Expr<'_>>>| {
        assert_eq!(exprs.len(), 1);
        match &exprs[0]
        {
            Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
            {
                Spanned(Expr::Cast(outer_ty, inner), _) =>
                {
                    assert_eq!(*outer_ty, int_ty(true, 64));
                    assert!(
                        matches!(inner.as_ref(), Spanned(Expr::Cast(ty, _), _) if *ty == int_ty(false, 32))
                    );
                },
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
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) =>
        {
            assert!(matches!(
                expr.as_ref(),
                Spanned(Expr::IntLit { value: 42, .. }, _)
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_braced_expression() {
    let exprs = parse_ok("let x: i64 = {42}");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) =>
        {
            assert!(matches!(
                expr.as_ref(),
                Spanned(Expr::IntLit { value: 42, .. }, _)
            ));
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
fn parse_nested_grouping() {
    // braces and parens are interchangeable grouping delimiters
    for input in [
        "let x: i64 = ((42))",
        "let x: i64 = {(42)}",
        "let x: i64 = ({42})",
    ]
    {
        let exprs = parse_ok(input);
        assert!(
            matches!(&exprs[0], Spanned(Expr::Declaration { expr, .. }, _) if matches!(expr.as_ref(), Spanned(Expr::IntLit { value: 42, .. }, _))),
            "failed on: {input}"
        );
    }
}

#[test]
fn parse_multiple_declarations() {
    let exprs = parse_ok("let a: i64 = 1  let b: f64 = 2.");
    assert_eq!(exprs.len(), 2);
    assert!(matches!(
        &exprs[0],
        Spanned(Expr::Declaration { name: "a", ty, .. }, _) if *ty == int_ty(true, 64)
    ));
    assert!(matches!(
        &exprs[1],
        Spanned(
            Expr::Declaration {
                name: "b",
                ty: Ty::F64,
                ..
            },
            _
        )
    ));
}

// =============================================================================
// Failure cases
// =============================================================================

#[test]
fn parse_missing_assign() {
    let diags = parse_err("let x: i64 1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_missing_colon() {
    let diags = parse_err("let x i64 = 1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
    let diags = parse_err("let xi64 = 1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_missing_type() {
    let diags = parse_err("let x: = 1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
    let diags = parse_err("let x = 1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_unclosed_paren() {
    let diags = parse_err("let x: i64 = (1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_unclosed_brace() {
    let diags = parse_err("let x: i64 = {1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_invalid_token() {
    let diags = parse_err("let x: i64 = @");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::InvalidToken as u32))
    );
}

#[test]
fn parse_keyword_as_value() {
    // `let` is not a valid expression
    let diags = parse_err("let x: i64 = let");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_keyword_as_name() {
    // `let` cannot be used as an identifier name
    let diags = parse_err("let let: i64 = 1");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_incomplete_cast() {
    // a type token with no following expression is not valid
    let diags = parse_err("let x: i64 = i64");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::DeclarationMalformed as u32))
    );
}

#[test]
fn parse_arbitrary_width_type() {
    let exprs = parse_ok("let x: u3 = 7");
    assert_eq!(exprs.len(), 1);
    assert!(matches!(
        &exprs[0],
        Spanned(Expr::Declaration { ty, .. }, _) if *ty == int_ty(false, 3)
    ));
}

#[test]
fn parse_type_width_too_large() {
    // u129 and wider are not valid types
    let diags = parse_err("let x: u129 = 0");
    assert!(
        diags
            .iter()
            .any(|d| d.code == Some(ErrorCode::InvalidToken as u32))
    );
}
