use crate::parser::expr::{DeclKind, Expr, Ty, UnaryOpKind};
use crate::program::Program;
use crate::program::diagnostics::ErrorCode;
use crate::tests::{get_default_target_machine, int_ty};
use crate::{Args, Spanned};

fn parse_ok(input: &str) -> Vec<Spanned<Expr<'_>>> {
    let output = Program::lex(Args::default(), get_default_target_machine(), input).parse();
    output.state
}

fn parse_err(input: &str, expected: ErrorCode) {
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .env("__FHIA_INPUT", input)
        .args([
            "--include-ignored",
            "--exact",
            "tests::parser::__inner_parse",
        ])
        .output()
        .expect("failed to spawn subprocess");
    assert!(
        !output.status.success(),
        "expected parse error for: {input}"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(expected.title()),
        "expected {:?} in stderr for: {input}\nstderr: {stderr}",
        expected.title(),
    );
}

#[test]
#[ignore = "subprocess helper"]
fn __inner_parse() {
    let input = std::env::var("__FHIA_INPUT").unwrap();
    let _ = Program::lex(Args::default(), get_default_target_machine(), &input).parse();
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
fn parse_missing_assign() { parse_err("let x: i64 1", ErrorCode::DeclarationMalformed); }

#[test]
fn parse_missing_colon() {
    parse_err("let x i64 = 1", ErrorCode::DeclarationMalformed);
    parse_err("let xi64 = 1", ErrorCode::DeclarationMalformed);
}

#[test]
fn parse_missing_type() {
    parse_err("let x: = 1", ErrorCode::DeclarationMalformed);
    parse_err("let x = 1", ErrorCode::DeclarationMalformed);
}

#[test]
fn parse_unclosed_paren() { parse_err("let x: i64 = (1", ErrorCode::UnclosedDelimiter); }

#[test]
fn parse_unclosed_brace() { parse_err("let x: i64 = {1", ErrorCode::UnclosedDelimiter); }

#[test]
fn parse_invalid_token() { parse_err("let x: i64 = @", ErrorCode::InvalidToken); }

#[test]
fn parse_keyword_as_value() {
    // `let` is not a valid expression
    parse_err("let x: i64 = let", ErrorCode::DeclarationMalformed);
}

#[test]
fn parse_keyword_as_name() {
    // `let` cannot be used as an identifier name
    parse_err("let let: i64 = 1", ErrorCode::DeclarationMalformed);
}

#[test]
fn parse_incomplete_cast() {
    // a type token with no following expression is not valid
    parse_err("let x: i64 = i64", ErrorCode::UnexpectedEof);
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

// =============================================================================
// Negation (unary minus)
// =============================================================================

#[test]
#[ignore = "Pause this feature"]
fn parse_neg_int_lit() {
    let exprs = parse_ok("let x: i64 = -42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::Neg,
                    operand,
                },
                _,
            ) =>
            {
                assert!(matches!(
                    operand.as_ref(),
                    Spanned(Expr::IntLit { value: 42, .. }, _)
                ));
            },
            _ => panic!("expected Unary::Neg"),
        },
        _ => panic!("expected declaration"),
    }
}

#[allow(clippy::approx_constant)]
#[test]
#[ignore = "Pause this feature"]
fn parse_neg_float_lit() {
    let exprs = parse_ok("let x: f64 = -3.14");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::Neg,
                    operand,
                },
                _,
            ) =>
            {
                assert!(matches!(
                    operand.as_ref(),
                    Spanned(Expr::F64(v), _) if (*v - 3.14).abs() < 1e-9
                ));
            },
            _ => panic!("expected Unary::Neg"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
#[ignore = "Pause this feature"]
fn parse_neg_ident() {
    let exprs = parse_ok("let x: i64 = -y");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::Neg,
                    operand,
                },
                _,
            ) =>
            {
                assert!(matches!(
                    operand.as_ref(),
                    Spanned(Expr::Ident { name: "y", .. }, _)
                ));
            },
            _ => panic!("expected Unary::Neg"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
#[ignore = "Pause this feature"]
fn parse_double_neg() {
    // --42  ≡  -(-(42))
    let exprs = parse_ok("let x: i64 = --42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::Neg,
                    operand: outer,
                },
                _,
            ) =>
            {
                assert!(matches!(
                    outer.as_ref(),
                    Spanned(
                        Expr::Unary {
                            kind: UnaryOpKind::Neg,
                            ..
                        },
                        _
                    )
                ));
            },
            _ => panic!("expected outer Unary::Neg"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
#[ignore = "Pause this feature"]
fn parse_neg_grouped() {
    // -(42)  — parens are stripped, still a Neg wrapping an IntLit
    let exprs = parse_ok("let x: i64 = -(42)");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::Neg,
                    operand,
                },
                _,
            ) =>
            {
                assert!(matches!(
                    operand.as_ref(),
                    Spanned(Expr::IntLit { value: 42, .. }, _)
                ));
            },
            _ => panic!("expected Unary::Neg"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
#[ignore = "Pause this feature"]
fn parse_neg_in_cast() {
    // `i64 -42` — the cast's operand is the whole negation expression
    let exprs = parse_ok("let x: i64 = i64 -42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(Expr::Cast(ty, inner), _) =>
            {
                assert_eq!(*ty, int_ty(true, 64));
                assert!(matches!(
                    inner.as_ref(),
                    Spanned(
                        Expr::Unary {
                            kind: UnaryOpKind::Neg,
                            ..
                        },
                        _
                    )
                ));
            },
            _ => panic!("expected Cast"),
        },
        _ => panic!("expected declaration"),
    }
}

#[test]
#[ignore = "Pause this feature"]
fn parse_neg_of_cast() {
    // `-i64 42` — the negation wraps the whole cast expression
    let exprs = parse_ok("let x: i64 = -i64 42");
    assert_eq!(exprs.len(), 1);
    match &exprs[0]
    {
        Spanned(Expr::Declaration { expr, .. }, _) => match expr.as_ref()
        {
            Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::Neg,
                    operand,
                },
                _,
            ) =>
            {
                assert!(matches!(operand.as_ref(), Spanned(Expr::Cast(..), _)));
            },
            _ => panic!("expected Unary::Neg"),
        },
        _ => panic!("expected declaration"),
    }
}
