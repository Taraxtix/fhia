use crate::lexer::Lexer;

use super::{expr::*, *};
use expr::ExprKind as EK;
use types::Type as T;

#[test]
fn test_lits() {
    let smpl_char_at = |c, line, column| Expr {
        kind: EK::Char(c),
        ty: Some(T::Char),
        span: Span::new_raw((line, column), (line, column + 3)),
        scope: Scope::default(),
    };

    let lexer = Lexer::new("tests/parser/correct/lits.fhia").unwrap();
    let actual = Parser::parse_user_program(lexer, &Args::default())
        .collected
        .iter()
        .filter(|i| matches!(i, Item::Expr(_)))
        .map(|e| {
            let Item::Expr(e) = e else { unreachable!() };
            e.kind.clone()
        })
        .collect::<Vec<_>>();

    let expected = vec![
        EK::U32(42),
        EK::F64(42.69),
        EK::U32(0xDEADBEEF),
        EK::U64(0xDEADBEEFCAFE),
        EK::Bool(false),
        EK::Str("Test".to_string()),
        EK::Char('T'),
        EK::Array(vec![
            smpl_char_at('A', 8, 2),
            smpl_char_at('B', 8, 7),
            smpl_char_at('C', 8, 12),
        ]),
        EK::Array(vec![]),
    ];

    assert_eq!(expected.len(), actual.len());
    for i in 0..expected.len() {
        assert_eq!(expected[i], actual[i]);
    }
}

#[test]
fn test_ops() {
    use BinOp as BO;
    let unop = |kind, arg| EK::UnOp {
        kind,
        arg: Box::new(arg),
    };

    let binop = |kind, lhs, rhs| EK::BinOp {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    };

    let u32 = |lit, line, column| Expr {
        kind: EK::U32(lit),
        ty: Some(T::U32),
        span: Span::new_raw((line, column), (line, column + 1)),
        scope: Scope::default(),
    };

    let lexer = Lexer::new("tests/parser/correct/ops.fhia").unwrap();
    let actual = Parser::parse_user_program(lexer, &Args::default())
        .collected
        .iter()
        .filter(|i| matches!(i, Item::Expr(_)))
        .map(|e| {
            let Item::Expr(e) = e else { unreachable!() };
            e.kind.clone()
        })
        .collect::<Vec<_>>();

    let expected = vec![
        binop(BO::Add, u32(1, 1, 1), u32(2, 1, 6)),
        binop(BO::Minus, u32(1, 2, 1), u32(2, 2, 6)),
        binop(BO::Mul, u32(1, 3, 1), u32(2, 3, 6)),
        binop(BO::Divide, u32(1, 4, 1), u32(2, 4, 6)),
        binop(BO::Power, u32(1, 5, 1), u32(2, 5, 6)),
        binop(BO::Modulo, u32(1, 6, 1), u32(2, 6, 6)),
        binop(BO::LShift, u32(1, 7, 1), u32(2, 7, 6)),
        binop(BO::RShift, u32(1, 8, 1), u32(2, 8, 6)),
        binop(BO::BAnd, u32(1, 9, 1), u32(2, 9, 6)),
        binop(BO::BOr, u32(1, 10, 1), u32(2, 10, 6)),
        binop(BO::Xor, u32(1, 11, 1), u32(2, 11, 6)),
        binop(BO::LAnd, u32(1, 12, 1), u32(2, 12, 6)),
        binop(BO::LOr, u32(1, 13, 1), u32(2, 13, 6)),
        binop(BO::Equal, u32(1, 14, 1), u32(2, 14, 6)),
        binop(BO::NEqual, u32(1, 15, 1), u32(2, 15, 6)),
        binop(BO::Lt, u32(1, 16, 1), u32(2, 16, 6)),
        binop(BO::LEq, u32(1, 17, 1), u32(2, 17, 6)),
        binop(BO::Gt, u32(1, 18, 1), u32(2, 18, 6)),
        binop(BO::GEq, u32(1, 19, 1), u32(2, 19, 6)),
        unop(UnOp::Minus, u32(1, 20, 2)),
        unop(UnOp::BNeg, u32(1, 21, 2)),
        unop(UnOp::LNot, u32(1, 22, 2)),
    ];

    assert_eq!(expected.len(), actual.len());
    for i in 0..expected.len() {
        assert_eq!(expected[i], actual[i]);
    }
}

#[test]
fn test_indexing() {
    let lexer = Lexer::new("tests/parser/correct/indexing.fhia").unwrap();
    let actual = Parser::parse_user_program(lexer, &Args::default())
        .collected
        .iter()
        .filter(|i| matches!(i, Item::Expr(_)))
        .map(|e| {
            let Item::Expr(e) = e else { unreachable!() };
            e.kind.clone()
        })
        .collect::<Vec<_>>();
    #[allow(clippy::useless_vec)]
    let expected = vec![EK::Index {
        expr: Box::new(Expr {
            kind: EK::Array(vec![Expr {
                kind: EK::Char('A'),
                ty: Some(T::Char),
                span: Span::new_raw((1, 2), (1, 5)),
                scope: Scope::default(),
            }]),
            ty: None,
            span: Span::new_raw((1, 1), (1, 6)),
            scope: Scope::default(),
        }),
        index: Box::new(Expr {
            kind: EK::U32(0),
            ty: Some(T::U32),
            span: Span::new_raw((1, 7), (1, 8)),
            scope: Scope::default(),
        }),
    }];

    assert_eq!(expected.len(), actual.len());
    for i in 0..expected.len() {
        assert_eq!(expected[i], actual[i]);
    }
}

#[test]
fn test_idents() {
    let lexer = Lexer::new("tests/parser/correct/ident.fhia").unwrap();
    let actual = Parser::parse_user_program(lexer, &Args::default())
        .collected
        .iter()
        .filter(|i| matches!(i, Item::Expr(_)))
        .map(|e| {
            let Item::Expr(e) = e else { unreachable!() };
            e.kind.clone()
        })
        .collect::<Vec<_>>();

    #[allow(clippy::useless_vec)]
    let expected = vec![
        EK::Var("argc".to_string()),
        EK::FuncCall {
            name: "dbg".to_string(),
            args: vec![Expr {
                kind: EK::Var("argc".to_string()),
                ty: Some(T::Size),
                span: Span::new_raw((2, 5), (2, 9)),
                scope: Scope::default(),
            }],
        },
        EK::FuncCall {
            name: "dbg".to_string(),
            args: vec![Expr {
                kind: EK::Var("argv".to_string()),
                ty: Some(T::c_ref(T::c_ref(T::Char))),
                span: Span::new_raw((3, 5), (3, 9)),
                scope: Scope::default(),
            }],
        },
        EK::Var("argv".to_string()),
    ];

    assert_eq!(expected.len(), actual.len());
    for i in 0..expected.len() {
        assert_eq!(expected[i], actual[i]);
    }
}
