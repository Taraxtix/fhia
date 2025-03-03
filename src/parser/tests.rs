use crate::lexer::{Lexer, Position};

use super::{expr::*, *};
use expr::ExprKind as EK;
use types::Type as T;

fn default_scope() -> Scope {
    Scope {
        env: Env {
            vars: vec![
                Var {
                    name: "argc".to_string(),
                    ty: T::Size,
                },
                Var {
                    name: "argv".to_string(),
                    ty: T::ConstRef(Box::new(T::ConstRef(Box::new(T::Char)))),
                },
            ],
            functions: vec![Func {
                name: "dbg".to_string(),
                args: vec![T::Any],
                ty: T::Unit,
            }],
        },
        parent: None,
    }
}

#[test]
fn test_lits() {
    let smpl_char_at = |c, line, column| Expr {
        kind: EK::Char(c),
        ty: Some(T::Char),
        span: Span {
            start: Position { line, column },
            end: Position {
                line,
                column: column + 3,
            },
        },
        scope: default_scope(),
    };

    let lexer = Lexer::new("tests/parser/correct/lits.fhia").unwrap();
    let actual = Parser::new(lexer)
        .collected
        .iter()
        .map(|e| (e.kind.clone(), e.ty.clone()))
        .collect::<Vec<_>>();

    let expected = vec![
        (EK::U32(42), Some(T::U32)),
        (EK::F64(42.69), Some(T::F64)),
        (EK::U32(0xDEADBEEF), Some(T::U32)),
        (EK::U64(0xDEADBEEFCAFE), Some(T::U64)),
        (EK::Bool(false), Some(T::Bool)),
        (EK::Str("Test".to_string()), Some(T::Str)),
        (EK::Char('T'), Some(T::Char)),
        (
            EK::Array(vec![
                smpl_char_at('A', 9, 2),
                smpl_char_at('B', 9, 7),
                smpl_char_at('C', 9, 12),
            ]),
            Some(T::Array {
                ty: Box::new(T::Char),
                len: 3,
            }),
        ),
        (
            EK::Array(vec![]),
            Some(T::Array {
                ty: Box::new(T::Any),
                len: 0,
            }),
        ),
    ];

    assert_eq!(expected, actual)
}
