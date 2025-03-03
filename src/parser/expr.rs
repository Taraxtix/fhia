use crate::lexer::{Lexer, Span, Token};

use super::{
    Scope,
    ops::{BinOp, UnOp},
    types::Type,
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ExprKind {
    Var(String),
    FuncCall(String, Vec<Expr>),

    ILit(i32),
    FLit(f64),
    StrLit(String),
    BoolLit(bool),
    CharLit(char),
    ArrayLit(Vec<Expr>),

    BinOp {
        kind: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnOp {
        kind: UnOp,
        arg: Box<Expr>,
    },
    //...
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
    pub span: Span,
    pub scope: Scope,
}

impl Expr {
    pub fn report_warning(&self, lexer: &Lexer<'_>, msg: &str) {
        println!("[WARNING]: {}:{}: {msg}", lexer.path, self.span.start);
    }

    pub fn from_lit((span, tok): (Span, Token), scope: Scope) -> Self {
        let (kind, ty) = match tok {
            Token::ILit(lit) => (ExprKind::ILit(lit), Type::I32),
            Token::FLit(lit) => (ExprKind::FLit(lit), Type::F64),
            Token::StrLit(lit) => (ExprKind::StrLit(lit), Type::Str),
            Token::CharLit(lit) => (ExprKind::CharLit(lit), Type::Char),
            Token::BoolLit(lit) => (ExprKind::BoolLit(lit), Type::Bool),
            _ => unreachable!(),
        };
        Self {
            kind,
            ty: Some(ty),
            span,
            scope,
        }
    }
}
