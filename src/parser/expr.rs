use std::fmt::Display;

use crate::lexer::{Lexer, Span, Token};

use super::{
    Scope,
    ops::{BinOp, UnOp},
    types::Type,
};

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Var(String),
    FuncCall(String, Vec<Expr>),
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },

    U32(u32),
    U64(u64),
    U128(u128),
    F64(f64),
    Str(String),
    Bool(bool),
    Char(char),
    Array(Vec<Expr>),

    BinOp {
        kind: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnOp {
        kind: UnOp,
        arg: Box<Expr>,
    },

    Unit,
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Var(name) => write!(f, "var: {name}"),
            ExprKind::FuncCall(name, exprs) => {
                write!(f, "{name}(")?;
                match exprs.last() {
                    Some(e) => {
                        for i in 0..exprs.len() - 1 {
                            write!(f, " {},", exprs.get(i).unwrap())?;
                        }
                        write!(f, "{e} )")
                    }
                    None => write!(f, ")"),
                }
            }
            ExprKind::U32(val) => write!(f, "{val}"),
            ExprKind::U64(val) => write!(f, "{val}"),
            ExprKind::U128(val) => write!(f, "{val}"),
            ExprKind::F64(val) => write!(f, "{val}"),
            ExprKind::Str(val) => write!(f, "\"{val}\""),
            ExprKind::Bool(val) => write!(f, "{val}"),
            ExprKind::Char(val) => write!(f, "'{val}'"),
            ExprKind::Array(exprs) => {
                write!(f, "[")?;
                match exprs.last() {
                    Some(e) => {
                        for i in 0..exprs.len() - 1 {
                            write!(f, " {},", exprs.get(i).unwrap())?;
                        }
                        write!(f, " {e} ]")
                    }
                    None => write!(f, "]"),
                }
            }
            ExprKind::BinOp { kind, lhs, rhs } => {
                write!(f, "{kind:?}( {lhs}, {rhs} )")
            }
            ExprKind::UnOp { kind, arg } => write!(f, "{kind:?}( {arg} )"),
            ExprKind::Unit => write!(f, "()"),
            ExprKind::Index { expr, index } => write!(f, "{expr}[{index}]"),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
    pub span: Span,
    pub scope: Scope,
}

impl Expr {
    pub fn _report_warning(&self, lexer: &Lexer<'_>, msg: &str) {
        println!("[WARNING]: {}:{}: {msg}", lexer.path, self.span.start);
    }

    pub fn from_lit((span, tok): (Span, Token), scope: Scope) -> Self {
        let (kind, ty) = match tok {
            Token::U32Lit(lit) => (ExprKind::U32(lit), Type::U32),
            Token::U64Lit(lit) => (ExprKind::U64(lit), Type::U64),
            Token::U128Lit(lit) => (ExprKind::U128(lit), Type::U128),
            Token::FLit(lit) => (ExprKind::F64(lit), Type::F64),
            Token::StrLit(lit) => (ExprKind::Str(lit), Type::Str),
            Token::CharLit(lit) => (ExprKind::Char(lit), Type::Char),
            Token::BoolLit(lit) => (ExprKind::Bool(lit), Type::Bool),
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty_str = match &self.ty {
            Some(ty) => format!("{ty}"),
            None => "None".to_string(),
        };

        write!(f, "@{}: {}: {}", self.span.start, self.kind, ty_str)
    }
}
