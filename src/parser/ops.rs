use crate::{
    lexer::{Span, Token},
    parser::expr::ExprKind,
};

use super::Expr;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Power,
    Divide,
    Modulo,

    BAnd,
    BOr,
    Xor,
    LShift,
    RShift,

    LAnd,
    LOr,
    Equal,
    NEqual,
    LEq,
    GEq,
    Lt,
    Gt,
}

impl BinOp {
    pub fn from_token(tok: &Token) -> Self {
        match tok {
            Token::Plus => Self::Add,
            Token::Minus => Self::Minus,
            Token::Times => Self::Mul,
            Token::Power => Self::Power,
            Token::Divide => Self::Divide,
            Token::Modulo => Self::Modulo,
            Token::LAnd => Self::LAnd,
            Token::BAnd => Self::BAnd,
            Token::LOr => Self::LOr,
            Token::BOr => Self::BOr,
            Token::NEqual => Self::NEqual,
            Token::LShift => Self::LShift,
            Token::LEq => Self::LEq,
            Token::RShift => Self::RShift,
            Token::GEq => Self::GEq,
            Token::Equal => Self::Equal,
            Token::Xor => Self::Xor,
            Token::LAngle => Self::Lt,
            Token::RAngle => Self::Gt,
            _ => unreachable!(),
        }
    }

    fn get_precedence(&self) -> u8 {
        use BinOp as Op;
        match self {
            Op::Power => 2,
            Op::Mul | Op::Divide | Op::Modulo => 3,
            Op::Add | Op::Minus => 4,
            Op::LShift | Op::RShift => 5,
            Op::Lt | Op::Gt | Op::LEq | Op::GEq => 6,
            Op::Equal | Op::NEqual => 7,
            Op::BAnd => 8,
            Op::Xor => 9,
            Op::BOr => 10,
            Op::LAnd => 11,
            Op::LOr => 12,
        }
    }

    pub fn get_expr(&self, lhs: Expr, rhs: Expr) -> Expr {
        match lhs {
            Expr {
                kind:
                    ExprKind::BinOp {
                        kind,
                        lhs: llhs,
                        rhs: lrhs,
                    },
                span: lspan,
                scope,
                ..
            } if kind.get_precedence() > self.get_precedence() => Expr {
                span: Span::new(lspan.start, rhs.span.end.clone()),
                scope: scope.clone(),
                kind: ExprKind::BinOp {
                    kind,
                    lhs: llhs,
                    rhs: Box::new(Expr {
                        span: Span::new(lrhs.span.start.clone(), rhs.span.end.clone()),
                        kind: ExprKind::BinOp {
                            kind: self.clone(),
                            lhs: lrhs,
                            rhs: Box::new(rhs),
                        },
                        ty: None,
                        scope,
                    }),
                },
                ty: None,
            },
            Expr {
                span: ref lspan,
                ref scope,
                ..
            } => Expr {
                span: Span::new(lspan.start.clone(), rhs.span.end.clone()),
                scope: scope.clone(),
                kind: ExprKind::BinOp {
                    kind: self.clone(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty: None,
            },
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Minus,

    // Decrement,
    // Increment,
    MutDeref,
    ConstDeref,

    LNot,
    BNeg,
}
impl UnOp {
    pub fn from_token(tok: &Token) -> Self {
        match tok {
            Token::Minus => Self::Minus,
            Token::ConstDeref => Self::ConstDeref,
            Token::MutDeref => Self::MutDeref,
            Token::Bang => Self::LNot,
            Token::BNeg => Self::BNeg,
            _ => unreachable!(),
        }
    }

    pub fn get_expr(&self, arg: Expr) -> Expr {
        Expr {
            span: arg.span.clone(),
            scope: arg.scope.clone(),
            kind: ExprKind::UnOp {
                kind: self.clone(),
                arg: Box::new(arg),
            },
            ty: None,
        }
    }
}
