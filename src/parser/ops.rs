use crate::{
    lexer::{Span, Token},
    parser::expr::ExprKind,
};

use super::{Expr, types::Type as T};

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

    fn accepted_types(&self) -> Vec<(T, T)> {
        use BinOp as Op;
        match self {
            Op::Add | Op::Minus => vec![
                (T::I8, T::I8),
                (T::I16, T::I16),
                (T::I32, T::I32),
                (T::I64, T::I64),
                (T::I128, T::I128),
                (T::U8, T::U8),
                (T::U16, T::U16),
                (T::U32, T::U32),
                (T::U64, T::U64),
                (T::U128, T::U128),
                (T::Size, T::Size),
                (T::F32, T::F32),
                (T::F64, T::F64),
                (T::F128, T::F128),
                (T::MutRef(Box::new(T::Any)), T::Size),
                (T::ConstRef(Box::new(T::Any)), T::Size),
            ],
            Op::Mul => todo!(),
            Op::Power => todo!(),
            Op::Divide => todo!(),
            Op::Modulo => todo!(),
            Op::BAnd => todo!(),
            Op::BOr => todo!(),
            Op::Xor => todo!(),
            Op::LShift => todo!(),
            Op::RShift => todo!(),
            Op::LAnd => todo!(),
            Op::LOr => todo!(),
            Op::Equal => todo!(),
            Op::NEqual => todo!(),
            Op::LEq => todo!(),
            Op::GEq => todo!(),
            Op::Lt => todo!(),
            Op::Gt => todo!(),
        }
    }

    pub fn get_expr(&self, curr: (Span, Token), lhs: Expr, rhs: Expr) -> Expr {
        match lhs {
            Expr {
                kind:
                    ExprKind::BinOp {
                        kind,
                        lhs: llhs,
                        rhs: lrhs,
                    },
                ty,
                ..
            } if kind.get_precedence() < self.get_precedence() => todo!("Expr with swapping"),
            _ => todo!("Expr without swapping"),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Minus,

    Decrement,
    Increment,

    DerefMut,
    DerefConst,
    //...
}
