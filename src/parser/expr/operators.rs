use std::fmt::Display;

use crate::{
    Spanned,
    parser::expr::{Expr, Ty},
};
pub const trait OpKind: Display {
    fn binding_force(self) -> (u8, u8);
    fn kind_name(self) -> &'static str;
}

#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
pub enum UnaryOpKind {
    Neg,
    As(Ty),
}

impl UnaryOpKind {
    pub fn display(self, operand: &Expr<'_>) -> String {
        match self
        {
            Self::Neg => format!("-({operand})"),
            Self::As(ty) => format!("({operand} as {ty})"),
        }
    }

    pub const fn to_expr_with_operand(self, operand: Box<Spanned<Expr<'_>>>) -> Expr<'_> {
        match self
        {
            Self::Neg => Expr::Unary {
                kind: Self::Neg,
                operand,
            },
            Self::As(ty) => Expr::Unary {
                kind: Self::As(ty),
                operand,
            },
        }
    }
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Self::Neg => f.write_str("-"),
            Self::As(ty) => f.write_fmt(format_args!("as {ty}")),
        }
    }
}

const impl OpKind for UnaryOpKind {
    fn binding_force(self) -> (u8, u8) {
        match self
        {
            Self::As(_) => (25, u8::MAX),
            Self::Neg => (u8::MAX, 26),
        }
    }

    fn kind_name(self) -> &'static str {
        match self
        {
            Self::Neg => "negation",
            Self::As(_) => "cast",
        }
    }
}
