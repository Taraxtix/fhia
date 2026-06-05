use std::fmt::Display;

use crate::parser::expr::Expr;
pub const trait OpKind: Display {
    fn _binding_force(self) -> (usize, usize);
    fn kind_name(self) -> &'static str;
}

#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
pub enum UnaryOpKind {
    Neg,
}

impl UnaryOpKind {
    pub fn display(self, operand: &Expr<'_>) -> String {
        match self
        {
            Self::Neg => format!("-({operand})"),
        }
    }
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Self::Neg => f.write_str("-"),
        }
    }
}

const impl OpKind for UnaryOpKind {
    fn _binding_force(self) -> (usize, usize) {
        match self
        {
            Self::Neg => (0, 27),
        }
    }

    fn kind_name(self) -> &'static str {
        match self
        {
            Self::Neg => "negation",
        }
    }
}
