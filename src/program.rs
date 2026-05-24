use crate::Args;

pub mod diagnostics;
pub mod env;

use crate::{Spanned, parser::expr::Expr};

pub struct Program<'src, State> {
    pub args:   Args,
    pub source: &'src str,
    pub state:  State,
}

pub type Lexer<'src> = Vec<Spanned<crate::lexer::Token<'src>>>;

pub type Parsed<'src> = Vec<Spanned<Expr<'src>>>;
