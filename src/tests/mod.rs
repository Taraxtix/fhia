use std::num::NonZero;

use crate::parser::expr::Ty;

mod const_eval;
mod lexer;
mod parser;
mod typer;

fn int_ty(signed: bool, width: u32) -> Ty {
    Ty::Int {
        signed,
        width: unsafe { NonZero::new_unchecked(width) },
    }
}
