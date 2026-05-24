#![feature(stmt_expr_attributes)]

use std::{fs::read_to_string, ops::Range};

mod codegen;
mod const_eval;
mod lexer;
mod parser;
mod program;
#[cfg(test)]
mod tests;
mod topo_order;
mod typer;

use clap::Parser as clapParser;

use crate::program::{Program, diagnostics::Diagnostic};

#[allow(clippy::struct_excessive_bools)]
#[derive(clapParser, Default)]
struct Args {
    /// Input file to compile
    // TODO: Replace with when at a usable state #[arg(required = true)]
    #[arg(default_value_t = String::from("test.fhia"))]
    input: String,

    /// Output file where the binary will be written
    #[arg(short, long, default_value_t = String::from("a.out"))]
    output: String,

    // TODO: Re-add if there is something useful to print with chumsky
    // /// Print the Lexer's output
    // #[arg(long, default_value_t = false)]
    // lexer: bool,
    /// Print the Parser's output
    #[arg(long, default_value_t = false)]
    parser: bool,

    /// Print the Typer's output
    #[arg(long, default_value_t = false)]
    typer: bool,

    /// Print the LLVM IR
    #[arg(long, default_value_t = false)]
    llvm_ir: bool,

    /// Don't link std module
    #[arg(long, default_value_t = true)] // TODO: Change to false when implemented
    no_std: bool,
}

#[derive(Clone, Debug)]
pub struct Spanned<T>(pub T, pub Range<usize>);
pub type ParsingError = Diagnostic;

fn main() {
    let args = Args::parse();

    let input = read_to_string(&args.input).unwrap_or_else(|e| {
        eprintln!("Failed to read input file '{path}': {e}", path = args.input);
        std::process::exit(1);
    });

    Program::lex(args, &input)
        .parse()
        .type_check()
        .const_eval()
        .compile();
}
