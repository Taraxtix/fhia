#![feature(stmt_expr_attributes)]

use std::{fs::read_to_string, ops::Range};

mod codegen;
mod diagnostics;
mod lexer;
mod parser;
#[cfg(test)]
mod tests;
mod typer;
mod util;

use clap::Parser as clapParser;
use diagnostics::Reportable;

use crate::diagnostics::Diagnostic;

#[allow(clippy::struct_excessive_bools)]
#[derive(clapParser)]
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

    let parser_output = parser::parse(&input);
    parser_output.report(&input, &args.input);
    if args.parser
    {
        println!("-------------------------------------------------");
        for expr in &parser_output.exprs
        {
            println!("{}", expr.0);
        }
    }
    let typed_output = parser_output.type_check();
    typed_output.report(&input, &args.input);
    if args.typer
    {
        println!("-------------------------------------------------");
        for expr in &typed_output.exprs
        {
            println!("{}", expr.0);
        }
    }
    typed_output.compile(&args, &input);
}
