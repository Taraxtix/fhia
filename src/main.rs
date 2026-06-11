#![feature(stmt_expr_attributes)]
#![feature(const_trait_impl)]
#![feature(box_patterns)]

use std::{fs::read_to_string, range::Range};

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
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};

use crate::program::{Program, diagnostics::Diagnostic};

#[allow(clippy::struct_excessive_bools)]
#[derive(clapParser, Default)]
pub struct Args {
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

    #[arg(long, default_value_t = TargetMachine::get_default_triple().as_str().to_string_lossy().into_owned())]
    target: String,

    /// Don't link std module
    #[arg(long, default_value_t = true)] // TODO: Change to false when implemented
    no_std: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub Range<usize>);
pub type ParsingError = Diagnostic;

fn main() {
    Target::initialize_all(&InitializationConfig::default());

    let args = Args::parse();

    let target_triple = TargetTriple::create(&args.target);
    let target_machine =
        Target::from_triple(&target_triple).expect("Target should be checked in main");

    let cpu = if args.target == TargetMachine::get_default_triple().to_string()
    {
        TargetMachine::get_host_cpu_name().to_string()
    }
    else
    {
        "generic".to_string()
    };

    let features = if args.target == TargetMachine::get_default_triple().to_string()
    {
        TargetMachine::get_host_cpu_features().to_string()
    }
    else
    {
        String::new()
    };

    let target_machine = target_machine
        .create_target_machine(
            &target_triple,
            &cpu,
            &features,
            inkwell::OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("Failed to create target machine");

    let input = read_to_string(&args.input).unwrap_or_else(|e| {
        eprintln!("Failed to read input file '{path}': {e}", path = args.input);
        std::process::exit(1);
    });

    Program::lex(args, target_machine, &input)
        .parse()
        .type_check()
        .const_eval()
        .compile();
}
