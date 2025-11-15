#![feature(string_into_chars, pattern, assert_matches)]
#![feature(str_from_raw_parts)]

mod compiler;
mod lexer;
mod modules;
mod parser;
mod typer;

use crate::lexer::Span;
use clap::Parser as clapParser;
use lexer::Lexer;
use parser::Parser;
use std::fmt::Display;

// Args struct
#[derive(clapParser, Debug)]
#[command(version, about, long_about = None)]
#[derive(Default)]
struct Args {
    /// Input file to compile
    // #[arg(required = true)]
    #[arg(default_value_t = String::from("test.fhia"))]
    input: String,

    /// Output file where the binary will be written
    #[arg(short, long, default_value_t = String::from("a.out"))]
    output: String,

    /// Print the Lexer's output
    #[arg(long, default_value_t = false)]
    lexer: bool,

    /// Print the Parser's output
    #[arg(long, default_value_t = false)]
    parser: bool,

    /// Print the Typer's output
    #[arg(long, default_value_t = false)]
    typer: bool,

    /// Enable debug mode
    #[arg(short, long, default_value_t = false)]
    debug: bool,

    /// Don't link std module
    #[arg(long, default_value_t = true)] // TODO: Change to false when implemented
    no_std: bool,
}

// Error
struct Error {
    prefix: &'static str,
    filename: String,
    span: Span,
    message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}:{}: {}",
            self.prefix, self.filename, self.span, self.message
        )
    }
}

fn main() {
    let args = Args::parse();

    let lexer = Lexer::new(&args.input).unwrap_or_else(|err| {
        println!("Failed to read file {}: {err}", args.input);
        std::process::exit(1);
    });

    if args.lexer {
        println!("---------------------------------------------");
        println!("Lexer output:\n");
        for (span, token) in lexer.clone() {
            println!("from {} to {}: {token}", span.start, span.end);
        }
        println!("---------------------------------------------");
    }

    let parser = Parser::parse_user_program(lexer, &args);

    if args.parser {
        if !args.lexer {
            println!("---------------------------------------------");
        }
        println!("Parser output:\n");
        for expr in parser.clone() {
            println!("{expr}")
        }
        println!("---------------------------------------------");
    }

    let typer = parser.check().unwrap_or_else(|err| {
        println!("Type checking error: {err}");
        std::process::exit(1);
    });

    if args.typer {
        if !args.parser && !args.lexer {
            println!("---------------------------------------------");
        }
        println!("Type checking output:\n");
        for expr in typer {
            println!("{expr}");
        }
        println!("---------------------------------------------");
    }

    // Compiler::new(&args).compile(program)
}
