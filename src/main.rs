#![feature(string_into_chars, pattern, assert_matches)]
#![feature(str_from_raw_parts)]
mod compiler;
mod lexer;
mod modules;
mod parser;

use clap::Parser as clapParser;
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;

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

    /// Only print the Lexer's output
    #[arg(long, default_value_t = false)]
    lexer: bool,

    /// Only print the Parser's output
    #[arg(long, default_value_t = false)]
    parser: bool,

    /// Enable debug mode
    #[arg(short, long, default_value_t = false)]
    debug: bool,

    /// Don't link std module
    #[arg(long, default_value_t = true)] // TODO: Change to false when implemented
    no_std: bool,
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
        println!("---------------------------------------------");
        println!("Parser output:\n");
        for expr in parser.clone() {
            println!("{expr}")
        }
        println!("---------------------------------------------");
    }

    Compiler::new().compile(parser, &args)
}
