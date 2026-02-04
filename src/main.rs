use std::fs::read_to_string;

mod parser;

use clap::Parser as clapParser;

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
    //
    // /// Print the Parser's output
    // #[arg(long, default_value_t = false)]
    // parser: bool,
    /// Print the Typer's output
    #[arg(long, default_value_t = false)]
    typer: bool,

    /// Don't link std module
    #[arg(long, default_value_t = true)] // TODO: Change to false when implemented
    no_std: bool,
}

fn main() {
    let args = Args::parse();

    let input = read_to_string(&args.input).unwrap_or_else(|e| {
        eprintln!(
            "Failed to read input file '{path}': {e}",
            path = &args.input
        );
        std::process::exit(1);
    });

    let result = parser::parse(&input);
    if let Some(output) = result.output() {
        println!("================ Output ==================");
        for expr in output {
            println!("{expr}");
        }
    } else {
        println!("NO OUTPUT");
    }

    if result.has_errors() {
        println!("================ Errors ==================");
        for err in result.errors() {
            eprintln!("{}: {}", err.span(), err.reason());
            eprintln!(
                "Previous context: {:#?}",
                err.contexts().collect::<Vec<_>>()
            );
        }
    }
}
