use crate::{Args, lexer::Lexer, parser::Parser};

const STD_FILES: [(&str, &str); 6] = [
    ("../../std/io.fhia", include_str!("../../std/io.fhia")),
    (
        "../../std/option.fhia",
        include_str!("../../std/option.fhia"),
    ),
    ("../../std/panic.fhia", include_str!("../../std/panic.fhia")),
    (
        "../../std/process.fhia",
        include_str!("../../std/process.fhia"),
    ),
    (
        "../../std/result.fhia",
        include_str!("../../std/result.fhia"),
    ),
    ("../../std/str.fhia", include_str!("../../std/str.fhia")),
];

#[allow(dead_code)]
pub struct Compiler<'a> {
    generated: String,
    included_files: Vec<&'a str>,
}

impl Compiler<'_> {
    pub fn new() -> Self {
        Self {
            generated: String::new(),
            included_files: Vec::new(),
        }
    }

    fn add_std(&mut self, args: &Args) {
        self.generated
            .push_str(include_str!("../../static/primitives.h"));

        for (path, source) in STD_FILES {
            if !self.included_files.contains(&path) {
                self.add(Parser::parse_user_program(
                    Lexer::from_filename_and_source(path, source),
                    args,
                ));
            }
        }

        self.generated
            .push_str(include_str!("../../static/start.c"));
    }

    fn add(&mut self, parser: Parser) {
        _ = parser;
        todo!()
    }

    pub fn compile(&mut self, parser: Parser, args: &Args) {
        if !args.no_std {
            self.add_std(args);
        }

        self.add(parser);
    }
}
