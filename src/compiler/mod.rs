use crate::{Args, parser::Parser};

#[allow(dead_code)]
pub struct Compiler<'a> {
    parser: Parser<'a>,
    generated: String,
}

impl<'a> Compiler<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            generated: String::new(),
        }
    }

    pub fn compile(self, args: &Args) {
        _ = args;
        todo!()
    }
}
