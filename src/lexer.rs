use std::{
    fmt::{Debug, Display},
    fs::read_to_string,
    io,
};

use regex::Regex;

#[derive(Debug, Clone)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{}:{}", self.line, self.column).as_str())
    }
}

#[derive(Clone, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Token {
    Ident(String),

    //Literals
    ILit(i32),
    FLit(f64),
    StrLit(String),
    CharLit(char),
    BoolLit(bool),

    ConstPtr,  // *const
    MutPtr,    // *mut
    ConstAddr, // &const
    MutAddr,   // &mut

    //Operators
    Plus,
    PlusAssign,
    Increment,
    Minus,
    Decrement,
    MinusAssign,
    Times,
    TimesAssign,
    Divide,
    DivideAssign,
    Modulo,
    ModuloAssign,
    LAnd,
    BAnd,
    AndAssign,
    LOr,
    BOr,
    OrAssign,
    LNot,
    NEqual,
    LAngle,
    LShift,
    LEq,
    RAngle,
    RShift,
    GEq,
    Equal,
    Assign,
    BNeg,
    Xor,
    XorAssign,

    // Delimiters
    LBracket,
    RBracket,
    Comma,
    RParen,
    LParen,
    LBrace,
    RBrace,
    Semicolon,
    Colon,
    Dot,

    // Ignored
    Whitespace,
    Comment,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

pub struct Lexer<'a> {
    path: &'a str,
    source: String,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(path: &'a str) -> io::Result<Self> {
        Ok(Self {
            path,
            source: read_to_string(path)?,
            pos: 0,
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Span, Token);

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
