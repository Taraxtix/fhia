use std::fmt::Display;

use crate::parser::expr::Ty;
use logos::{Lexer, Logos};

fn to_i64<'a>(lex: &mut Lexer<'a, Token<'a>>) -> i64 {
    let mut lit_str = lex.slice().to_string();

    let parse_base_10 = |mut lit_str: String| {
        while let Some(new) = lit_str.strip_prefix("0") {
            lit_str = new.to_string()
        }
        if lit_str.is_empty() {
            return 0;
        }
        lit_str.parse().expect("Invalid int regex")
    };

    let neg = lit_str.starts_with('-');
    if neg {
        lit_str = lit_str
            .strip_prefix('-')
            .expect("Checked above")
            .to_string();
    }
    let lit = if lit_str.len() < 3 {
        parse_base_10(lit_str)
    } else {
        match &lit_str[..2] {
            "0b" => u64::from_str_radix(&lit_str[2..], 2).unwrap() as i64,
            "0o" => u64::from_str_radix(&lit_str[2..], 8).unwrap() as i64,
            "0x" => u64::from_str_radix(&lit_str[2..], 16).unwrap() as i64,
            _ => parse_base_10(lit_str),
        }
    };
    if neg { -lit } else { lit }
}

fn to_f64<'a>(lex: &mut Lexer<'a, Token<'a>>) -> f64 {
    lex.slice().parse().expect("Invalid float regex")
}

fn to_ty<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Ty {
    lex.slice().try_into().unwrap_or_else(|_| {
        println!("Invalid ty regex : {}", lex.slice());
        panic!()
    })
}

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"\s+")]
#[logos(export_dir = "export/graph.mmd")] // Add debug feature to update graph
pub enum Token<'src> {
    #[token(r"let")]
    Let,
    #[token("=")]
    Assign,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,

    #[regex(r"([iuf](32|64))|([iu](8|16|128|size))", to_ty, priority = 200)]
    Ty(Ty),

    #[regex(r"[+-]?((0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)|\d+)", to_i64)]
    I64(i64),

    #[regex(r"[+-]?([0-9]+\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?", to_f64)]
    F64(f64),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'src str),

    Error,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let => f.write_str("let"),
            Self::Assign => f.write_str("="),
            Self::I64(i) => f.write_fmt(format_args!("{i}")),
            Self::F64(float) => f.write_fmt(format_args!("f{float}")),
            Self::Ident(ident) => f.write_str(ident),
            Self::Ty(ty) => f.write_fmt(format_args!("{ty}")),
            Self::Error => f.write_str("ERROR"),
            Self::LParen => f.write_str("("),
            Self::RParen => f.write_str(")"),
            Self::LBrace => f.write_str("{"),
            Self::RBrace => f.write_str("}"),
            Self::Colon => f.write_str(":"),
        }
    }
}
