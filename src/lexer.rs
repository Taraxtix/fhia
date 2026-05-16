use std::fmt::Display;

use logos::{Lexer, Logos};

use crate::parser::expr::Ty;

fn to_u128<'a>(lex: &Lexer<'a, Token<'a>>) -> u128 {
    let s = lex.slice();
    if s.len() >= 3
    {
        match &s[..2]
        {
            "0b" => u128::from_str_radix(&s[2..], 2).unwrap(),
            "0o" => u128::from_str_radix(&s[2..], 8).unwrap(),
            "0x" => u128::from_str_radix(&s[2..], 16).unwrap(),
            _ => s.parse().unwrap(),
        }
    }
    else
    {
        s.parse().unwrap()
    }
}

fn to_f64<'a>(lex: &Lexer<'a, Token<'a>>) -> f64 {
    lex.slice().parse().expect("Invalid float regex")
}

fn to_ty<'a>(lex: &Lexer<'a, Token<'a>>) -> Option<Ty> { lex.slice().try_into().ok() }

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip(r"(\s+)|//[^\n]*", allow_greedy = true))]
pub enum Token<'src> {
    #[token(r"let")]
    Let,
    #[token(r"const")]
    Const,
    #[token(r"mut")]
    Mut,
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

    #[regex(r"(f(32|64))|([iu](([1-9][0-9]*)|size))", to_ty, priority = 200)]
    Ty(Ty),

    #[regex(r"((0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)|\d+)", to_u128)]
    ILit(u128),

    #[regex(r"([0-9]+\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?", to_f64)]
    FLit(f64),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'src str),

    Error,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Self::Let => f.write_str("let"),
            Self::Assign => f.write_str("="),
            Self::ILit(i) => f.write_fmt(format_args!("{i}")),
            Self::FLit(float) => f.write_fmt(format_args!("f{float}")),
            Self::Ident(ident) => f.write_str(ident),
            Self::Ty(ty) => f.write_fmt(format_args!("{ty}")),
            Self::Error => f.write_str("ERROR"),
            Self::LParen => f.write_str("("),
            Self::RParen => f.write_str(")"),
            Self::LBrace => f.write_str("{"),
            Self::RBrace => f.write_str("}"),
            Self::Colon => f.write_str(":"),
            Self::Const => f.write_str("const"),
            Self::Mut => f.write_str("mut"),
        }
    }
}
