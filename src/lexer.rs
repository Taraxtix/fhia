use std::fmt::Display;

use inkwell::targets::TargetMachine;
use logos::{Lexer as LogosLexer, Logos};

use crate::{
    Args,
    Spanned,
    parser::expr::Ty,
    program::{
        Lexer,
        Program,
        diagnostics::{Diagnostic, ErrorCode, Reportable},
    },
};

fn to_u128<'a>(lex: &LogosLexer<'a, Token<'a>>) -> u128 {
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

fn to_f64<'a>(lex: &LogosLexer<'a, Token<'a>>) -> f64 {
    lex.slice().parse().expect("Invalid float regex")
}

fn to_ty<'a>(lex: &LogosLexer<'a, Token<'a>>) -> Option<Ty> { lex.slice().try_into().ok() }

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip(r"(\s+)|//[^\n]*|/\*(.|\n)*\*/", allow_greedy = true))]
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
    #[token("-")]
    Minus,

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
            Self::Minus => f.write_str("-"),
        }
    }
}

impl<'src> Program<'src, Lexer<'src>> {
    pub fn lex(args: Args, target_machine: TargetMachine, source: &'src str) -> Self {
        let mut diagnostics = Vec::new();
        let mut tokens = Vec::new();
        for (tok, span) in Token::lexer(source).spanned().map(|(tok, span)| match tok
        {
            Ok(tok) => (tok, span),
            Err(()) => (Token::Error, span),
        })
        {
            if tok == Token::Error
            {
                diagnostics.push(
                    Diagnostic::error(ErrorCode::InvalidToken)
                        .with_main_label(span.into(), "Invalid token"),
                );
            }
            else
            {
                tokens.push(Spanned(tok, span.into()));
            }
        }

        diagnostics.report(source, &args.input);
        Self {
            args,
            target_machine,
            source,
            state: tokens,
        }
    }
}
