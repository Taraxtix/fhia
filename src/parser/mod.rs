use std::{fs::OpenOptions, io::Write};

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

pub(crate) mod expr;
use expr::*;

use crate::lexer::Token;

fn expr<'a, I>() -> impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let litteral = select! {
        Token::I64(i) => Expr::I64(i),
        Token::F64(f) => Expr::F64(f),
    }
    .labelled("litteral");

    let ident = select! {
        Token::Ident(name) => Expr::Ident{name, ty: Ty::Unknown},
    }
    .labelled("Identifier");

    let ty = select! {
        Token::Ty(ty) => ty,
    }
    .labelled("Type");

    recursive(|expr| {
        let expr = expr.labelled("Expr");

        let decla = just(Token::Let)
            .labelled("Let")
            .ignore_then(ident)
            .then_ignore(just(Token::Assign).labelled("="))
            .then(expr.clone())
            .map(|(ident, expr)| {
                let Expr::Ident { name, ty } = ident else {
                    unreachable!()
                };
                Expr::Declaration {
                    name: name,
                    ty: ty,
                    expr: Box::new(expr),
                }
            })
            .labelled("Declaration");

        let cast = ty
            .then(expr)
            .map(|(ty, expr)| Expr::Cast(ty, Box::new(expr)))
            .labelled("Cast");

        choice((decla, litteral, cast))
    })
}

fn program<'a, I>() -> impl Parser<'a, I, Vec<Expr<'a>>, extra::Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    expr()
        .filter(|expr| matches!(expr, Expr::Declaration { .. }))
        .labelled("Declaration")
        .repeated()
        .collect()
}

pub fn parse(source: &str) {
    let tok_iter = Token::lexer(source).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, Into::<SimpleSpan>::into(span)),
        Err(()) => (Token::Error, span.into()),
    });

    let token_stream =
        Stream::from_iter(tok_iter).map((0..source.len()).into(), |(tok, span)| (tok, span));

    // DEBUG
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("parser.svg")
        .unwrap();

    let debug_info = program::<&[Token]>().debug().to_railroad_svg().to_string();
    file.write(debug_info.as_bytes()).unwrap();
    // DEBUG

    match program().parse(token_stream).into_result() {
        Ok(exprs) => {
            for expr in exprs {
                println!("{expr}");
            }
        }
        Err(errs) => {
            println!("=========================");
            for err in errs {
                println!("{err}");
            }
            println!("=========================");
        }
    }
}
