pub(crate) mod expr;

use std::{fs::OpenOptions, io::Write};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

use crate::lexer::Token;
use expr::*;

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
            .then(expr.clone())
            .map(|(ty, expr)| Expr::Cast(ty, Box::new(expr)))
            .labelled("Cast");

        choice((
            decla,
            litteral,
            cast,
            just(Token::LParen)
                .labelled("LParen")
                .ignore_then(expr)
                .then_ignore(just(Token::RParen).labelled("RParen")),
        ))
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
            for err in errs {
                Report::build(ReportKind::Error, ((), err.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_code(1)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(((), err.span().into_range()))
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(source))
                    .unwrap();
            }
        }
    }
}
