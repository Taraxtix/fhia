pub(crate) mod expr;

use std::{fs::OpenOptions, io::Write};

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

use crate::diagnostics;
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
    .labelled("Litteral");

    let ident = select! {
        Token::Ident(name) => Expr::Ident{name, ty: Ty::Unknown},
    }
    .labelled("Identifier");

    let ty = select! {
        Token::Ty(ty) => ty,
    }
    .labelled("Type");

    recursive(|expr| {
        let expr = expr.labelled("Expr").as_context();

        let decla = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
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
            .labelled("Declaration")
            .as_context();

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
                .then_ignore(just(Token::RParen).labelled("RParen"))
                .labelled("Parenthesized expression")
                .as_context(),
        ))
    })
}

fn program<'a, I>() -> impl Parser<'a, I, Vec<Expr<'a>>, extra::Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    expr()
        .filter(|expr| matches!(expr, Expr::Declaration { .. }))
        .repeated()
        .collect()
}

#[derive(Debug)]
pub struct ParseOutput<'a> {
    pub exprs: Vec<Expr<'a>>,
    pub diagnostics: Vec<diagnostics::Diagnostic>,
}

pub fn parse(source: &str) -> ParseOutput<'_> {
    let mut diagnostics = Vec::new();
    let mut tokens = Vec::new();
    for (tok, span) in Token::lexer(source).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, Into::<SimpleSpan>::into(span)),
        Err(()) => (Token::Error, span.into()),
    }) {
        if tok == Token::Error {
            diagnostics.push(
                diagnostics::Diagnostic::error("Invalid token")
                    .with_main_label(span.into_range(), "invalid token"),
            );
        } else {
            tokens.push((tok, span));
        }
    }

    if !diagnostics.is_empty() {
        return ParseOutput {
            exprs: Vec::new(),
            diagnostics,
        };
    }

    let token_stream = Stream::from_iter(tokens.into_iter())
        .map((0..source.len()).into(), |(tok, span)| (tok, span));

    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open("parser.svg")
        .unwrap();

    let debug_info = program::<&[Token]>().debug().to_railroad_svg().to_string();
    file.write(debug_info.as_bytes()).unwrap();

    match program().parse(token_stream).into_result() {
        Ok(exprs) => ParseOutput {
            exprs,
            diagnostics: Vec::new(),
        },
        Err(errs) => {
            let diagnostics = errs
                .into_iter()
                .map(diagnostics::parser::from_chumsky)
                .collect();
            ParseOutput {
                exprs: Vec::new(),
                diagnostics,
            }
        }
    }
}
