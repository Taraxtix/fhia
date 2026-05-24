pub mod expr;
#[macro_use]
pub mod combinator;

use std::collections::VecDeque;

use combinator::ParsedItem;
use expr::{DeclKind, Expr};
use lexparse::{Parser as _, ParserItem};

use crate::{
    ParsingError,
    Spanned,
    lexer::Token,
    parser::expr::Ty,
    program::{
        Lexer,
        Parsed,
        Program,
        diagnostics::{Diagnostic, ErrorCode, Reportable},
    },
};

type ParserOutput<'a, T> = Result<T, Vec<ParsingError>>;

fn parse_litteral<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::ILit(_), "a litteral")
        .or(|prev| {
            prev.push_back_input(input);
            just!(input, Token::FLit(_), "a litteral")
        })
        .map(|item| {
            item.token_lit_as_expr()
                .expect("just! should ensure that the token is a litteral")
        })
}

fn parse_ident<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::Ident(_), "an identifier").map(|tok| {
        assume!(let ParsedItem::Token(consumed, Spanned(Token::Ident(name), span)) = tok);
        ParsedItem::Expr(
            consumed,
            Spanned(
                Expr::Ident {
                    name,
                    ty: Ty::Unknown,
                },
                span,
            ),
        )
    })
}

fn parse_cast<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::Ty(_), "a type")
        .then(|| parse_expr(input))
        .map(|item| {
            assume!(let Seq[
                ParsedItem::Token(mut consumed, Spanned(Token::Ty(ty), ty_span)),
                ParsedItem::Expr(expr_consumed, Spanned(expr, expr_span)),
                ] = item);
            let range = ty_span.start..expr_span.end;
            consumed.extend(expr_consumed);
            ParsedItem::Expr(
                consumed,
                Spanned(Expr::Cast(ty, Box::new(Spanned(expr, expr_span))), range),
            )
        })
}

fn parse_opt_mut<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    if matches!(input.front(), Some(Spanned(Token::Mut, _)))
    {
        let tok = input.pop_front().unwrap();
        ParsedItem::Opt(
            VecDeque::from([tok.clone()]),
            Some(Box::new(ParsedItem::Token(VecDeque::new(), tok))),
        )
    }
    else
    {
        ParsedItem::Opt(VecDeque::new(), None)
    }
}

fn parse_decla<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    let result = just!(input, Token::Let, "`let`", ErrorCode::DeclarationMalformed)
        .then(|| parse_opt_mut(input))
        .or(ParsedItem::or(
            |input| {
                just!(
                    input,
                    Token::Const,
                    "`const`",
                    ErrorCode::DeclarationMalformed
                )
                .then(|| ParsedItem::Opt(VecDeque::new(), None))
            },
            input,
        ))
        .then(|| parse_ident(input))
        .then_ignore(
            ParsedItem::then_ignore(
                |input| just!(input, Token::Colon, "`:`", ErrorCode::DeclarationMalformed),
                input,
            ),
            ParsedItem::then_ignore_on_error(),
        )
        .then(|| {
            just!(
                input,
                Token::Ty(_),
                "a type",
                ErrorCode::DeclarationMalformed
            )
        })
        .then_ignore(
            ParsedItem::then_ignore(
                |input| just!(input, Token::Assign, "`=`", ErrorCode::DeclarationMalformed),
                input,
            ),
            ParsedItem::then_ignore_on_error(),
        )
        .then(|| parse_expr(input))
        .map(|item| {
            assume!(let Seq[
                ParsedItem::Token(mut consumed, Spanned(decl_kind, decl_kind_span)),
                ParsedItem::Opt(opt_consumed, opt_mut),
                ParsedItem::Expr(ident_consumed, Spanned(Expr::Ident { name, .. }, _)),
                ParsedItem::Token(ty_consumed, Spanned(Token::Ty(ty), _)),
                ParsedItem::Expr(expr_consumed, Spanned(expr, expr_span)),
            ] = item);
            let kind = match (&decl_kind, opt_mut.is_some())
            {
                (Token::Let, true) => DeclKind::Let { is_mut: true },
                (Token::Let, false) => DeclKind::Let { is_mut: false },
                (Token::Const, _) => DeclKind::Const,
                _ => unreachable!(),
            };
            let span = decl_kind_span.start..expr_span.end;
            consumed.extend(opt_consumed);
            consumed.extend(ident_consumed);
            consumed.extend(ty_consumed);
            consumed.extend(expr_consumed);
            ParsedItem::Expr(
                consumed,
                Spanned(
                    Expr::Declaration {
                        kind,
                        name,
                        ty,
                        expr: Box::new(Spanned(expr, expr_span)),
                    },
                    span,
                ),
            )
        });
    match result.into_output()
    {
        Ok(expr) => ParsedItem::Expr(VecDeque::new(), expr),
        Err((consumed, mut diag)) =>
        {
            let code = ErrorCode::DeclarationMalformed;
            diag.message = code.title().to_string();
            diag.code = Some(code as u32);
            ParsedItem::Err(consumed, diag)
        },
    }
}

fn parse_expr<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    // Commit to a declaration as soon as `let`, `mut` or `const` is seen — no backtracking to
    // other alternatives if the declaration is malformed.
    if matches!(input.front(), Some(Spanned(Token::Let | Token::Const, _)))
    {
        return parse_decla(input);
    }
    parse_litteral(input)
        .or(ParsedItem::or(parse_ident, input))
        .or(ParsedItem::or(parse_cast, input))
        .or(ParsedItem::or(
            |input| {
                just!(input, Token::LParen, "`(`")
                    .ignore_then(ParsedItem::ignore_then(|input| parse_expr(input), input))
                    .then_ignore(
                        ParsedItem::then_ignore(|input| just!(input, Token::RParen, "`)`"), input),
                        ParsedItem::then_ignore_on_error(),
                    )
            },
            input,
        ))
        .or(ParsedItem::or(
            |input| {
                just!(input, Token::LBrace, "`{`")
                    .ignore_then(ParsedItem::ignore_then(|input| parse_expr(input), input))
                    .then_ignore(
                        ParsedItem::then_ignore(|input| just!(input, Token::RBrace, "`}`"), input),
                        ParsedItem::then_ignore_on_error(),
                    )
            },
            input,
        ))
}

fn parse_program<'a>(
    input: &mut VecDeque<Spanned<Token<'a>>>,
) -> ParserOutput<'a, Vec<Spanned<Expr<'a>>>> {
    let mut exprs = Vec::new();
    let mut errors = Vec::new();
    while !input.is_empty()
    {
        match parse_expr(input).into_output()
        {
            Ok(expr @ Spanned(Expr::Declaration { .. }, _)) => exprs.push(expr),
            Ok(Spanned(expr, span)) => errors.push(
                Diagnostic::error(ErrorCode::TopLevelNotDeclaration).with_main_label(
                    span,
                    format!(
                        "Expected a declaration but found a {} instead",
                        expr.kind_name()
                    ),
                ),
            ),
            Err((consumed, err)) =>
            {
                if consumed.is_empty()
                {
                    // parse_expr made no progress
                    // Pop the first Token to guarantee termination
                    let Spanned(_, span) = input.pop_front().expect("checked non-empty above");
                    errors.push(
                        Diagnostic::error(ErrorCode::UnexpectedToken).with_main_label(
                            span,
                            "This token cannot start a top-level declaration",
                        ),
                    );
                }
                else
                {
                    errors.push(err);
                }
            },
        }
    }
    if errors.is_empty() { Ok(exprs) } else { Err(errors) }
}

pub type Parser<'src> = Vec<Spanned<Expr<'src>>>;

impl<'src> Program<'src, Lexer<'src>> {
    pub fn parse(self) -> Program<'src, Parsed<'src>> {
        let mut diagnostics = Vec::new();

        let exprs = match parse_program(&mut VecDeque::from(self.state))
        {
            Ok(exprs) => exprs,
            Err(diags) =>
            {
                diagnostics.extend(diags);
                Vec::new()
            },
        };

        if self.args.parser
        {
            println!("-------------------------------------------------");
            for expr in &exprs
            {
                println!("{}", expr.0);
            }
        }

        diagnostics.report(self.source, &self.args.input);
        Program {
            args:   self.args,
            source: self.source,
            state:  exprs,
        }
    }
}
