pub mod expr;
#[macro_use]
pub mod combinator;

use std::collections::VecDeque;

use combinator::ParsedItem;
use expr::{DeclKind, Expr};
use lexparse::{Parser, ParserItem};
use logos::Logos;

use crate::{
    ParsingError,
    Spanned,
    diagnostics::{self, Diagnostic, ErrorCode, Reportable},
    lexer::Token,
    parser::expr::Ty,
    typer::TypedOutput,
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

#[derive(Debug)]
pub struct ParseOutput<'a> {
    pub exprs:       Vec<Spanned<Expr<'a>>>,
    pub diagnostics: Vec<diagnostics::Diagnostic>,
}

impl<'a> ParseOutput<'a> {
    #[must_use]
    pub fn type_check(self) -> TypedOutput<'a> { TypedOutput::type_check(self.exprs) }
}

impl Reportable for ParseOutput<'_> {
    fn diagnostics(&self) -> &[Diagnostic] { self.diagnostics.as_slice() }
}

/// # Panics
/// - Panics if there is an error while opening or writing to the parser.svg file
pub fn parse(source: &str) -> ParseOutput<'_> {
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
                diagnostics::Diagnostic::error(ErrorCode::InvalidToken)
                    .with_main_label(span, "Invalid token"),
            );
        }
        else
        {
            tokens.push(Spanned(tok, span));
        }
    }

    if !diagnostics.is_empty()
    {
        return ParseOutput {
            exprs: Vec::new(),
            diagnostics,
        };
    }

    match parse_program(&mut VecDeque::from(tokens))
    {
        Ok(exprs) => ParseOutput {
            exprs,
            diagnostics: Vec::new(),
        },
        Err(diagnostics) => ParseOutput {
            exprs: Vec::new(),
            diagnostics,
        },
    }
}
