pub mod expr;

use core::panic;
use std::collections::VecDeque;

use expr::Expr;
use lexparse::{Parser, ParserItem};
use logos::Logos;

use crate::{
    ParsingError,
    Spanned,
    diagnostics::{self, Diagnostic},
    lexer::Token,
    parser::expr::Ty,
    typer::TypedOutput,
};

type ParserOutput<'a, T> = Result<T, Vec<ParsingError>>;

#[derive(Clone)]
enum ParsedItem<'a> {
    Expr(Vec<Spanned<Token<'a>>>, Spanned<Expr<'a>>),
    Token(Vec<Spanned<Token<'a>>>, Spanned<Token<'a>>),
    Seq(Box<Self>, Box<Self>),
    Err(Vec<Spanned<Token<'a>>>, Diagnostic),
}

impl<'a> ParsedItem<'a> {
    fn token_lit_as_expr(self) -> Option<Self> {
        match self
        {
            Self::Token(mut consumed, Spanned(Token::I64(i), span)) =>
            {
                consumed.push(Spanned(Token::I64(i), span.clone()));
                Some(Self::Expr(consumed, Spanned(Expr::I64(i), span)))
            },
            Self::Token(mut consumed, Spanned(Token::F64(f), span)) =>
            {
                consumed.push(Spanned(Token::F64(f), span.clone()));
                Some(Self::Expr(consumed, Spanned(Expr::F64(f), span)))
            },
            _ => None,
        }
    }

    fn consumed(self) -> Vec<Spanned<Token<'a>>> {
        match self
        {
            Self::Expr(old_c, _) => old_c,
            Self::Token(old_c, _) => old_c,
            Self::Seq(item1, item2) =>
            {
                let mut consumed = item1.consumed();
                consumed.extend(item2.consumed().into_iter());
                consumed
            },
            Self::Err(old_c, _) => old_c,
        }
    }

    fn get_seq_root_mut(&mut self) -> &mut Self {
        match self
        {
            this @ (Self::Expr(..) | Self::Token(..) | Self::Err(..)) => this,
            Self::Seq(parsed_item, _) => parsed_item.get_seq_root_mut(),
        }
    }
}

impl<'a> ParserItem for ParsedItem<'a> {
    type Error = (Vec<Spanned<Token<'a>>>, Diagnostic);
    type Output = Spanned<Expr<'a>>;

    fn is_err(&self) -> bool {
        match self
        {
            Self::Expr(..) | Self::Token(..) => false,
            Self::Seq(item1, item2) => item1.is_err() || item2.is_err(),
            Self::Err(..) => true,
        }
    }

    fn into_output(self) -> Result<Self::Output, Self::Error> {
        match self
        {
            Self::Expr(_, expr) => Ok(expr),
            Self::Err(consumed, err) => Err((consumed, err)),
            _ => panic!("Cannot call into_output on a token or a sequence"),
        }
    }

    fn replace(self, other: Self) -> Self {
        let mut old_consumed = self.consumed();
        match other
        {
            Self::Expr(consumed, spanned) =>
            {
                old_consumed.extend(consumed.into_iter());
                Self::Expr(old_consumed, spanned)
            },
            Self::Token(consumed, spanned) =>
            {
                old_consumed.extend(consumed.into_iter());
                Self::Token(old_consumed, spanned)
            },
            Self::Seq(mut parsed_item, parsed_item1) =>
            {
                let root = parsed_item.get_seq_root_mut();
                match root
                {
                    Self::Expr(consumed, spanned) =>
                    {
                        old_consumed.extend(consumed.clone().into_iter());
                        *root = Self::Expr(old_consumed, spanned.clone());
                    },
                    Self::Token(consumed, spanned) =>
                    {
                        old_consumed.extend(consumed.clone().into_iter());
                        *root = Self::Token(old_consumed, spanned.clone());
                    },
                    Self::Err(consumed, diagnostic) =>
                    {
                        old_consumed.extend(consumed.clone().into_iter());
                        *root = Self::Err(old_consumed, diagnostic.clone());
                    },
                    Self::Seq(..) => unreachable!(),
                };
                Self::Seq(parsed_item, parsed_item1)
            },
            Self::Err(consumed, diagnostic) =>
            {
                old_consumed.extend(consumed);
                Self::Err(old_consumed, diagnostic)
            },
        }
    }
}

impl Parser for ParsedItem<'_> {
    type Item = Self;

    fn from_item(item: Self::Item) -> Self { item }

    fn make_sequence(self, other: Self::Item) -> Self {
        assert!(
            !matches!(other, Self::Seq(..)),
            "`other` should never be a Sequence"
        );
        Self::Seq(Box::new(self), Box::new(other))
    }
}

macro_rules! just {
    ($input:expr, $tok_pat:pat, $expected:literal) => {
        match $input.pop_front()
        {
            Some(tok @ Spanned($tok_pat, _)) => ParsedItem::Token(vec![tok.clone()], tok),
            Some(Spanned(tok, span)) =>
            {
                $input.push_front(Spanned(tok.clone(), span.clone()));
                ParsedItem::Err(
                    vec![],
                    Diagnostic::error(format!("Expected {}", $expected)).with_main_label(
                        span,
                        format!("Expected {} but found {} instead", $expected, tok),
                    ),
                )
            },
            None => ParsedItem::Err(
                vec![],
                Diagnostic::error("Unexpected end of input")
                    .with_main_label(0..0, "Unexpected end of input"),
            ),
        }
    };
}

macro_rules! assume {
    (let $pat:pat = $expr:expr) => {
        let $pat = $expr
        else
        {
            unreachable!()
        };
    };
    (let Seq[$pat1:pat, $pat2:pat $(,)?] = $expr:expr) => {
        assume!(let ParsedItem::Seq(__temp1, __temp2) = $expr);
        assume!(let $pat1 = *__temp1);
        assume!(let $pat2 = *__temp2);
    };

    (let SeqPrev[$pat:pat] = $expr:expr) => {
        assume!(let ParsedItem::Seq(__temp1, __temp2) = $expr);
        assume!(let $pat = *__temp2);
    };

    (let SeqPrev[$pat:pat, $($rest:pat),+] = $expr:expr) => {
        // This tranmuted value is never used (We use an invalid value (42) to help catch bug where it will be used somehow)
        let __temp1: Box<ParsedItem<'_>> = unsafe { core::mem::transmute([42u8;8]) };
        assume!(let SeqPrev[$($rest),+] = $expr);
        assume!(let ParsedItem::Seq(__temp1, __temp2) = *__temp1);
        assume!(let $pat = *__temp2);
    };

    (let Seq[$pat1:pat, $pat2:pat, $($rest:pat),+ $(,)?] = $expr:expr) => {
        assume!(let SeqPrev[$pat1, $pat2, $($rest),+] = $expr);
    };
}

fn parse_litteral<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::I64(_), "a litteral")
        .or(|| just!(input, Token::F64(_), "a litteral"))
        .map(|item| {
            item.token_lit_as_expr()
                .expect("just! should ensure that the token is a litteral")
        })
}

fn parse_ident<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::Ident(_), " an identifier").map(|tok| {
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
            let range = ty_span.start..expr_span.clone().end;
            consumed.extend(expr_consumed.into_iter());
            ParsedItem::Expr(
                consumed,
                Spanned(Expr::Cast(ty, Box::new(Spanned(expr, expr_span))), range),
            )
        })
}

fn parse_decla<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::Let, "Expected `let`")
        .ignore_then(|| parse_ident(input))
        .then_ignore(|| just!(input, Token::Colon, "Expected `:`"))
        .then(|| just!(input, Token::Ty(_), "Expected a type"))
        .then_ignore(|| just!(input, Token::Assign, "Expected `=`"))
        .then(|| parse_expr(input))
        .map(|item| {
            assume!(let Seq[
                ParsedItem::Expr(mut consumed, Spanned(Expr::Ident { name, .. }, ident_span)),
                ParsedItem::Token(ty_consumed, Spanned(Token::Ty(ty), _)),
                ParsedItem::Expr(expr_consumed, Spanned(expr, expr_span)),
            ] = item);
            let span = ident_span.start..expr_span.end;
            consumed.extend(ty_consumed.into_iter());
            consumed.extend(expr_consumed.into_iter());
            ParsedItem::Expr(
                consumed,
                Spanned(
                    Expr::Declaration {
                        name,
                        ty,
                        expr: Box::new(Spanned(expr, expr_span)),
                    },
                    span,
                ),
            )
        })
}

fn parse_expr<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    parse_decla(input)
        .or(|| parse_litteral(input))
        .or(|| parse_cast(input))
        .or(|| {
            just!(input, Token::LParen, "Expected `(`")
                .ignore_then(|| parse_expr(input))
                .then_ignore(|| just!(input, Token::RParen, "Expected `)`"))
        })
        .or(|| {
            just!(input, Token::LBrace, "Expected `(`")
                .ignore_then(|| parse_expr(input))
                .then_ignore(|| just!(input, Token::RBrace, "Expected `)`"))
        })
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
                Diagnostic::error(format!(
                    "Expected a declaration but found a {} instead",
                    expr.kind_name()
                ))
                .with_main_label(span, "Top level expressions must be declarations")
                .with_code(1),
            ),
            Err((_, err)) => errors.push(err),
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
                diagnostics::Diagnostic::error("Invalid token")
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
