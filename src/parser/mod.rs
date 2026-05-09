pub mod expr;

use core::panic;
use std::collections::VecDeque;

use expr::Expr;
use lexparse::{Parser, ParserItem};
use logos::Logos;

use crate::{
    ParsingError,
    Spanned,
    diagnostics::{self, Diagnostic, ErrorCode},
    lexer::Token,
    parser::expr::Ty,
    typer::TypedOutput,
};

type ParserOutput<'a, T> = Result<T, Vec<ParsingError>>;

#[derive(Debug, Clone)]
enum ParsedItem<'a> {
    Expr(VecDeque<Spanned<Token<'a>>>, Spanned<Expr<'a>>),
    Token(VecDeque<Spanned<Token<'a>>>, Spanned<Token<'a>>),
    Seq(Box<Self>, Box<Self>),
    Err(VecDeque<Spanned<Token<'a>>>, Diagnostic),
}

impl<'a> ParsedItem<'a> {
    fn or(
        parser: impl FnOnce(&mut VecDeque<Spanned<Token<'a>>>) -> Self,
        input: &mut VecDeque<Spanned<Token<'a>>>,
    ) -> impl FnOnce(Self) -> Self {
        |prev| {
            let mut item = parser(input);
            item.consumed_mut().extend(prev.consumed());
            item
        }
    }

    fn token_lit_as_expr(self) -> Option<Self> {
        match self
        {
            Self::Token(mut consumed, Spanned(Token::I64(i), span)) =>
            {
                consumed.push_back(Spanned(Token::I64(i), span.clone()));
                Some(Self::Expr(consumed, Spanned(Expr::I64(i), span)))
            },
            Self::Token(mut consumed, Spanned(Token::F64(f), span)) =>
            {
                consumed.push_back(Spanned(Token::F64(f), span.clone()));
                Some(Self::Expr(consumed, Spanned(Expr::F64(f), span)))
            },
            _ => None,
        }
    }

    fn consumed(self) -> VecDeque<Spanned<Token<'a>>> {
        match self
        {
            Self::Seq(item1, item2) =>
            {
                assert!(item1.consumed().is_empty());
                item2.consumed()
            },
            Self::Expr(old_c, _) | Self::Token(old_c, _) | Self::Err(old_c, _) => old_c,
        }
    }

    fn consumed_mut(&mut self) -> &mut VecDeque<Spanned<Token<'a>>> {
        match self
        {
            Self::Seq(item1, item2) =>
            {
                assert!(item1.consumed_mut().is_empty());
                item2.consumed_mut()
            },
            Self::Expr(old_c, _) | Self::Token(old_c, _) | Self::Err(old_c, _) => old_c,
        }
    }

    fn extract_err(self) -> Option<(VecDeque<Spanned<Token<'a>>>, Diagnostic)> {
        match self
        {
            Self::Err(c, d) => Some((c, d)),
            Self::Seq(a, b) => b.extract_err().or_else(|| a.extract_err()),
            _ => None,
        }
    }

    fn push_back_input(self, input: &mut VecDeque<Spanned<Token<'a>>>) {
        match self
        {
            Self::Expr(consumed, _) | Self::Token(consumed, _) | Self::Err(consumed, _) =>
            {
                for tok in consumed.into_iter().rev()
                {
                    input.push_front(tok);
                }
            },
            Self::Seq(_, parsed_item1) => parsed_item1.push_back_input(input),
        }
    }

    fn ignore_then(
        parser: impl FnOnce(&mut VecDeque<Spanned<Token<'a>>>) -> Self,
        input: &mut VecDeque<Spanned<Token<'a>>>,
    ) -> impl FnOnce(Self) -> Self {
        |prev| {
            let mut item = parser(input);
            let consumed = item.consumed_mut();
            let mut prev_consumed = prev.consumed();
            while let Some(tok) = prev_consumed.pop_back()
            {
                consumed.push_front(tok);
            }
            item
        }
    }

    fn then_ignore(
        parser: impl FnOnce(&mut VecDeque<Spanned<Token<'a>>>) -> Self,
        input: &mut VecDeque<Spanned<Token<'a>>>,
    ) -> impl FnOnce(&mut Self) -> Self {
        |prev| {
            let mut item = parser(input);
            let consumed = std::mem::take(item.consumed_mut());
            prev.consumed_mut().extend(consumed);
            item
        }
    }

    fn then_ignore_on_error() -> impl FnOnce(Self, &mut Self) {
        |mut prev, item| {
            let consumed = prev.consumed_mut();
            while let Some(tok) = consumed.pop_back()
            {
                item.consumed_mut().push_front(tok);
            }
        }
    }
}

impl<'a> ParserItem for ParsedItem<'a> {
    type Error = (VecDeque<Spanned<Token<'a>>>, Diagnostic);
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
            s => match s.extract_err()
            {
                Some(err) => Err(err),
                None => panic!("Cannot call into_output on a non-error token or sequence"),
            },
        }
    }
}

impl Parser for ParsedItem<'_> {
    type Item = Self;

    fn from_item(item: Self::Item) -> Self { item }

    // TODO: Refactor that
    fn make_sequence(mut self, mut other: Self::Item) -> Self {
        assert!(
            !matches!(other, Self::Seq(..)),
            "`other` should never be a Sequence"
        );
        match &mut self
        {
            Self::Expr(consumed, _) | Self::Token(consumed, _) | Self::Err(consumed, _) =>
            {
                match &mut other
                {
                    Self::Expr(other_consumed, _)
                    | Self::Token(other_consumed, _)
                    | Self::Err(other_consumed, _) =>
                    {
                        while let Some(tok) = consumed.pop_back()
                        {
                            other_consumed.push_front(tok);
                        }
                    },
                    Self::Seq(..) => unreachable!(),
                }
            },
            Self::Seq(_, parsed_item1) => match parsed_item1.as_mut()
            {
                Self::Expr(consumed, _) | Self::Token(consumed, _) | Self::Err(consumed, _) =>
                {
                    match &mut other
                    {
                        Self::Expr(other_consumed, _)
                        | Self::Token(other_consumed, _)
                        | Self::Err(other_consumed, _) =>
                        {
                            while let Some(tok) = consumed.pop_back()
                            {
                                other_consumed.push_front(tok);
                            }
                        },
                        Self::Seq(..) => unreachable!(),
                    }
                },
                Self::Seq(..) => unreachable!(),
            },
        }
        Self::Seq(Box::new(self), Box::new(other))
    }
}

macro_rules! just {
    ($input:expr, $tok_pat:pat, $expected:literal, $code:expr) => {
        match $input.pop_front()
        {
            Some(tok @ Spanned($tok_pat, _)) =>
            {
                ParsedItem::Token(VecDeque::from(vec![tok.clone()]), tok)
            },
            Some(Spanned(tok, span)) =>
            {
                $input.push_front(Spanned(tok.clone(), span.clone()));
                ParsedItem::Err(
                    VecDeque::from(vec![]),
                    Diagnostic::error($code).with_main_label(
                        span,
                        format!("Expected {} but found {} instead", $expected, tok),
                    ),
                )
            },
            None => ParsedItem::Err(
                VecDeque::from(vec![]),
                Diagnostic::error($code).with_main_label(0..0, "Unexpected end of input"),
            ),
        }
    };
    ($input:expr, $tok_pat:pat, $expected:literal) => {
        just!($input, $tok_pat, $expected, ErrorCode::UnexpectedToken)
    };
}

macro_rules! assume {
    // Base: single pattern = expression
    (let $pat:pat = $expr:expr) => {
        let $pat = $expr else { unreachable!() };
    };

    // Entry point: reverse the pattern list first
    (let Seq[$($pats:pat),+ $(,)?] = $expr:expr) => {
        assume!(@seq_rev $expr; []; $($pats),+)
    };

    // Reverse accumulator: empty acc
    (@seq_rev $expr:expr; []; $head:pat, $($tail:pat),+) => {
        assume!(@seq_rev $expr; [$head]; $($tail),+)
    };

    // Reverse accumulator: non-empty acc
    (@seq_rev $expr:expr; [$($acc:pat),+]; $head:pat, $($tail:pat),+) => {
        assume!(@seq_rev $expr; [$head, $($acc),+]; $($tail),+)
    };

    // Reverse done: hand off to muncher
    (@seq_rev $expr:expr; [$($reversed:pat),*]; $last:pat) => {
        assume!(@seq $expr; $last, $($reversed),*)
    };

    // Muncher: two patterns left — base case
    (@seq $expr:expr; $pat1:pat, $pat2:pat) => {
        assume!(let ParsedItem::Seq(__a, __b) = $expr);
        assume!(let $pat1 = *__b);
        assume!(let $pat2 = *__a);
    };

    // Muncher: more than two — peel one from the RIGHT is impossible,
    // so peel from the LEFT instead
    (@seq $expr:expr; $pat1:pat, $($rest:pat),+) => {
        assume!(let ParsedItem::Seq(__a, __b) = $expr);
        assume!(let $pat1 = *__b);
        assume!(@seq *__a; $($rest),+)
    };
}

fn parse_litteral<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    just!(input, Token::I64(_), "a litteral")
        .or(|prev| {
            prev.push_back_input(input);
            just!(input, Token::F64(_), "a litteral")
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

fn parse_decla<'a>(input: &mut VecDeque<Spanned<Token<'a>>>) -> ParsedItem<'a> {
    let result = just!(input, Token::Let, "`let`", ErrorCode::DeclarationMalformed)
        .ignore_then(ParsedItem::ignore_then(parse_ident, input))
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
                ParsedItem::Expr(mut consumed, Spanned(Expr::Ident { name, .. }, ident_span)),
                ParsedItem::Token(ty_consumed, Spanned(Token::Ty(ty), _)),
                ParsedItem::Expr(expr_consumed, Spanned(expr, expr_span)),
            ] = item);
            let span = ident_span.start..expr_span.end;
            consumed.extend(ty_consumed);
            consumed.extend(expr_consumed);
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
    // Commit to a declaration as soon as `let` is seen — no backtracking to
    // other alternatives if the declaration is malformed.
    if matches!(input.front(), Some(Spanned(Token::Let, _)))
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
                    // parse_expr made no progress — this token cannot start any expression.
                    // Pop it to guarantee termination; report a precise error for it.
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
