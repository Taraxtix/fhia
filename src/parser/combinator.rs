use std::collections::VecDeque;

use lexparse::{Parser, ParserItem};

use crate::{
    Spanned,
    lexer::Token,
    parser::expr::{Expr, Ty},
    program::diagnostics::Diagnostic,
};

#[derive(Debug, Clone)]
pub(super) enum ParsedItem<'a> {
    Expr(VecDeque<Spanned<Token<'a>>>, Spanned<Expr<'a>>),
    Token(VecDeque<Spanned<Token<'a>>>, Spanned<Token<'a>>),
    Opt(VecDeque<Spanned<Token<'a>>>, Option<Box<Self>>),
    Seq(Box<Self>, Box<Self>),
    Err(VecDeque<Spanned<Token<'a>>>, Diagnostic),
}

impl<'a> ParsedItem<'a> {
    pub(super) fn or(
        parser: impl FnOnce(&mut VecDeque<Spanned<Token<'a>>>) -> Self,
        input: &mut VecDeque<Spanned<Token<'a>>>,
    ) -> impl FnOnce(Self) -> Self {
        |prev| {
            let mut item = parser(input);
            item.consumed_mut().extend(prev.consumed());
            item
        }
    }

    pub(super) fn token_lit_as_expr(self) -> Option<Self> {
        match self
        {
            Self::Token(mut consumed, Spanned(Token::ILit(n), span)) =>
            {
                consumed.push_back(Spanned(Token::ILit(n), span));
                Some(Self::Expr(
                    consumed,
                    Spanned(
                        Expr::IntLit {
                            ty:    Ty::IntLit,
                            value: n,
                        },
                        span,
                    ),
                ))
            },
            Self::Token(mut consumed, Spanned(Token::FLit(f), span)) =>
            {
                consumed.push_back(Spanned(Token::FLit(f), span));
                Some(Self::Expr(consumed, Spanned(Expr::F64(f), span)))
            },
            _ => None,
        }
    }

    fn fold_rightmost<T, F>(self, f: F) -> T
    where
        F: FnOnce(Self) -> T,
    {
        match self
        {
            Self::Seq(_, item2) => item2.fold_rightmost(f),
            leaf => f(leaf),
        }
    }

    fn consumed(self) -> VecDeque<Spanned<Token<'a>>> {
        self.fold_rightmost(|item| match item
        {
            Self::Expr(c, _) | Self::Token(c, _) | Self::Err(c, _) | Self::Opt(c, _) => c,
            Self::Seq(..) => unreachable!(),
        })
    }

    fn consumed_mut(&mut self) -> &mut VecDeque<Spanned<Token<'a>>> {
        match self
        {
            Self::Seq(item1, item2) =>
            {
                assert!(item1.consumed_mut().is_empty());
                item2.consumed_mut()
            },
            Self::Expr(old_c, _)
            | Self::Token(old_c, _)
            | Self::Err(old_c, _)
            | Self::Opt(old_c, _) => old_c,
        }
    }

    fn extract_err(self) -> Option<(VecDeque<Spanned<Token<'a>>>, Diagnostic)> {
        match self
        {
            Self::Err(c, d) => Some((c, d)),
            Self::Seq(a, b) => b.extract_err().or_else(|| a.extract_err()),
            Self::Opt(_, inner) => inner.and_then(|i| i.extract_err()),
            _ => None,
        }
    }

    pub(super) fn push_back_input(self, input: &mut VecDeque<Spanned<Token<'a>>>) {
        self.fold_rightmost(|item| match item
        {
            Self::Expr(consumed, _)
            | Self::Token(consumed, _)
            | Self::Err(consumed, _)
            | Self::Opt(consumed, _) =>
            {
                for tok in consumed.into_iter().rev()
                {
                    input.push_front(tok);
                }
            },
            Self::Seq(..) => unreachable!(),
        });
    }

    pub(super) fn ignore_then(
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

    pub(super) fn then_ignore(
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

    pub(super) fn then_ignore_on_error() -> impl FnOnce(Self, &mut Self) {
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
            Self::Expr(..) | Self::Token(..) | Self::Opt(..) => false,
            Self::Seq(item1, item2) => item1.is_err() || item2.is_err(),
            Self::Err(..) => true,
        }
    }

    fn into_output(self) -> Result<Self::Output, Self::Error> {
        match self
        {
            Self::Expr(_, expr) => Ok(expr),
            Self::Err(consumed, err) => Err((consumed, err)),
            s => s.extract_err().map_or_else(
                || panic!("Cannot call into_output on a non-error token or sequence"),
                Err,
            ),
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
            Self::Expr(consumed, _)
            | Self::Token(consumed, _)
            | Self::Err(consumed, _)
            | Self::Opt(consumed, _) => match &mut other
            {
                Self::Expr(other_consumed, _)
                | Self::Token(other_consumed, _)
                | Self::Err(other_consumed, _)
                | Self::Opt(other_consumed, _) =>
                {
                    while let Some(tok) = consumed.pop_back()
                    {
                        other_consumed.push_front(tok);
                    }
                },
                Self::Seq(..) => unreachable!(),
            },
            Self::Seq(_, parsed_item1) => match parsed_item1.as_mut()
            {
                Self::Expr(consumed, _)
                | Self::Token(consumed, _)
                | Self::Err(consumed, _)
                | Self::Opt(consumed, _) => match &mut other
                {
                    Self::Expr(other_consumed, _)
                    | Self::Token(other_consumed, _)
                    | Self::Err(other_consumed, _)
                    | Self::Opt(other_consumed, _) =>
                    {
                        while let Some(tok) = consumed.pop_back()
                        {
                            other_consumed.push_front(tok);
                        }
                    },
                    Self::Seq(..) => unreachable!(),
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
                $input.push_front(Spanned(tok.clone(), span));
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
                Diagnostic::error($code).with_main_label(
                    std::range::Range::<usize>::from(0..0),
                    "Unexpected end of input",
                ),
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
