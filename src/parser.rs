pub mod expr;
use std::{iter::Peekable, range::Range};

use crate::{
    Spanned,
    lexer::Token,
    parser::expr::{DeclKind, Expr, Ty, UnaryOpKind, operators::OpKind},
    program::{
        Lexer,
        Parsed,
        Program,
        diagnostics::{Diagnostic, ErrorCode, Reportable},
    },
};

macro_rules! expect {
    {let $pattern:pat = $input:expr, $span:expr} => {
        let tok = match $input
        {
            #[allow(unused_variables)]
            Some(tok @ $pattern) => tok,
            Some(Spanned(tok, span)) =>
            {
                return Err(Diagnostic::error(ErrorCode::UnexpectedToken)
                    .with_main_label(span, format!("Unexpected token `{tok}`")))
            },
            None =>
            {
                return Err(Diagnostic::error(ErrorCode::UnexpectedEof)
                    .with_main_label(Range::from($span.end..$span.end), "Unexpected end of input"))
            },
        };
        let $pattern = tok
        else
        {
            unsafe { core::hint::unreachable_unchecked() }
        };
    };
    {let $pattern:pat = $input:expr, $span:expr, $custom_err:expr, $custom_expected_msg:expr} => {
        let tok = match $input
        {
            #[allow(unused_variables)]
            Some(tok @ $pattern) => tok,
            Some(Spanned(tok, span)) =>
            {
                return Err(Diagnostic::error($custom_err)
                    .with_main_label(span, format!("{} but found `{tok}`", $custom_expected_msg)))
            },
            None =>
            {
                return Err(Diagnostic::error($custom_err)
                    .with_main_label(Range::from($span.end..$span.end), format!("Unexpected end of input. {}", $custom_expected_msg)))
            },
        };
        let $pattern = tok
        else
        {
            unsafe { core::hint::unreachable_unchecked() }
        };
    };
}

fn parse_decla<'src>(
    mut kind: DeclKind,
    span: Range<usize>,
    input: &mut Peekable<impl Iterator<Item = Spanned<Token<'src>>>>,
) -> Result<Spanned<Expr<'src>>, Diagnostic> {
    if matches!(input.peek(), Some(Spanned(Token::Mut, _)))
    {
        input.next();
        kind = DeclKind::Let { is_mut: true };
    }
    expect!(let Spanned(Token::Ident(name), _) = input.next(), span, ErrorCode::DeclarationMalformed, "Expected an identifier");
    expect!(let Spanned(Token::Colon, _) = input.next(), span, ErrorCode::DeclarationMalformed, "Expected `:`");
    expect!(let Spanned(Token::Ty(ty), _) = input.next(), span, ErrorCode::DeclarationMalformed, "Expected a type");
    expect!(let Spanned(Token::Assign, assign_span) = input.next(), span, ErrorCode::DeclarationMalformed, "Expected `=`");
    let expr = parse_expr(None, input, Range::from(span.start..assign_span.end))?;
    let expr_span = expr.1;
    Ok(Spanned(
        Expr::Declaration {
            kind,
            name,
            ty,
            expr: Box::new(expr),
        },
        Range::from(span.start..expr_span.end),
    ))
}

fn parse_surrounded_expr<'src>(
    end: &Token<'src>,
    tokens: &mut Peekable<impl Iterator<Item = Spanned<Token<'src>>>>,
    span: Range<usize>,
) -> Result<Spanned<Expr<'src>>, Diagnostic> {
    let Spanned(expr, expr_span) = parse_expr(None, tokens, span)?;
    let end_span = match end
    {
        Token::RParen =>
        {
            expect!(let Spanned(Token::RParen, end_span) = tokens.next(), Range::from(expr_span.end-1..expr_span.end), ErrorCode::UnclosedDelimiter, "Expected `)`");
            end_span
        },
        Token::RBrace =>
        {
            expect!(let Spanned(Token::RBrace, end_span) = tokens.next(), Range::from(expr_span.end-1..expr_span.end), ErrorCode::UnclosedDelimiter, "Expected `}`");
            end_span
        },
        _ => unreachable!("Ensured by caller"),
    };
    Ok(Spanned(
        Expr::Atom(Box::new(Spanned(expr, expr_span))),
        Range::from(span.start..end_span.end),
    ))
}

fn parse_op<'src>(
    prev_expr: Option<Spanned<Expr<'src>>>,
    op: Token,
    tokens: &mut Peekable<impl Iterator<Item = Spanned<Token<'src>>>>,
    span: Range<usize>,
) -> Result<Spanned<Expr<'src>>, Diagnostic> {
    match op
    {
        Token::Minus if prev_expr.is_none() =>
        {
            let Spanned(operand, operand_span) = parse_expr(None, tokens, span)?;
            Ok(match operand
            {
                Expr::Unary {
                    kind,
                    operand: inner_operand,
                } if UnaryOpKind::Neg.binding_force().1 > kind.binding_force().0 =>
                {
                    let Spanned(inner_operand, inner_operand_span) = *inner_operand;
                    Spanned(
                        kind.to_expr_with_operand(Box::new(Spanned(
                            Expr::Unary {
                                operand: Box::new(Spanned(inner_operand, inner_operand_span)),
                                kind:    UnaryOpKind::Neg,
                            },
                            Range::from(span.start..inner_operand_span.end),
                        ))),
                        Range::from(span.start..operand_span.end),
                    )
                },
                _ => Spanned(
                    Expr::Unary {
                        kind:    expr::UnaryOpKind::Neg,
                        operand: Box::new(Spanned(operand, operand_span)),
                    },
                    Range::from(span.start..operand_span.end),
                ),
            })
        },
        Token::Minus => todo!("Binary minus"),
        Token::As if prev_expr.is_none() => Err(Diagnostic::error(ErrorCode::DeclarationMalformed)
            .with_main_label(span, "Expected an expression, but found `as`")),
        Token::As =>
        {
            expect!(let Spanned(Token::Ty(ty), ty_span) = tokens.next(), span, ErrorCode::DeclarationMalformed, "Expected a type after `as`");
            let operand = Box::new(unsafe { prev_expr.unwrap_unchecked() });
            Ok(Spanned(
                Expr::Unary {
                    kind: UnaryOpKind::As(ty),
                    operand,
                },
                Range::from(span.start..ty_span.end),
            ))
        },
        _ => unreachable!("Ensured by caller"),
    }
}

fn parse_expr<'src>(
    prev_expr: Option<Spanned<Expr<'src>>>,
    tokens: &mut Peekable<impl Iterator<Item = Spanned<Token<'src>>>>,
    span: Range<usize>,
) -> Result<Spanned<Expr<'src>>, Diagnostic> {
    let Spanned(first_tok, span) = tokens.next().ok_or_else(|| {
        Diagnostic::error(ErrorCode::UnexpectedEof).with_main_label(
            span,
            "Expected an expression but reached the end of the input",
        )
    })?;

    let expr = match first_tok
    {
        Token::Let => parse_decla(DeclKind::Let { is_mut: false }, span, tokens)?,
        Token::Const => parse_decla(DeclKind::Const, span, tokens)?,
        Token::Ident(name) => Spanned(
            Expr::Ident {
                name,
                ty: Ty::Unknown,
            },
            span,
        ),
        Token::ILit(lit) => Spanned(
            Expr::IntLit {
                ty:      Ty::IntLit,
                value:   lit,
                negated: false,
            },
            span,
        ),
        Token::FLit(lit) => Spanned(Expr::F64(lit), span),
        Token::LParen => parse_surrounded_expr(&Token::RParen, tokens, span)?,
        Token::LBrace => parse_surrounded_expr(&Token::RBrace, tokens, span)?,
        Token::As | Token::Minus => parse_op(prev_expr, first_tok, tokens, span)?,
        tok @ (Token::Mut
        | Token::Ty(_)
        | Token::Assign
        | Token::RParen
        | Token::RBrace
        | Token::Colon
        | Token::Error) =>
        {
            return Err(Diagnostic::error(ErrorCode::DeclarationMalformed)
                .with_main_label(span, format!("Expected an expression but found {tok}")))?;
        },
    };

    if let Some(Spanned(peeked, peeked_span)) = tokens.peek()
    {
        let span = *peeked_span;
        match *peeked
        {
            Token::Ty(_)
            | Token::ILit(_)
            | Token::FLit(_)
            | Token::Ident(_)
            | Token::Let
            | Token::Const
            | Token::Mut
            | Token::Assign
            | Token::LParen
            | Token::RParen
            | Token::LBrace
            | Token::RBrace
            | Token::Colon
            | Token::Error => (),
            Token::As | Token::Minus => return parse_expr(Some(expr), tokens, span),
        }
    }
    Ok(expr)
}

fn parse_top_level<'src>(
    tokens: &mut Peekable<impl Iterator<Item = Spanned<Token<'src>>>>,
) -> Result<Spanned<Expr<'src>>, Diagnostic> {
    const fn starts_top_level_decl(tok: &Spanned<Token<'_>>) -> bool {
        matches!(tok, Spanned(Token::Let | Token::Const, _))
    }

    match tokens.next().expect("Ensured by caller")
    {
        Spanned(Token::Let, span) => parse_decla(DeclKind::Let { is_mut: false }, span, tokens),
        Spanned(Token::Const, span) => parse_decla(DeclKind::Const, span, tokens),
        Spanned(tok, span) => Err(Diagnostic::error(ErrorCode::DeclarationMalformed)
            .with_main_label(span, format!("Unexpected token `{tok}` at top level"))),
    }
    .inspect_err(|_| {
        tokens
            .by_ref()
            .take_while(|tok| !starts_top_level_decl(tok))
            .for_each(drop);
    })
}

pub type Parser<'src> = Vec<Spanned<Expr<'src>>>;

impl<'src> Program<'src, Lexer<'src>> {
    pub fn parse(self) -> Program<'src, Parsed<'src>> {
        let mut peekable = self.state.into_iter().peekable();
        let mut diagnostics = Vec::new();
        let mut exprs = Vec::new();

        core::iter::from_fn(|| {
            peekable
                .peek()
                .is_some()
                .then(|| parse_top_level(&mut peekable))
        })
        .for_each(|res| match res
        {
            Ok(expr) => exprs.push(expr),
            Err(diag) => diagnostics.push(diag),
        });

        let exprs: Vec<Spanned<Expr<'_>>> =
            exprs.into_iter().map(super::Spanned::flatten).collect();

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
            args:           self.args,
            target_machine: self.target_machine,
            source:         self.source,
            state:          exprs,
        }
    }
}

impl Spanned<Expr<'_>> {
    fn flatten(self) -> Self {
        let Spanned(expr, span) = self;
        Spanned(
            match expr
            {
                Expr::Atom(spanned) => return spanned.clone().flatten(),
                Expr::Declaration {
                    kind,
                    name,
                    ty,
                    expr,
                } => Expr::Declaration {
                    kind,
                    name,
                    ty,
                    expr: Box::new(expr.flatten()),
                },
                Expr::Unary { kind, operand } => Expr::Unary {
                    kind,
                    operand: Box::new(operand.flatten()),
                },
                Expr::IntLit { .. } | Expr::F64(_) | Expr::Ident { .. } => expr,
            },
            span,
        )
    }
}
