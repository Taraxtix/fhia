use chumsky::prelude::*;

mod expr;
use expr::*;

macro_rules! ParserOf {
    ($T:ty) => {
   impl Parser<'a, &'a str, $T, extra::Err<Rich<'a, char>>> + Clone
    };
}

enum Keyword {
    Let,
}

impl Keyword {
    fn parser<'a>(&self) -> ParserOf!(&'a str) {
        match self {
            Self::Let => text::keyword("let"),
        }
        .padded_by(text::whitespace())
    }

    fn any<'a>() -> ParserOf!(&'a str) {
        choice(vec![Self::Let.parser()])
    }
}

impl Ty {
    fn parse<'a>() -> ParserOf!(Self) {
        choice(vec![
            text::keyword("i8"),
            text::keyword("i16"),
            text::keyword("i32"),
            text::keyword("i64"),
            text::keyword("i128"),
            text::keyword("u8"),
            text::keyword("u16"),
            text::keyword("u32"),
            text::keyword("u64"),
            text::keyword("u128"),
            text::keyword("f32"),
            text::keyword("f64"),
        ])
        .map(|ty_str| Ty::try_from(ty_str).expect("Invalid type"))
        .padded_by(text::whitespace())
    }
}

fn parser<'a>() -> ParserOf!(Vec<Expr<'a>>) {
    let ident = text::ident()
        .and_is(Keyword::any().not())
        .padded_by(text::whitespace());

    let literal = text::int(10)
        .map(|str_lit: &str| Expr::I64(str_lit.parse().unwrap()))
        .padded_by(text::whitespace());

    let mut expr = Recursive::declare();

    let decla = Keyword::Let
        .parser()
        .ignore_then(ident)
        .then_ignore(just('=').padded_by(text::whitespace()))
        .then(expr.clone())
        .map(|(name, expr): (&str, _)| Expr::Declaration {
            name,
            ty: Ty::Unknown,
            expr: Box::new(expr),
        })
        .padded_by(text::whitespace());

    let cast = Ty::parse()
        .then(expr.clone())
        .map(|(ty, expr)| Expr::Cast(ty, Box::new(expr)))
        .padded_by(text::whitespace());

    expr.define(decla.clone().or(literal).or(cast));

    decla.repeated().collect()
}

pub fn parse<'src>(input: &'src str) -> ParseResult<Vec<Expr<'src>>, Rich<'src, char>> {
    parser().parse(input)
}
