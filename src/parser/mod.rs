use chumsky::prelude::*;

mod expr;
use expr::*;

enum SyntaxError {
    MissingLet,
    Other,
}

type Output<'src> = Vec<Expr<'src>>;
fn parser<'a, Err>() -> impl Parser<'a, &'a str, Output<'a>, extra::Err<Rich<'a, char>>> {
    let keyword = choice(vec![text::keyword("let")]);

    let ident = text::ident().and_is(keyword.not());

    let literal = text::int(10).map(|str_lit: &str| Expr::I64(str_lit.parse().unwrap()));

    let ty = choice(vec![
        just("i8"),
        just("i16"),
        just("i32"),
        just("i64"),
        just("i128"),
        just("u8"),
        just("u16"),
        just("u32"),
        just("u64"),
        just("u128"),
        just("f32"),
        just("f64"),
    ])
    .map(|ty| match ty {
        "i8" => Ty::I8,
        "i16" => Ty::I16,
        "i32" => Ty::I32,
        "i64" => Ty::I64,
        "i128" => Ty::I128,
        "u8" => Ty::U8,
        "u16" => Ty::U16,
        "u32" => Ty::U32,
        "u64" => Ty::U64,
        "u128" => Ty::U128,
        "f32" => Ty::F32,
        "f64" => Ty::F64,
        _ => unreachable!(),
    });

    let mut expr = Recursive::declare();

    let decla = just("let")
        .then_ignore(text::whitespace())
        .ignore_then(ident)
        .then_ignore(text::whitespace())
        .then_ignore(just('='))
        .then_ignore(text::whitespace())
        .then(expr.clone())
        .then_ignore(text::whitespace())
        .map(|(name, expr): (&str, _)| Expr::Declaration {
            name,
            ty: Ty::Unknown,
            expr: Box::new(expr),
        });

    let cast = ty
        .then_ignore(text::whitespace())
        .then(expr.clone())
        .then_ignore(text::whitespace())
        .map(|(ty, expr)| Expr::Cast(ty, Box::new(expr)));

    expr.define(decla.clone().or(literal).or(cast));

    decla.repeated().collect()
}

pub fn parse<'src>(input: &'src str) -> ParseResult<Vec<Expr<'src>>, Rich<'src, char>> {
    parser::<extra::Err<char>>().parse(input)
}
