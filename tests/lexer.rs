use fhia::lexer::Token;
use fhia::parser::expr::Ty;
use logos::Logos;

fn lex_one(input: &str) -> Result<Token<'_>, ()> {
    let mut lexer = Token::lexer(input);
    lexer.next().expect("expected at least one token")
}

fn lex_all(input: &str) -> Result<Vec<Token<'_>>, ()> {
    let lexer = Token::lexer(input);
    lexer.into_iter().collect()
}

#[test]
fn lex_let() {
    assert_eq!(lex_one("let"), Ok(Token::Let));
    assert_ne!(lex_one("lett"), Ok(Token::Let));
    assert_ne!(lex_one("llet"), Ok(Token::Let));
}

#[test]
fn lex_symbols() {
    assert_eq!(
        lex_all("=(){}:"),
        Ok(vec![
            Token::Assign,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Colon,
        ])
    )
}

#[test]
fn lex_type() {
    assert_eq!(lex_one("u8"), Ok(Token::Ty(Ty::U8)));
    assert_eq!(lex_one("u16"), Ok(Token::Ty(Ty::U16)));
    assert_eq!(lex_one("u32"), Ok(Token::Ty(Ty::U32)));
    assert_eq!(lex_one("u64"), Ok(Token::Ty(Ty::U64)));
    assert_eq!(lex_one("u128"), Ok(Token::Ty(Ty::U128)));
    assert_eq!(lex_one("i8"), Ok(Token::Ty(Ty::I8)));
    assert_eq!(lex_one("i16"), Ok(Token::Ty(Ty::I16)));
    assert_eq!(lex_one("i32"), Ok(Token::Ty(Ty::I32)));
    assert_eq!(lex_one("i64"), Ok(Token::Ty(Ty::I64)));
    assert_eq!(lex_one("i128"), Ok(Token::Ty(Ty::I128)));
    assert_eq!(lex_one("f32"), Ok(Token::Ty(Ty::F32)));
    assert_eq!(lex_one("f64"), Ok(Token::Ty(Ty::F64)));
    assert_eq!(lex_one("isize"), Ok(Token::Ty(Ty::Isize)));
    assert_eq!(lex_one("usize"), Ok(Token::Ty(Ty::Usize)));

    assert_ne!(lex_one("xi64"), Ok(Token::Ty(Ty::I64)));
    assert_ne!(lex_one("f32x"), Ok(Token::Ty(Ty::F32)));
    assert_ne!(lex_one("xf32"), Ok(Token::Ty(Ty::F32)));
}

#[test]
fn lex_i64() {
    assert_eq!(lex_one("42"), Ok(Token::I64(42)));
    assert_eq!(lex_one("-5"), Ok(Token::I64(-5)));
    assert_eq!(lex_one("0b1010"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0o12"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0xA"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0xa"), Ok(Token::I64(10)));

    assert_ne!(lex_one("3.0"), Ok(Token::I64(3)));
    assert_ne!(lex_one("3."), Ok(Token::I64(3)));
    assert_ne!(lex_one(".0"), Ok(Token::I64(0)));
}

#[test]
fn lex_f64() {
    assert_eq!(lex_one("3.14"), Ok(Token::F64(3.14)));
    assert_eq!(lex_one(".5"), Ok(Token::F64(0.5)));
    assert_eq!(lex_one("1.0e3"), Ok(Token::F64(1000.0)));
    assert_eq!(lex_one("1."), Ok(Token::F64(1.)));

    assert_ne!(lex_one("10"), Ok(Token::F64(10.0)));
}

#[test]
fn lex_ident() {
    assert_eq!(lex_one("abc"), Ok(Token::Ident("abc")));
    assert_eq!(lex_one("_x1"), Ok(Token::Ident("_x1")));

    assert_ne!(lex_one("12abc"), Ok(Token::Ident("12abc")));
    assert_ne!(lex_one("=abc"), Ok(Token::Ident("=abc")));
}
