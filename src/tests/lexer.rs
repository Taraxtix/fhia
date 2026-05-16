use logos::Logos;

use crate::lexer::Token;
use crate::parser::expr::Ty;

fn lex_one(input: &str) -> Result<Token<'_>, ()> {
    let mut lexer = Token::lexer(input);
    lexer.next().expect("expected at least one token")
}

fn lex_all(input: &str) -> Result<Vec<Token<'_>>, ()> { Token::lexer(input).collect() }

// =============================================================================
// Keyword
// =============================================================================

#[test]
fn lex_keyword() {
    assert_eq!(lex_one("let"), Ok(Token::Let));
    assert_eq!(lex_one("mut"), Ok(Token::Mut));
    assert_eq!(lex_one("const"), Ok(Token::Const));

    // must be an exact word — prefix/suffix disqualify it
    assert_ne!(lex_one("lett"), Ok(Token::Let));
    assert_ne!(lex_one("llet"), Ok(Token::Let));
}

// =============================================================================
// Symbols
// =============================================================================

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
    );
}

// =============================================================================
// Types
// =============================================================================

#[test]
fn lex_types() {
    assert_eq!(lex_one("i8"), Ok(Token::Ty(Ty::I8)));
    assert_eq!(lex_one("i16"), Ok(Token::Ty(Ty::I16)));
    assert_eq!(lex_one("i32"), Ok(Token::Ty(Ty::I32)));
    assert_eq!(lex_one("i64"), Ok(Token::Ty(Ty::I64)));
    assert_eq!(lex_one("i128"), Ok(Token::Ty(Ty::I128)));
    assert_eq!(lex_one("u8"), Ok(Token::Ty(Ty::U8)));
    assert_eq!(lex_one("u16"), Ok(Token::Ty(Ty::U16)));
    assert_eq!(lex_one("u32"), Ok(Token::Ty(Ty::U32)));
    assert_eq!(lex_one("u64"), Ok(Token::Ty(Ty::U64)));
    assert_eq!(lex_one("u128"), Ok(Token::Ty(Ty::U128)));
    assert_eq!(lex_one("f32"), Ok(Token::Ty(Ty::F32)));
    assert_eq!(lex_one("f64"), Ok(Token::Ty(Ty::F64)));
    assert_eq!(lex_one("isize"), Ok(Token::Ty(Ty::Isize)));
    assert_eq!(lex_one("usize"), Ok(Token::Ty(Ty::Usize)));

    // type token must not be consumed as part of an identifier
    assert_ne!(lex_one("xi64"), Ok(Token::Ty(Ty::I64)));
    assert_ne!(lex_one("f32x"), Ok(Token::Ty(Ty::F32)));
}

// =============================================================================
// Integer literals
// =============================================================================

#[test]
fn lex_integer_decimal() {
    assert_eq!(lex_one("42"), Ok(Token::I64(42)));
    assert_eq!(lex_one("0"), Ok(Token::I64(0)));
    // leading zeros are stripped
    assert_eq!(lex_one("007"), Ok(Token::I64(7)));
    assert_eq!(lex_one("00"), Ok(Token::I64(0)));
}

#[test]
fn lex_integer_binary() {
    assert_eq!(lex_one("0b1010"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0b0"), Ok(Token::I64(0)));
    assert_eq!(lex_one("0b11111111"), Ok(Token::I64(255)));
}

#[test]
fn lex_integer_octal() {
    assert_eq!(lex_one("0o12"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0o0"), Ok(Token::I64(0)));
    assert_eq!(lex_one("0o377"), Ok(Token::I64(255)));
}

#[test]
fn lex_integer_hex() {
    assert_eq!(lex_one("0xA"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0xa"), Ok(Token::I64(10)));
    assert_eq!(lex_one("0xFF"), Ok(Token::I64(255)));
    assert_eq!(lex_one("0x0"), Ok(Token::I64(0)));
}

#[test]
fn lex_integer_negative() {
    assert_eq!(lex_one("-5"), Ok(Token::I64(-5)));
    assert_eq!(lex_one("-0"), Ok(Token::I64(0)));
    assert_eq!(lex_one("-0b1010"), Ok(Token::I64(-10)));
    assert_eq!(lex_one("-0o12"), Ok(Token::I64(-10)));
    assert_eq!(lex_one("-0xFF"), Ok(Token::I64(-255)));
}

#[test]
fn lex_integer_not_float() {
    assert_ne!(lex_one("3.0"), Ok(Token::I64(3)));
    assert_ne!(lex_one("3."), Ok(Token::I64(3)));
    assert_ne!(lex_one(".0"), Ok(Token::I64(0)));
}

// =============================================================================
// Float literals
// =============================================================================

#[allow(clippy::approx_constant)]
#[test]
fn lex_float_decimal() {
    assert_eq!(lex_one("1."), Ok(Token::F64(1.0)));
    assert_eq!(lex_one(".5"), Ok(Token::F64(0.5)));
    assert_eq!(lex_one("3.14"), Ok(Token::F64(3.14)));
    assert_eq!(lex_one("0.0"), Ok(Token::F64(0.0)));
}

#[test]
fn lex_float_scientific() {
    assert_eq!(lex_one("1.0e3"), Ok(Token::F64(1000.0)));
    assert_eq!(lex_one("1.e3"), Ok(Token::F64(1000.0)));
    assert_eq!(lex_one("1.0E3"), Ok(Token::F64(1000.0)));
    assert_eq!(lex_one("1.0e-3"), Ok(Token::F64(0.001)));
}

#[test]
fn lex_float_negative() {
    assert_eq!(lex_one("-1.5"), Ok(Token::F64(-1.5)));
    assert_eq!(lex_one("-0.0"), Ok(Token::F64(-0.0)));
}

#[test]
fn lex_float_not_integer() {
    assert_ne!(lex_one("10"), Ok(Token::F64(10.0)));
}

// =============================================================================
// Identifiers
// =============================================================================

#[test]
fn lex_ident() {
    assert_eq!(lex_one("abc"), Ok(Token::Ident("abc")));
    assert_eq!(lex_one("_x1"), Ok(Token::Ident("_x1")));
    assert_eq!(lex_one("_"), Ok(Token::Ident("_")));
    assert_eq!(lex_one("camelCase"), Ok(Token::Ident("camelCase")));
    assert_eq!(
        lex_one("with_underscore"),
        Ok(Token::Ident("with_underscore"))
    );

    // must not start with a digit or symbol
    assert_ne!(lex_one("1abc"), Ok(Token::Ident("1abc")));
    assert_ne!(lex_one("=abc"), Ok(Token::Ident("=abc")));
}

// =============================================================================
// Invalid tokens
// =============================================================================

#[test]
fn lex_invalid_token() {
    assert_eq!(lex_one("@"), Err(()));
    assert_eq!(lex_one("#"), Err(()));
    assert_eq!(lex_one("$"), Err(()));
    assert_eq!(lex_one("?"), Err(()));
    // a single invalid char contaminates the whole lex_all result
    assert_eq!(lex_all("let @"), Err(()));
}
