use logos::Logos;

use crate::lexer::Token;
use crate::parser::expr::Ty;
use crate::tests::int_ty;

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
    assert_eq!(lex_one("i8"), Ok(Token::Ty(int_ty(true, 8))));
    assert_eq!(lex_one("i16"), Ok(Token::Ty(int_ty(true, 16))));
    assert_eq!(lex_one("i32"), Ok(Token::Ty(int_ty(true, 32))));
    assert_eq!(lex_one("i64"), Ok(Token::Ty(int_ty(true, 64))));
    assert_eq!(lex_one("i128"), Ok(Token::Ty(int_ty(true, 128))));
    assert_eq!(lex_one("u8"), Ok(Token::Ty(int_ty(false, 8))));
    assert_eq!(lex_one("u16"), Ok(Token::Ty(int_ty(false, 16))));
    assert_eq!(lex_one("u32"), Ok(Token::Ty(int_ty(false, 32))));
    assert_eq!(lex_one("u64"), Ok(Token::Ty(int_ty(false, 64))));
    assert_eq!(lex_one("u128"), Ok(Token::Ty(int_ty(false, 128))));
    assert_eq!(lex_one("f32"), Ok(Token::Ty(Ty::F32)));
    assert_eq!(lex_one("f64"), Ok(Token::Ty(Ty::F64)));
    assert_eq!(lex_one("isize"), Ok(Token::Ty(Ty::Isize)));
    assert_eq!(lex_one("usize"), Ok(Token::Ty(Ty::Usize)));

    // arbitrary widths
    assert_eq!(lex_one("u1"), Ok(Token::Ty(int_ty(false, 1))));
    assert_eq!(lex_one("i3"), Ok(Token::Ty(int_ty(true, 3))));
    assert_eq!(lex_one("u43"), Ok(Token::Ty(int_ty(false, 43))));

    // type token must not be consumed as part of an identifier
    assert_ne!(lex_one("xi64"), Ok(Token::Ty(int_ty(true, 64))));
    assert_ne!(lex_one("f32x"), Ok(Token::Ty(Ty::F32)));
}

#[test]
fn lex_type_width_too_large() {
    assert_eq!(lex_one("u129"), Err(()));
    assert_eq!(lex_one("i129"), Err(()));
    assert_eq!(lex_one("u999"), Err(()));
}

// =============================================================================
// Integer literals
// =============================================================================

#[test]
fn lex_integer_decimal() {
    assert_eq!(lex_one("42"), Ok(Token::ILit(42)));
    assert_eq!(lex_one("0"), Ok(Token::ILit(0)));
    // leading zeros are stripped
    assert_eq!(lex_one("007"), Ok(Token::ILit(7)));
    assert_eq!(lex_one("00"), Ok(Token::ILit(0)));
}

#[test]
fn lex_integer_binary() {
    assert_eq!(lex_one("0b1010"), Ok(Token::ILit(10)));
    assert_eq!(lex_one("0b0"), Ok(Token::ILit(0)));
    assert_eq!(lex_one("0b11111111"), Ok(Token::ILit(255)));
}

#[test]
fn lex_integer_octal() {
    assert_eq!(lex_one("0o12"), Ok(Token::ILit(10)));
    assert_eq!(lex_one("0o0"), Ok(Token::ILit(0)));
    assert_eq!(lex_one("0o377"), Ok(Token::ILit(255)));
}

#[test]
fn lex_integer_hex() {
    assert_eq!(lex_one("0xA"), Ok(Token::ILit(10)));
    assert_eq!(lex_one("0xa"), Ok(Token::ILit(10)));
    assert_eq!(lex_one("0xFF"), Ok(Token::ILit(255)));
    assert_eq!(lex_one("0x0"), Ok(Token::ILit(0)));
}

// TODO: Bring back this test once `UnaryMinus` is on
// #[test]
// fn lex_integer_negative() {
//     assert_eq!(lex_one("-5"), Ok(Token::ILit(-5)));
//     assert_eq!(lex_one("-0"), Ok(Token::ILit(0)));
//     assert_eq!(lex_one("-0b1010"), Ok(Token::ILit(-10)));
//     assert_eq!(lex_one("-0o12"), Ok(Token::ILit(-10)));
//     assert_eq!(lex_one("-0xFF"), Ok(Token::ILit(-255)));
// }

#[test]
fn lex_integer_not_float() {
    assert_ne!(lex_one("3.0"), Ok(Token::ILit(3)));
    assert_ne!(lex_one("3."), Ok(Token::ILit(3)));
    assert_ne!(lex_one(".0"), Ok(Token::ILit(0)));
}

// =============================================================================
// Float literals
// =============================================================================

#[allow(clippy::approx_constant)]
#[test]
fn lex_float_decimal() {
    assert_eq!(lex_one("1."), Ok(Token::FLit(1.0)));
    assert_eq!(lex_one(".5"), Ok(Token::FLit(0.5)));
    assert_eq!(lex_one("3.14"), Ok(Token::FLit(3.14)));
    assert_eq!(lex_one("0.0"), Ok(Token::FLit(0.0)));
}

#[test]
fn lex_float_scientific() {
    assert_eq!(lex_one("1.0e3"), Ok(Token::FLit(1000.0)));
    assert_eq!(lex_one("1.e3"), Ok(Token::FLit(1000.0)));
    assert_eq!(lex_one("1.0E3"), Ok(Token::FLit(1000.0)));
    assert_eq!(lex_one("1.0e-3"), Ok(Token::FLit(0.001)));
}

// TODO: Bring back this test once `UnaryMinus` is on
// #[test]
// fn lex_float_negative() {
//     assert_eq!(lex_one("-1.5"), Ok(Token::FLit(-1.5)));
//     assert_eq!(lex_one("-0.0"), Ok(Token::FLit(-0.0)));
// }

#[test]
fn lex_float_not_integer() {
    assert_ne!(lex_one("10"), Ok(Token::FLit(10.0)));
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
fn lex_comment() {
    assert_eq!(
        lex_all("// this is a comment\n42"),
        Ok(vec![Token::ILit(42)])
    );
    assert_eq!(
        lex_all("let // comment\nx"),
        Ok(vec![Token::Let, Token::Ident("x")])
    );
    assert_eq!(lex_all("// only a comment"), Ok(vec![]));
}

#[test]
fn lex_block_comment() {
    // simple block comment is skipped
    assert_eq!(lex_all("/* this is a comment */"), Ok(vec![]));
    // empty block comment
    assert_eq!(lex_all("/**/"), Ok(vec![]));
    // block comment between tokens
    assert_eq!(
        lex_all("let /* comment */ x"),
        Ok(vec![Token::Let, Token::Ident("x")])
    );
    // multi-line block comment
    assert_eq!(
        lex_all("let /* line one\nline two */ x"),
        Ok(vec![Token::Let, Token::Ident("x")])
    );
    // block comment containing a line comment marker
    assert_eq!(
        lex_all("let /* // not a line comment */ x"),
        Ok(vec![Token::Let, Token::Ident("x")])
    );
    // line comment containing `/*` — must NOT start a block comment
    assert_eq!(
        lex_all("// /* this is still a line comment\n42"),
        Ok(vec![Token::ILit(42)])
    );
    // only a block comment
    assert_eq!(lex_all("/* only a comment */"), Ok(vec![]));
}

#[test]
fn lex_invalid_token() {
    assert_eq!(lex_one("@"), Err(()));
    assert_eq!(lex_one("#"), Err(()));
    assert_eq!(lex_one("$"), Err(()));
    assert_eq!(lex_one("?"), Err(()));
    assert_eq!(lex_one("u129"), Err(()));
    assert_eq!(lex_one("u200"), Err(()));
    assert_eq!(lex_one("i129"), Err(()));
    // a single invalid char contaminates the whole lex_all result
    assert_eq!(lex_all("let @"), Err(()));
}
