use std::process::{Command, Output, Stdio};

use std::ffi::OsStr;

use super::*;

fn capture_output(args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> Output {
    Command::new("cargo")
        .arg("build")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to launch `cargo build`")
        .wait()
        .expect("Failed to wait on `cargo build`");

    Command::new("target/debug/fhia")
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to launch the compiler with requested arguments")
        .wait_with_output()
        .expect("Failed to wait on the compiler with requested arguments")
}

fn _capture_stdout(args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> String {
    capture_output(args)
        .stdout
        .iter()
        .map(|c| *c as char)
        .collect::<String>()
}

fn capture_stderr(args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> String {
    capture_output(args)
        .stderr
        .iter()
        .map(|c| *c as char)
        .collect::<String>()
}

#[test]
fn test_unknown_token() {
    assert_eq!(
        capture_stderr(["tests/lexer/incorrect/unknown_token.fhia"]),
        "[ERROR]: tests/lexer/incorrect/unknown_token.fhia:1:1: Lexing Error: unknown token: \\unknown_token\n"
    )
}

#[test]
fn test_whitespaces() {
    //Content of the file: "\r\t\n "
    let mut lexer = Lexer::new("tests/lexer/correct/whitespaces.fhia").unwrap();
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_comments() {
    let mut lexer = Lexer::new("tests/lexer/correct/comments.fhia").unwrap();
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal() {
    let expected = vec![
        Token::StrLit("".to_string()),
        Token::StrLit("TEST".to_string()),
        Token::StrLit("\n\t\r\0\x10\u{00ffFF}\"".to_string()),
        Token::StrLit("multiline string".to_string()),
    ];

    let actual = Lexer::new("tests/lexer/correct/string_lit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(actual, expected);
}

#[test]
fn test_unterminated_string_literal() {
    let capture = capture_stderr(["tests/lexer/incorrect/unterminated_string_lit.fhia"]);
    assert_eq!(
        capture,
        "[ERROR]: tests/lexer/incorrect/unterminated_string_lit.fhia:1:1: Lexing Error: unterminated string literal\n"
    )
}

#[test]
fn test_char_literal() {
    let expected = vec![
        Token::CharLit('T'),
        Token::CharLit('\n'),
        Token::CharLit('\t'),
        Token::CharLit('\r'),
        Token::CharLit('\0'),
        Token::CharLit('\x10'),
        Token::CharLit('\u{00ffFF}'),
        Token::CharLit('\''),
    ];

    let actual = Lexer::new("tests/lexer/correct/char_lit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_unterminated_char_literal() {
    let capture = capture_stderr(["tests/lexer/incorrect/unterminated_char_lit.fhia"]);
    assert_eq!(
        capture,
        "[ERROR]: tests/lexer/incorrect/unterminated_char_lit.fhia:1:1: Lexing Error: unterminated char literal\n"
    )
}

#[test]
fn test_overlong_char_literal() {
    let capture = capture_stderr(["tests/lexer/incorrect/overlong_char_lit.fhia"]);
    assert_eq!(
        capture,
        "[ERROR]: tests/lexer/incorrect/overlong_char_lit.fhia:1:1: Lexing Error: overlong char literal\n"
    )
}

#[test]
fn test_float_literal() {
    let expected = vec![
        Token::FLit(69.42),
        Token::FLit(00000.42),
        Token::FLit(00069.42),
        Token::FLit(0.42),
        Token::FLit(69.),
    ];

    let actual = Lexer::new("tests/lexer/correct/flit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
#[allow(clippy::mixed_case_hex_literals)]
fn test_int_literal() {
    let expected = vec![
        Token::U32Lit(6942),
        Token::U32Lit(6942),
        Token::U32Lit(0o467),
        Token::U32Lit(0b101),
        Token::U32Lit(0xdead),
        Token::U32Lit(0xDEAD),
        Token::U32Lit(0xdEaD),
    ];

    let actual = Lexer::new("tests/lexer/correct/ilit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_types() {
    let expected = vec![
        Token::I8,
        Token::I16,
        Token::I32,
        Token::I64,
        Token::I128,
        Token::U8,
        Token::U16,
        Token::U32,
        Token::U64,
        Token::U128,
        Token::Size,
        Token::F32,
        Token::F64,
        Token::F128,
        Token::Bool,
        Token::Char,
        Token::Str,
        Token::Unit,
        Token::Wildcard,
        Token::Bang,
    ];

    let actual = Lexer::new("tests/lexer/correct/types.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_ops() {
    let expected = vec![
        Token::PlusAssign,
        Token::Increment,
        Token::Plus,
        Token::MinusAssign,
        Token::Decrement,
        Token::Minus,
        Token::TimesAssign,
        Token::Power,
        Token::Times,
        Token::DivideAssign,
        Token::Divide,
        Token::ModuloAssign,
        Token::Modulo,
        Token::LAnd,
        Token::AndAssign,
        Token::BAnd,
        Token::LOr,
        Token::OrAssign,
        Token::BOr,
        Token::Bang,
        Token::NEqual,
        Token::LAngle,
        Token::LShiftAssign,
        Token::LShift,
        Token::LEq,
        Token::RAngle,
        Token::RShiftAssign,
        Token::RShift,
        Token::GEq,
        Token::Equal,
        Token::Assign,
        Token::XorAssign,
        Token::Xor,
        Token::BNeg,
    ];

    let actual = Lexer::new("tests/lexer/correct/ops.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_delim() {
    let expected = vec![
        Token::LBracket,
        Token::RBracket,
        Token::LParen,
        Token::RParen,
        Token::LBrace,
        Token::RBrace,
        Token::Semicolon,
        Token::Comma,
        Token::Dot,
        Token::Colon,
    ];

    let actual = Lexer::new("tests/lexer/correct/delim.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_bool_literal() {
    let expected = vec![Token::BoolLit(true), Token::BoolLit(false)];

    let actual = Lexer::new("tests/lexer/correct/bool_lit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_keywords() {
    let expected = vec![
        Token::Mut,
        Token::Let,
        Token::If,
        Token::Else,
        Token::While,
        Token::For,
        Token::In,
    ];

    let actual = Lexer::new("tests/lexer/correct/keywords.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_ident() {
    let expected = vec![
        Token::Ident("ident".to_string()),
        Token::Ident("dbg".to_string()),
        Token::Ident("long_ident".to_string()),
        Token::Ident("ident-with-dash".to_string()),
        Token::Ident("two".to_string()),
        Token::Ident("idents".to_string()),
        Token::Ident("two".to_string()),
        Token::Plus,
        Token::Ident("idents".to_string()),
    ];

    let actual = Lexer::new("tests/lexer/correct/ident.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}
