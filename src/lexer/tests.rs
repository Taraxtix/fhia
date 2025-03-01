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
        capture_stderr(["tests/incorrect/unknown_token.fhia"]),
        "[ERROR]: tests/incorrect/unknown_token.fhia:1:1: Lexing Error: unknown token: \\unknown_token\n"
    )
}

#[test]
fn test_whitespaces() {
    //Content of the file: "\r\t\n "
    let mut lexer = Lexer::new("tests/correct/whitespaces.fhia").unwrap();
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_comments() {
    let mut lexer = Lexer::new("tests/correct/comments.fhia").unwrap();
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

    let actual = Lexer::new("tests/correct/string_lit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(actual, expected);
}

#[test]
fn test_unterminated_string_literal() {
    let capture = capture_stderr(["tests/incorrect/unterminated_string_lit.fhia"]);
    assert_eq!(
        capture,
        "[ERROR]: tests/incorrect/unterminated_string_lit.fhia:1:1: Lexing Error: unterminated string literal\n"
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

    let actual = Lexer::new("tests/correct/char_lit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_unterminated_char_literal() {
    let capture = capture_stderr(["tests/incorrect/unterminated_char_lit.fhia"]);
    assert_eq!(
        capture,
        "[ERROR]: tests/incorrect/unterminated_char_lit.fhia:1:1: Lexing Error: unterminated char literal\n"
    )
}

#[test]
fn test_overlong_char_literal() {
    let capture = capture_stderr(["tests/incorrect/overlong_char_lit.fhia"]);
    assert_eq!(
        capture,
        "[ERROR]: tests/incorrect/overlong_char_lit.fhia:1:1: Lexing Error: overlong char literal\n"
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

    let actual = Lexer::new("tests/correct/flit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
#[allow(clippy::mixed_case_hex_literals)]
fn test_int_literal() {
    let expected = vec![
        Token::ILit(6942),
        Token::ILit(6942),
        Token::ILit(0o467),
        Token::ILit(0b101),
        Token::ILit(0xdead),
        Token::ILit(0xDEAD),
        Token::ILit(0xdEaD),
    ];

    let actual = Lexer::new("tests/correct/ilit.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}

#[test]
fn test_ptr_ops() {
    let expected = vec![
        Token::MutDeref,
        Token::MutRef,
        Token::ConstDeref,
        Token::ConstRef,
    ];

    let actual = Lexer::new("tests/correct/ptr_ops.fhia")
        .unwrap()
        .map(|t| t.1)
        .collect::<Vec<_>>();

    assert_eq!(expected, actual);
}
