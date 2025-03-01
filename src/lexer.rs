use std::{
    fmt::{Debug, Display},
    fs::read_to_string,
    io::{self},
    str::pattern::Pattern,
};

use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{}:{}", self.line, self.column).as_str())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),

    //Literals
    ILit(i32),      //X
    FLit(f64),      //X
    StrLit(String), //X
    CharLit(char),  //X
    BoolLit(bool),

    ConstPtr,  // *const
    MutPtr,    // *mut
    ConstAddr, // &const
    MutAddr,   // &mut

    Const,

    //Operators
    Plus,
    PlusAssign,
    Increment,
    Minus,
    Decrement,
    MinusAssign,
    Times,
    TimesAssign,
    Divide,
    DivideAssign,
    Modulo,
    ModuloAssign,
    LAnd,
    BAnd,
    AndAssign,
    LOr,
    BOr,
    OrAssign,
    LNot,
    NEqual,
    LAngle,
    LShift,
    LEq,
    RAngle,
    RShift,
    GEq,
    Equal,
    Assign,
    BNeg,
    Xor,
    XorAssign,

    // Delimiters
    LBracket,
    RBracket,
    Comma,
    RParen,
    LParen,
    LBrace,
    RBrace,
    Semicolon,
    Colon,
    Dot,

    // Ignored
    Whitespace, //X
    Comment,    //X
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

pub struct Lexer<'a> {
    path: &'a str,
    source: String,
    idx: usize,
    pos: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(path: &'a str) -> io::Result<Self> {
        Ok(Self {
            path,
            source: read_to_string(path)?,
            idx: 0,
            pos: Position { line: 1, column: 1 },
        })
    }

    fn report_error(&self, pos: Position, msg: impl Display) -> ! {
        eprintln!("[ERROR]: {}:{}: Lexing Error: {}", self.path, pos, msg);
        std::process::exit(1);
    }

    /// Consume and return the matching prefix of the given pattern
    fn consume(&mut self, pattern: impl Pattern) -> Option<&str> {
        let src = &self.source[self.idx..];
        let mut matcher = src.matches(pattern);
        match matcher.next() {
            Some(m) if m.is_prefix_of(src) => {
                self.idx += m.len();
                self.pos.line += m.matches('\n').count();
                self.pos.column = m.len() - m.rfind('\n').unwrap_or(0);
                Some(m)
            }
            _ => None,
        }
    }

    /// Consume leading whitespaces returning true if it consumed any
    ///
    /// Whitespaces includes tabs, newlines, and spaces
    #[inline]
    fn consume_whitespace(&mut self) -> bool {
        self.consume(&Regex::new(r"\s+").unwrap()).is_some()
    }

    /// Consume leading single line comment returning true if it consumed any
    #[inline]
    fn consume_line_comment(&mut self) -> bool {
        self.consume(&Regex::new(r"//[^\n]*").unwrap()).is_some()
    }

    /// Consume leading block comment returning true if it consumed any
    #[inline]
    fn consume_block_comment(&mut self) -> bool {
        self.consume(&Regex::new(r"/\*(.|\n)*?\*/").unwrap())
            .is_some()
    }

    fn escape_byte_escape_sequences(&mut self, lit: &mut String, start: Position) {
        let escape_byte_regex = Regex::new(r"\\x[0-9a-fA-F]{2}").unwrap();
        while let Some((idx, str)) = lit.match_indices(&escape_byte_regex).next() {
            let code = u8::from_str_radix(&str[2..], 16).unwrap();
            if code > 0x7F {
                self.report_error(
                    start,
                    format!("invalid byte escape sequence ({:#02X?}) in literal", code).as_str(),
                );
            }
            lit.replace_range(idx..idx + str.len(), (code as char).to_string().as_str());
        }
    }

    fn escape_unicode_escape_sequences(&mut self, lit: &mut String, start: Position) {
        let escape_unicode_regex = Regex::new(r"\\u\{[0-9a-fA-F]*\}").unwrap();
        while let Some((idx, str)) = lit.match_indices(&escape_unicode_regex).next() {
            if str.len() < 5 {
                self.report_error(start, "empty unicode escape sequence in literal");
            }
            if str.len() > 10 {
                self.report_error(start, "overlong unicode escape sequence in literal: must be at most 6 hexadecimal digits");
            }
            let unicode = u32::from_str_radix(&str[3..str.len() - 1], 16).unwrap();
            if unicode > 0x10FFFF {
                self.report_error(
                    start,
                    format!(
                        "invalid unicode escape sequence ({:#06X?}) in literal: must be at most 0x10FFFF",
                        unicode
                    )
                    .as_str(),
                );
            }
            lit.replace_range(
                idx..idx + str.len(),
                char::from_u32(unicode)
                    .unwrap_or_else(|| {
                        self.report_error(
                            start.clone(),
                            format!("invalid unicode escape sequence ({:#06X?}) in literal: Does not corresponds to a valid unicode code point", unicode)
                                .as_str(),
                        )
                    })
                    .to_string()
                    .as_str(),
            );
        }
    }

    /// Consume a string literal returning `(Span, Token::StrLit(lit))` if it consumed any
    fn consume_str_lit(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let str = self.consume(&Regex::new(r#""([^"\\\n]|\\.|\\\n|)*("|.$|\n)"#).unwrap())?;

        if !str.ends_with('\"') || "\\\"".is_suffix_of(str) {
            self.report_error(start, "unterminated string literal");
        }

        let mut lit = str[1..str.len() - 1]
            .replace("\\\n", "")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\0", "\0")
            .replace("\\n", "\n")
            .replace("\\\"", "\"")
            .replace("\\\\", "\\");

        self.escape_byte_escape_sequences(&mut lit, start.clone());
        self.escape_unicode_escape_sequences(&mut lit, start.clone());

        Some((
            Span {
                start,
                end: self.pos.clone(),
            },
            Token::StrLit(lit),
        ))
    }

    fn consume_char_lit(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let str = self.consume(&Regex::new(r#"'([^'\\]|\\.)*('|$|\n)"#).unwrap())?;

        if !str.ends_with('\'') || "\\'".is_suffix_of(str) {
            self.report_error(start, "unterminated char literal");
        }

        let mut lit = str[1..str.len() - 1]
            .replace("\\'", "'")
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\0", "\0");

        self.escape_byte_escape_sequences(&mut lit, start.clone());
        self.escape_unicode_escape_sequences(&mut lit, start.clone());

        if lit.is_empty() {
            self.report_error(start, "empty char literal");
        }
        if lit.chars().count() > 1 {
            self.report_error(start, "overlong char literal");
        }

        Some((
            Span {
                start,
                end: self.pos.clone(),
            },
            Token::CharLit(lit.chars().next().unwrap()),
        ))
    }

    fn consume_f_lit(&mut self) -> Option<(Span, Token)> {
        Some((
            Span {
                start: self.pos.clone(),
                end: self.pos.clone(),
            },
            Token::FLit(
                self.consume(&Regex::new(r"([0-9]+\.[0-9]*)|(\.[0-9]+)").unwrap())?
                    .parse::<f64>()
                    .unwrap_or_else(|e| {
                        self.report_error(self.pos.clone(), format!("invalid float literal {}", e));
                    }),
            ),
        ))
    }

    fn consume_int_lit(&mut self) -> Option<(Span, Token)> {
        let span = Span {
            start: self.pos.clone(),
            end: self.pos.clone(),
        };
        let lit = self.consume(&Regex::new(r"(0[obx][0-9a-fA-F]+)|[0-9]+").unwrap())?;
        Some((
            span,
            Token::ILit(match &lit[0..2] {
                "0b" => i32::from_str_radix(&lit[2..], 2).unwrap_or_else(|e| {
                    self.report_error(
                        self.pos.clone(),
                        format!("invalid binary integer literal {}", e),
                    )
                }),
                "0o" => i32::from_str_radix(&lit[2..], 8).unwrap_or_else(|e| {
                    self.report_error(
                        self.pos.clone(),
                        format!("invalid octal integer literal {}", e),
                    )
                }),
                "0x" => i32::from_str_radix(&lit[2..], 16).unwrap_or_else(|e| {
                    self.report_error(
                        self.pos.clone(),
                        format!("invalid hexadecimal integer literal {}", e),
                    )
                }),
                _ => lit.parse::<i32>().unwrap_or_else(|e| {
                    self.report_error(self.pos.clone(), format!("invalid integer literal {}", e))
                }),
            }),
        ))
    }
}

impl Iterator for Lexer<'_> {
    type Item = (Span, Token);

    fn next(&mut self) -> Option<Self::Item> {
        // Reached end of source
        if self.idx >= self.source.len() {
            return None;
        }

        // Skip whitespaces and comments
        if self.consume_whitespace() || self.consume_line_comment() || self.consume_block_comment()
        {
            return self.next();
        }

        Some(
            self.consume_str_lit()
                .or(self.consume_char_lit())
                .or(self.consume_f_lit())
                .or(self.consume_int_lit())
                .unwrap_or_else(|| {
                    self.report_error(
                        self.pos.clone(),
                        format!(
                            "unknown token: {}",
                            &self.source[self.idx..].split_whitespace().next().unwrap()
                        )
                        .as_str(),
                    )
                }),
        )
    }
}

#[cfg(test)]
mod test {
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
        let mut lexer = Lexer::new("tests/correct/string_lit.fhia").unwrap();

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::StrLit("".to_string()));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::StrLit("TEST".to_string()));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(
            token.unwrap().1,
            Token::StrLit("\n\t\r\0\x10\u{00ffFF}\"".to_string())
        );

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(
            token.unwrap().1,
            Token::StrLit("multiline string".to_string())
        );

        assert_eq!(lexer.next(), None);
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
        let mut lexer = Lexer::new("tests/correct/char_lit.fhia").unwrap();

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('T'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\n'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\t'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\r'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\0'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\x10'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\u{00ffFF}'));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::CharLit('\''));

        assert_eq!(lexer.next(), None);
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
        let mut lexer = Lexer::new("tests/correct/flit.fhia").unwrap();

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::FLit(69.42));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::FLit(00000.42));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::FLit(00069.42));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::FLit(0.42));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::FLit(69.));

        assert_eq!(lexer.next(), None);
    }

    #[test]
    #[allow(clippy::mixed_case_hex_literals)]
    fn test_int_literal() {
        let mut lexer = Lexer::new("tests/correct/ilit.fhia").unwrap();

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(6942));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(6942));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(0o467));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(0b101));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(0xdead));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(0xDEAD));

        let token = lexer.next();
        assert!(token.is_some());
        assert_eq!(token.unwrap().1, Token::ILit(0xDeAd));
    }
}
