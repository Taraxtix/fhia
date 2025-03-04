#[cfg(test)]
mod tests;

use std::{
    fmt::{Debug, Display},
    fs::read_to_string,
    io::{self},
    num::IntErrorKind,
    str::pattern::Pattern,
};

use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
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

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    #[cfg(test)]
    pub fn new_raw(start: (usize, usize), end: (usize, usize)) -> Self {
        Span {
            start: Position {
                line: start.0,
                column: start.1,
            },
            end: Position {
                line: end.0,
                column: end.1,
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),

    //Literals
    U32Lit(u32),
    U64Lit(u64),
    U128Lit(u128),
    FLit(f64),
    StrLit(String),
    CharLit(char),
    BoolLit(bool),

    //Keywords
    Mut,
    Let,
    If,
    Else,
    While,
    For,
    In,

    //Operators
    Plus,
    PlusAssign,
    Increment,
    Minus,
    Decrement,
    MinusAssign,
    ConstDeref, // *const
    MutDeref,   // *mut
    Times,
    TimesAssign,
    Power,
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
    Bang,
    NEqual,
    LAngle,
    LShift,
    LShiftAssign,
    LEq,
    RAngle,
    RShift,
    RShiftAssign,
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

    // Types
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    Size,
    F32,
    F64,
    F128,
    Bool,
    Char,
    Str,
    Unit,
    ConstRef, // &const
    MutRef,   // &mut
    Array { ty: Box<Token>, size: Box<Token> },
    Wildcard,
}

impl Token {
    fn is_type(&self) -> bool {
        matches!(
            self,
            Token::I8
                | Token::I16
                | Token::I32
                | Token::I64
                | Token::I128
                | Token::U8
                | Token::U16
                | Token::U32
                | Token::U64
                | Token::U128
                | Token::Size
                | Token::F32
                | Token::F64
                | Token::F128
                | Token::Bool
                | Token::Char
                | Token::Str
                | Token::Unit
                | Token::ConstRef
                | Token::MutRef
                | Token::Array { .. }
                | Token::Bang
                | Token::Wildcard
        )
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

pub struct Lexer<'a> {
    pub path: &'a str,
    source: String,
    idx: usize,
    pub pos: Position,
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
                let num_nl = m.matches('\n').count();
                self.pos.line += num_nl;
                if num_nl > 0 {
                    self.pos.column = m.len() - m.rfind('\n').unwrap_or(0);
                } else {
                    self.pos.column += m.len()
                }
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

        Some((Span::new(start, self.pos.clone()), Token::StrLit(lit)))
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
            Span::new(start, self.pos.clone()),
            Token::CharLit(lit.chars().next().unwrap()),
        ))
    }

    fn consume_f_lit(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = Token::FLit(
            self.consume(&Regex::new(r"([0-9]+\.[0-9]*)|(\.[0-9]+)").unwrap())?
                .parse::<f64>()
                .unwrap_or_else(|e| {
                    self.report_error(self.pos.clone(), format!("invalid float literal {}", e));
                }),
        );
        Some((Span::new(start, self.pos.clone()), tok))
    }

    fn ulit_from_string(&mut self, s: &str, radix: u32) -> Token {
        let error = |e: std::num::ParseIntError| {
            let radix_string = match radix {
                2 => "binary",
                8 => "octal",
                10 => "",
                16 => "hexadecimal",
                _ => unreachable!(),
            };
            self.report_error(
                self.pos.clone(),
                format!("invalid {radix_string} integer literal {}", e),
            )
        };

        match u32::from_str_radix(s, radix) {
            Ok(n) => Token::U32Lit(n),
            Err(e) if e.kind() == &IntErrorKind::PosOverflow => {
                match u64::from_str_radix(s, radix) {
                    Ok(n) => Token::U64Lit(n),
                    Err(e) if e.kind() == &IntErrorKind::PosOverflow => {
                        match u128::from_str_radix(s, radix) {
                            Ok(n) => Token::U128Lit(n),
                            Err(e) => error(e),
                        }
                    }
                    Err(e) => error(e),
                }
            }
            Err(e) => error(e),
        }
    }

    fn consume_int_lit(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let lit = self
            .consume(&Regex::new(r"(0[obx][0-9a-fA-F]+)|[0-9]+").unwrap())?
            .to_string();

        Some((
            Span::new(start, self.pos.clone()),
            if lit.len() >= 2 {
                match &lit[0..2] {
                    "0b" => self.ulit_from_string(&lit[2..], 2),
                    "0o" => self.ulit_from_string(&lit[2..], 8),
                    "0x" => self.ulit_from_string(&lit[2..], 16),
                    _ => self.ulit_from_string(&lit, 10),
                }
            } else {
                self.ulit_from_string(&lit, 10)
            },
        ))
    }

    fn consume_type(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = match self.consume(&Regex::new(r"\[.+;[^]]+\]|[iuf](32|64|128)|[iu](8|16)|size|bool|char|string|\(\)|&(const|mut)|_").unwrap())? {
                "i8" => Token::I8,
                "i16" => Token::I16,
                "i32" => Token::I32,
                "i64" => Token::I64,
                "i128" => Token::I128,
                "u8" => Token::U8,
                "u16" => Token::U16,
                "u32" => Token::U32,
                "u64" => Token::U64,
                "u128" => Token::U128,
                "size" => Token::Size,
                "f32" => Token::F32,
                "f64" => Token::F64,
                "f128" => Token::F128,
                "bool" => Token::Bool,
                "char" => Token::Char,
                "string" => Token::Str,
                "()" => Token::Unit,
                "&const" => Token::ConstRef,
                "&mut" => Token::MutRef,
                "_" => Token::Wildcard,
                array_str if array_str.starts_with('[') => {
                    let array_str = &array_str[1..array_str.len() - 1];
                    let (ty, size) = array_str.rsplit_once(';').map(|c| (c.0.to_string(), c.1.to_string())).unwrap();

                    let (mut ty_lexer, mut size_lexer) = (
                        Self {
                        pos: self.pos.clone(),
                        idx: 0,
                        path: self.path,
                        source: ty,
                    },Self {
                        pos: self.pos.clone(),
                        idx: 0,
                        path: self.path,
                        source: size,
                    }
                    );

                    let tok = match (ty_lexer.next(), size_lexer.next()) {
                        (Some((_, ty)), Some((_, size))) if ty.is_type() => Token::Array{ty: Box::new(ty), size: Box::new(size)},
                        (Some((_, Token::Ident(user_ty))), Some((_, size))) => Token::Array{ty: Box::new(Token::Ident(user_ty)), size: Box::new(size)},
                        (Some(_), Some(_)) => self.report_error(self.pos.clone(), "invalid array type"),
                        (None, Some(_)) => self.report_error(self.pos.clone(), "missing array type"),
                        (Some(_), None) => self.report_error(self.pos.clone(), "missing array size"),
                        (None, None) => self.report_error(self.pos.clone(), "missing array type and size"),
                    };

                    match (ty_lexer.next(), size_lexer.next()) {
                        (None, None) => tok,
                        (Some(_), None) => self.report_error(self.pos.clone(), "Too many tokens in array type"),
                        (None, Some(_)) => self.report_error(self.pos.clone(), "Too many tokens in array size"),
                        (Some(_), Some(_)) => self.report_error(self.pos.clone(), "Too many tokens in array type and size"),
                    }
                },
                _ => unreachable!(),
            };
        Some((Span::new(start, self.pos.clone()), tok))
    }

    fn consume_ops(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = match self.consume(
            &Regex::new(r"(\*(const|mut)|[<>]{2}=|[+*|&<>=-]{2}|[+*/%|&!<>^-]=?|[~=])").unwrap(),
        )? {
            "+=" => Token::PlusAssign,
            "++" => Token::Increment,
            "+" => Token::Plus,
            "-=" => Token::MinusAssign,
            "--" => Token::Decrement,
            "-" => Token::Minus,
            "*const" => Token::ConstDeref,
            "*mut" => Token::MutDeref,
            "*=" => Token::TimesAssign,
            "**" => Token::Power,
            "*" => Token::Times,
            "/=" => Token::DivideAssign,
            "/" => Token::Divide,
            "%=" => Token::ModuloAssign,
            "%" => Token::Modulo,
            "&&" => Token::LAnd,
            "&=" => Token::AndAssign,
            "&" => Token::BAnd,
            "||" => Token::LOr,
            "|=" => Token::OrAssign,
            "|" => Token::BOr,
            "!" => Token::Bang,
            "!=" => Token::NEqual,
            "<" => Token::LAngle,
            "<<=" => Token::LShiftAssign,
            "<<" => Token::LShift,
            "<=" => Token::LEq,
            ">" => Token::RAngle,
            ">>=" => Token::RShiftAssign,
            ">>" => Token::RShift,
            ">=" => Token::GEq,
            "==" => Token::Equal,
            "=" => Token::Assign,
            "^=" => Token::XorAssign,
            "^" => Token::Xor,
            "~" => Token::BNeg,
            _ => unreachable!(),
        };
        Some((Span::new(start, self.pos.clone()), tok))
    }

    fn consume_delims(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = match self.consume(&Regex::new(r"[\[\].(){};,.:]").unwrap())? {
            "[" => Token::LBracket,
            "]" => Token::RBracket,
            "(" => Token::LParen,
            ")" => Token::RParen,
            "{" => Token::LBrace,
            "}" => Token::RBrace,
            ";" => Token::Semicolon,
            "," => Token::Comma,
            "." => Token::Dot,
            ":" => Token::Colon,
            _ => unreachable!(),
        };
        Some((Span::new(start, self.pos.clone()), tok))
    }

    fn consume_bool_lit(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = match self.consume(&Regex::new(r"true|false").unwrap())? {
            "true" => Token::BoolLit(true),
            "false" => Token::BoolLit(false),
            _ => unreachable!(),
        };
        Some((Span::new(start, self.pos.clone()), tok))
    }

    fn consume_keywords(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = match self.consume(&Regex::new(r"mut|let|if|else|while|for|in").unwrap())? {
            "mut" => Token::Mut,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            _ => unreachable!(),
        };
        Some((Span::new(start, self.pos.clone()), tok))
    }

    fn consume_ident(&mut self) -> Option<(Span, Token)> {
        let start = self.pos.clone();
        let tok = Token::Ident(
            self.consume(&Regex::new(r"\w[\w_-]*").unwrap())?
                .to_string(),
        );
        Some((Span::new(start, self.pos.clone()), tok))
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
                .or_else(|| self.consume_type())
                .or_else(|| self.consume_char_lit())
                .or_else(|| self.consume_f_lit())
                .or_else(|| self.consume_int_lit())
                .or_else(|| self.consume_ops())
                .or_else(|| self.consume_delims())
                .or_else(|| self.consume_bool_lit())
                .or_else(|| self.consume_keywords())
                .or_else(|| self.consume_ident())
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
