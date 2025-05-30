use std::fmt::Display;
use std::iter::Peekable;

use crate::lexer::{Span, Token};

use super::Args;
use super::lexer::Lexer;

#[derive(Debug, Clone)]
pub enum Expr {
    Decla {
        name: String,
        args: Vec<Pattern>,
        ty: Ty,
        expr: Box<Expr>,
    },
    Ident(String),
    U32(u32),
    U64(u64),
    U128(u128),
    F64(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Unit(Option<Box<Expr>>),
    Sequence {
        curr: Box<Expr>,
        next: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Paren(Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Decla {
                    name,
                    args,
                    ty,
                    expr,
                } => [
                    format!("Declaration: {name} ("),
                    args.iter()
                        .map(|arg| format!("{arg}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    format!("): {ty} =\n\t{expr}"),
                ]
                .join(" "),
                Expr::Ident(name) => name.to_string(),
                Expr::U32(lit) => format!("{lit}"),
                Expr::U64(lit) => format!("{lit}"),
                Expr::U128(lit) => format!("{lit}"),
                Expr::F64(lit) => format!("{lit}"),
                Expr::Bool(lit) => format!("{lit}"),
                Expr::Str(lit) => lit.to_string(),
                Expr::Char(lit) => format!("{lit}"),
                Expr::Unit(expr) => match expr {
                    Some(expr) => format!("{expr};"),
                    None => "()".into(),
                },
                Expr::Sequence { curr, next } => format!("{curr} {next}"),
                Expr::BinOp { op, lhs, rhs } => format!("({lhs} {op} {rhs})"),
                Expr::Paren(expr) => format!("({expr})"),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Power,
    Modulo,
    BAnd,
    LAnd,
    BOr,
    LOr,
    Xor,
    LShift,
    RShift,
    Equal,
    NEqual,
    Gt,
    GEq,
    Lt,
    LEq,
    Assign,
    PlusAssign,
    MinusAssign,
    TimesAssign,
    DivideAssign,
    ModuloAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
    Dot,
}

impl BinOp {
    fn from_tok(op: Token) -> Self {
        match op {
            // Token::If => todo!(),    // |
            // Token::Else => todo!(),  // |
            // Token::While => todo!(), // | See about ternary operator and stuffs like that
            // Token::For => todo!(),   // |
            // Token::In => todo!(),    // |
            Token::Plus => BinOp::Plus,
            Token::PlusAssign => BinOp::PlusAssign,
            Token::Minus => BinOp::Minus,
            Token::MinusAssign => BinOp::MinusAssign,
            Token::Times => BinOp::Times,
            Token::TimesAssign => BinOp::TimesAssign,
            Token::Power => BinOp::Power,
            Token::Divide => BinOp::Divide,
            Token::DivideAssign => BinOp::DivideAssign,
            Token::Modulo => BinOp::Modulo,
            Token::ModuloAssign => BinOp::ModuloAssign,
            Token::LAnd => BinOp::LAnd,
            Token::BAnd => BinOp::BAnd,
            Token::AndAssign => BinOp::AndAssign,
            Token::LOr => BinOp::LOr,
            Token::BOr => BinOp::BOr,
            Token::OrAssign => BinOp::OrAssign,
            Token::NEqual => BinOp::NEqual,
            Token::LAngle => BinOp::Lt,
            Token::LShift => BinOp::LShift,
            Token::LShiftAssign => BinOp::LShiftAssign,
            Token::LEq => BinOp::LEq,
            Token::RAngle => BinOp::Gt,
            Token::RShift => BinOp::RShift,
            Token::RShiftAssign => BinOp::RShiftAssign,
            Token::GEq => BinOp::GEq,
            Token::Equal => BinOp::Equal,
            Token::Assign => BinOp::Assign,
            Token::Xor => BinOp::Xor,
            Token::XorAssign => BinOp::XorAssign,
            Token::Dot => BinOp::Dot,
            _ => unreachable!("BinOp::from_tok({op})"),
        }
    }

    // Inspired by Pratt Parsing
    fn attraction_power(&self) -> usize {
        match self {
            BinOp::Assign => 0,
            BinOp::PlusAssign => 0,
            BinOp::MinusAssign => 0,
            BinOp::TimesAssign => 0,
            BinOp::DivideAssign => 0,
            BinOp::ModuloAssign => 0,
            BinOp::AndAssign => 0,
            BinOp::OrAssign => 0,
            BinOp::XorAssign => 0,
            BinOp::LShiftAssign => 0,
            BinOp::RShiftAssign => 0,
            BinOp::LOr => 1,
            BinOp::LAnd => 2,
            BinOp::BOr => 3,
            BinOp::Xor => 4,
            BinOp::BAnd => 5,
            BinOp::Equal => 6,
            BinOp::NEqual => 6,
            BinOp::Gt => 7,
            BinOp::GEq => 7,
            BinOp::Lt => 7,
            BinOp::LEq => 7,
            BinOp::LShift => 8,
            BinOp::RShift => 8,
            BinOp::Plus => 9,
            BinOp::Minus => 9,
            BinOp::Times => 10,
            BinOp::Divide => 10,
            BinOp::Modulo => 10,
            BinOp::Power => 11,
            BinOp::Dot => 12,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BinOp::Plus => "+",
            BinOp::Minus => "-",
            BinOp::Times => "*",
            BinOp::Divide => "/",
            BinOp::Modulo => "%",
            BinOp::LAnd => "&&",
            BinOp::BAnd => "&",
            BinOp::LOr => "||",
            BinOp::BOr => "|",
            BinOp::Xor => "^",
            BinOp::LShift => "<<",
            BinOp::RShift => ">>",
            BinOp::Equal => "==",
            BinOp::NEqual => "!=",
            BinOp::Gt => ">",
            BinOp::GEq => ">=",
            BinOp::Lt => "<",
            BinOp::LEq => "<=",
            BinOp::Assign => "=",
            BinOp::PlusAssign => "+=",
            BinOp::MinusAssign => "-=",
            BinOp::TimesAssign => "*=",
            BinOp::DivideAssign => "/=",
            BinOp::ModuloAssign => "%=",
            BinOp::AndAssign => "&=",
            BinOp::OrAssign => "|=",
            BinOp::XorAssign => "^=",
            BinOp::LShiftAssign => "<<=",
            BinOp::RShiftAssign => ">>=",
            BinOp::Power => "**",
            BinOp::Dot => ".",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    path: &'a str,
    lexer: Peekable<Lexer<'a>>,
    // symbols: Vec<Symbol>,
}

impl<'a> Parser<'a> {
    pub fn parse_user_program(lexer: Lexer<'a>, _args: &Args) -> Self {
        Self {
            path: lexer.path,
            lexer: lexer.peekable(),
            // symbols: Vec::new(),
        }
    }

    fn report_error(&mut self, span: Span, msg: impl Display) -> ! {
        eprintln!("[ERROR]: {}:{}: Parsing Error: {}", self.path, span, msg);
        std::process::exit(1);
    }

    fn expect_tok(&mut self, span: Span, expected: Token) {
        match self.next_tok() {
            Some((_, got)) if expected == got => (),
            Some((span, got)) => {
                self.report_error(span, format!("Expected token `{expected}` but got `{got}`"))
            }
            None => self.report_error(span, format!("Expected token `{expected}` but got nothing")),
        }
    }

    fn next_tok(&mut self) -> Option<(Span, Token)> {
        self.lexer.next()
    }

    fn peek_tok(&mut self) -> Option<&(Span, Token)> {
        self.lexer.peek()
    }

    fn parse_let(&mut self, span: Span) -> Result<Expr, (Span, String)> {
        let (name_span, name) = match self.next_tok() {
            Some((span, Token::Ident(name))) => Ok((span, name)),
            Some((span, tok)) => Err((
                span,
                format!("Expected identifier after `let` but got `{tok}`"),
            )),
            None => Err((
                span,
                "Expected identifier after `let` but got nothing".into(),
            )),
        }?;

        let mut args: Vec<Pattern> = vec![];
        let mut typed = false;
        let mut last_span = None;

        loop {
            match self.next_tok().ok_or((
                last_span.unwrap_or(name_span.clone()),
                "Expected either `:`, `=` or an argument pattern but got nothing".into(),
            ))? {
                (span, Token::Colon) => {
                    last_span = Some(span);
                    typed = true;
                    break;
                }
                (span, Token::Equal) => {
                    last_span = Some(span);
                    break;
                }
                (arg_span, Token::LParen) => {
                    last_span = Some(arg_span.clone());
                    args.push(self.parse_paren_arg(arg_span));
                }
                (span, Token::Ident(arg_name)) => {
                    last_span = Some(span);
                    args.push(Pattern::NamedWildcard(arg_name));
                }
                (span, Token::Wildcard) => {
                    last_span = Some(span);
                    args.push(Pattern::Wildcard);
                }
                (span, tok) => {
                    return Err((
                        span,
                        format!("Expected either `:`, `=` or an argument pattern but got {tok}"),
                    ));
                }
            }
        }

        let last_span = last_span.unwrap();

        Ok(Expr::Decla {
            name,
            args,
            ty: if !typed {
                Ty::Unknown
            } else {
                let ty = self.parse_type(name_span);
                self.expect_tok(last_span.clone(), Token::Assign);
                ty
            },
            expr: Box::new(self.parse_expr(last_span, false, false)?),
        })
    }

    fn parse_paren_arg(&mut self, span: Span) -> Pattern {
        let (span, name) = match self.next_tok() {
            Some((span, Token::Ident(name))) => (span, name),
            _ => self.report_error(span, "Expected pattern as argument"),
        };
        self.expect_tok(span.clone(), Token::Colon);

        let ty = self.parse_type(span.clone());

        self.expect_tok(span.clone(), Token::RParen);
        Pattern::Typed { ty, name }
    }

    fn parse_type(&mut self, span: Span) -> Ty {
        match self.next_tok() {
            Some((_, Token::I8)) => Ty::I8,
            Some((_, Token::I16)) => Ty::I16,
            Some((_, Token::I32)) => Ty::I32,
            Some((_, Token::I64)) => Ty::I64,
            Some((_, Token::I128)) => Ty::I128,
            Some((_, Token::U8)) => Ty::U8,
            Some((_, Token::U16)) => Ty::U16,
            Some((_, Token::U32)) => Ty::U32,
            Some((_, Token::U128)) => Ty::U128,
            Some((_, Token::U64)) => Ty::U64,
            Some((_, Token::F32)) => Ty::F32,
            Some((_, Token::F64)) => Ty::F64,
            Some((_, Token::F128)) => Ty::F128,
            Some((_, Token::Usize)) => Ty::Usize,
            Some((_, Token::Isize)) => Ty::Isize,
            Some((_, Token::Char)) => Ty::Char,
            Some((_, Token::Str)) => Ty::Str,
            Some((_, Token::Bool)) => Ty::Bool,
            Some((_, Token::Unit)) => Ty::Unit,
            Some((span, Token::Mut)) => Ty::Mut(Box::new(self.parse_type(span))),
            Some((_, Token::Bang)) => Ty::Never,
            Some((_, Token::Wildcard)) => Ty::Unknown,
            Some((_, Token::BAnd)) => self.parse_ref(span),
            Some((_, Token::LBracket)) => self.parse_array_slice(span),
            Some((_, Token::Ident(type_name))) => todo!("Parse custom type name {type_name}"),
            Some((span, tok)) => self.report_error(span, format!("Expected type, got token {tok}")),
            None => self.report_error(span, "Expected type, but got none."),
        }
    }

    fn parse_ref(&mut self, span: Span) -> Ty {
        if self
            .peek_tok()
            .map(|(_, tok)| tok == &Token::Mut)
            .unwrap_or(false)
        {
            self.next_tok();
            Ty::Mut(Box::new(self.parse_type(span)))
        } else {
            Ty::ConstRef(Box::new(self.parse_type(span)))
        }
    }

    fn parse_array_slice(&mut self, span: Span) -> Ty {
        let val_type = self.parse_type(span.clone());
        match self.next_tok() {
            Some((_, Token::RBracket)) => Ty::Slice(Box::new(val_type)),
            Some((_, Token::Semicolon)) => {
                let (span, size) = match self.next_tok() {
                    Some((span, Token::U32Lit(size))) => (span, size as usize),
                    Some((span, Token::U64Lit(size))) => (span, size as usize),
                    Some((span, Token::U128Lit(size))) => (span, size as usize),
                    _ => self.report_error(span, "Expected array size"),
                };
                self.expect_tok(span, Token::RBracket);
                Ty::Arr {
                    ty: Box::new(val_type),
                    size,
                }
            }
            _ => self.report_error(span, "Misformed array/slice type"),
        }
    }

    fn parse_expr(
        &mut self,
        span: Span,
        in_block: bool,
        collecting_rhs: bool,
    ) -> Result<Expr, (Span, String)> {
        let (span, tok) = self
            .next_tok()
            .ok_or((span, "Expected expression".to_string()))?;
        let expr = match tok {
            Token::Ident(name) => Expr::Ident(name),
            Token::U32Lit(lit) => Expr::U32(lit),
            Token::U64Lit(lit) => Expr::U64(lit),
            Token::U128Lit(lit) => Expr::U128(lit),
            Token::FLit(lit) => Expr::F64(lit),
            Token::StrLit(lit) => Expr::Str(lit),
            Token::CharLit(lit) => Expr::Char(lit),
            Token::BoolLit(lit) => Expr::Bool(lit),
            Token::Unit => Expr::Unit(None),
            Token::Let => self.parse_let(span)?,
            Token::If => todo!("Parse if expression"),
            Token::While => todo!("Parse while expression"),
            Token::For => todo!("Parse for expression"),
            Token::Increment => todo!("Parse prefix increment expression"),
            Token::Decrement => todo!("Parse prefix decrement expression"),
            Token::Minus => todo!("Parse minus unop expression"),
            Token::Times => todo!("Parse deref expression"),
            Token::Bang => todo!("Parse LNot expression"),
            Token::BNeg => todo!("Parse BNeg expression"),
            Token::LBracket => {
                todo!("Parse array slice expression");
                // self.expect_tok(span, Token::RBracket);
                // expr
            }
            Token::LParen => {
                let expr = self.parse_expr(span.clone(), in_block, collecting_rhs)?;
                self.expect_tok(span, Token::RParen);
                Expr::Paren(Box::new(expr))
            }
            Token::LBrace => {
                let expr = self.parse_expr(span.clone(), true, collecting_rhs)?;
                self.expect_tok(span, Token::RBrace);
                expr
            }
            tok => {
                return Err((span, format!("Expected expression got {tok}")));
            }
        };
        let Some((peeked_span, peeked)) = self.peek_tok().cloned() else {
            return Ok(expr);
        };

        match peeked.clone() {
            // Token::StrLit(_) => todo!("Parse string/char concatenation like that ?"),
            // Token::CharLit(_) => todo!("Parse string/char concatenation like that ?"),
            // Token::If => todo!("Parse ternary expression like that ?"),
            // Token::Else => todo!("Parse ternary expression like that ?"),
            // Token::While => todo!("Parse while expression like that ?"),
            // Token::For => todo!("Parse for expression like that ?"),
            // Token::In => todo!("Parse in like "value in SET" like that ?"),
            Token::Increment
            | Token::Decrement
            | Token::PlusAssign
            | Token::MinusAssign
            | Token::TimesAssign
            | Token::DivideAssign
            | Token::ModuloAssign
            | Token::AndAssign
            | Token::OrAssign
            | Token::LShiftAssign
            | Token::RShiftAssign
            | Token::XorAssign
            | Token::Assign
            | Token::Plus
            | Token::Minus
            | Token::Times
            | Token::Power
            | Token::Divide
            | Token::Modulo
            | Token::LAnd
            | Token::BAnd
            | Token::LOr
            | Token::BOr
            | Token::NEqual
            | Token::LAngle
            | Token::LShift
            | Token::LEq
            | Token::RAngle
            | Token::RShift
            | Token::GEq
            | Token::Equal
            | Token::Xor
            | Token::Dot => {
                let op = self.parse_binop(expr)?;
                if let Some((_, Token::Semicolon)) = self.peek_tok() {
                    self.next_tok(); // Consume semicolon
                    if in_block {
                        if let Some((_, Token::RBrace)) = self.peek_tok() {
                            Ok(Expr::Unit(Some(Box::new(op))))
                        } else {
                            Ok(Expr::Sequence {
                                curr: Box::new(Expr::Unit(Some(Box::new(op)))),
                                next: Box::new(self.parse_expr(
                                    peeked_span.clone(),
                                    in_block,
                                    collecting_rhs,
                                )?),
                            })
                        }
                    } else {
                        Ok(Expr::Unit(Some(Box::new(op))))
                    }
                } else {
                    Ok(op)
                }
            }
            Token::Semicolon if in_block => {
                let span = peeked_span.clone();
                self.next_tok(); // Consume semicolon
                if let Some((_, Token::RBrace)) = self.peek_tok() {
                    self.next_tok(); // Consume right brace
                    Ok(Expr::Unit(Some(Box::new(expr))))
                } else {
                    Ok(Expr::Sequence {
                        curr: Box::new(Expr::Unit(Some(Box::new(expr)))),
                        next: Box::new(self.parse_expr(span, in_block, collecting_rhs)?),
                    })
                }
            }
            Token::Semicolon if !collecting_rhs => {
                self.next_tok(); // Consume semicolon
                Ok(Expr::Unit(Some(Box::new(expr))))
            }
            _ => Ok(expr),
        }
    }

    fn parse_binop(&mut self, lhs: Expr) -> Result<Expr, (Span, String)> {
        let (op_span, op) = self.next_tok().unwrap();
        let op = BinOp::from_tok(op);
        let rhs = self.parse_expr(op_span.clone(), false, true)?;
        Ok(match (lhs, rhs) {
            (
                Expr::BinOp {
                    op: l_op,
                    lhs: l_lhs,
                    rhs: l_rhs,
                },
                Expr::BinOp {
                    op: r_op,
                    lhs: r_lhs,
                    rhs: r_rhs,
                },
            ) => {
                if op.attraction_power() > l_op.attraction_power() {
                    if r_op.attraction_power() > op.attraction_power() {
                        // a | b + c * d
                        Expr::BinOp {
                            op: l_op,
                            lhs: l_lhs,
                            rhs: Box::new(Expr::BinOp {
                                op,
                                lhs: l_rhs,
                                rhs: Box::new(Expr::BinOp {
                                    op: r_op,
                                    lhs: r_lhs,
                                    rhs: r_rhs,
                                }),
                            }),
                        }
                    } else {
                        //a + b * c + d
                        Expr::BinOp {
                            op: r_op,
                            lhs: Box::new(Expr::BinOp {
                                op: l_op,
                                lhs: l_lhs,
                                rhs: Box::new(Expr::BinOp {
                                    op,
                                    lhs: l_rhs,
                                    rhs: r_lhs,
                                }),
                            }),
                            rhs: r_rhs,
                        }
                    }
                } else if r_op.attraction_power() > op.attraction_power() {
                    // a * b + c * d
                    Expr::BinOp {
                        op,
                        lhs: Box::new(Expr::BinOp {
                            op: l_op,
                            lhs: l_lhs,
                            rhs: l_rhs,
                        }),
                        rhs: Box::new(Expr::BinOp {
                            op: r_op,
                            lhs: r_lhs,
                            rhs: r_rhs,
                        }),
                    }
                } else {
                    // a * b + c | d
                    Expr::BinOp {
                        op: r_op,
                        lhs: Box::new(Expr::BinOp {
                            op,
                            lhs: Box::new(Expr::BinOp {
                                op: l_op,
                                lhs: l_lhs,
                                rhs: l_rhs,
                            }),
                            rhs: r_lhs,
                        }),
                        rhs: r_rhs,
                    }
                }
            }
            (
                Expr::BinOp {
                    op: l_op,
                    lhs: l_lhs,
                    rhs: l_rhs,
                },
                rhs,
            ) => {
                if op.attraction_power() > l_op.attraction_power() {
                    // a + b * c
                    Expr::BinOp {
                        op: l_op,
                        lhs: l_lhs,
                        rhs: Box::new(Expr::BinOp {
                            op,
                            lhs: l_rhs,
                            rhs: Box::new(rhs),
                        }),
                    }
                } else {
                    // a * b + c
                    Expr::BinOp {
                        op,
                        lhs: Box::new(Expr::BinOp {
                            op: l_op,
                            lhs: l_lhs,
                            rhs: l_rhs,
                        }),
                        rhs: Box::new(rhs),
                    }
                }
            }
            (
                lhs,
                Expr::BinOp {
                    op: r_op,
                    lhs: r_lhs,
                    rhs: r_rhs,
                },
            ) => {
                if op.attraction_power() > r_op.attraction_power() {
                    // a * b + c
                    Expr::BinOp {
                        op: r_op,
                        lhs: Box::new(Expr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: r_lhs,
                        }),
                        rhs: r_rhs,
                    }
                } else {
                    // a + b * c
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(Expr::BinOp {
                            op: r_op,
                            lhs: r_lhs,
                            rhs: r_rhs,
                        }),
                    }
                }
            }
            (lhs, rhs) => Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        })
    }
}

impl Iterator for Parser<'_> {
    type Item = Expr;

    fn next(&mut self) -> Option<Self::Item> {
        let (span, tok) = self.next_tok()?;
        Some(
            match tok {
                Token::Ident(_) => todo!(),
                Token::Let => self.parse_let(span),
                Token::If => todo!(),
                Token::Else => todo!(),
                Token::While => todo!(),
                Token::For => todo!(),
                Token::In => todo!(),
                Token::Use => todo!(),
                Token::Mut => todo!(),
                Token::Const => todo!(),
                Token::Inline => todo!(),
                Token::Extern => todo!(),
                Token::LBracket => todo!(),
                Token::RBracket => todo!(),
                Token::LParen => todo!(),
                Token::RParen => todo!(),
                Token::LBrace => todo!(),
                Token::RBrace => todo!(),
                Token::U32Lit(_) => todo!(),
                Token::U64Lit(_) => todo!(),
                Token::U128Lit(_) => todo!(),
                Token::FLit(_) => todo!(),
                Token::StrLit(_) => todo!(),
                Token::CharLit(_) => todo!(),
                Token::BoolLit(_) => todo!(),
                Token::Plus => todo!(),
                Token::PlusAssign => todo!(),
                Token::Increment => todo!(),
                Token::Minus => todo!(),
                Token::Decrement => todo!(),
                Token::MinusAssign => todo!(),
                Token::Times => todo!(),
                Token::TimesAssign => todo!(),
                Token::Power => todo!(),
                Token::Divide => todo!(),
                Token::DivideAssign => todo!(),
                Token::Modulo => todo!(),
                Token::ModuloAssign => todo!(),
                Token::LAnd => todo!(),
                Token::BAnd => todo!(),
                Token::AndAssign => todo!(),
                Token::LOr => todo!(),
                Token::BOr => todo!(),
                Token::OrAssign => todo!(),
                Token::Bang => todo!(),
                Token::NEqual => todo!(),
                Token::LAngle => todo!(),
                Token::LShift => todo!(),
                Token::LShiftAssign => todo!(),
                Token::LEq => todo!(),
                Token::RAngle => todo!(),
                Token::RShift => todo!(),
                Token::RShiftAssign => todo!(),
                Token::GEq => todo!(),
                Token::Equal => todo!(),
                Token::Assign => todo!(),
                Token::BNeg => todo!(),
                Token::Xor => todo!(),
                Token::XorAssign => todo!(),
                Token::Comma => todo!(),
                Token::Semicolon => todo!(),
                Token::Colon => todo!(),
                Token::Dot => todo!(),
                Token::I8 => todo!(),
                Token::I16 => todo!(),
                Token::I32 => todo!(),
                Token::I64 => todo!(),
                Token::I128 => todo!(),
                Token::U8 => todo!(),
                Token::U16 => todo!(),
                Token::U32 => todo!(),
                Token::U64 => todo!(),
                Token::U128 => todo!(),
                Token::Isize => todo!(),
                Token::Usize => todo!(),
                Token::F32 => todo!(),
                Token::F64 => todo!(),
                Token::F128 => todo!(),
                Token::Bool => todo!(),
                Token::Char => todo!(),
                Token::Str => todo!(),
                Token::Unit => todo!(),
                Token::Wildcard => todo!(),
            }
            .unwrap_or_else(|e| self.report_error(e.0, e.1)),
        )
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    NamedWildcard(String),
    Typed { ty: Ty, name: String },
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pattern::Wildcard => "_".into(),
                Pattern::NamedWildcard(name) => format!("{name}: _"),
                Pattern::Typed { ty, name } => format!("{name}: {ty}"),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
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
    F32,
    F64,
    F128,
    Usize,
    Isize,
    Char,
    Str,
    ConstPtr(Box<Ty>),
    MutPtr(Box<Ty>),
    ConstRef(Box<Ty>),
    MutRef(Box<Ty>),
    Slice(Box<Ty>),
    Arr { ty: Box<Ty>, size: usize },
    Unknown,
    Bool,
    Unit,
    Mut(Box<Ty>),
    Never,
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ty::I8 => "i8".into(),
                Ty::I16 => "i16".into(),
                Ty::I32 => "i32".into(),
                Ty::I64 => "i64".into(),
                Ty::I128 => "i128".into(),
                Ty::U8 => "u8".into(),
                Ty::U16 => "u16".into(),
                Ty::U32 => "u32".into(),
                Ty::U64 => "u64".into(),
                Ty::U128 => "u128".into(),
                Ty::F32 => "f32".into(),
                Ty::F64 => "f64".into(),
                Ty::F128 => "f128".into(),
                Ty::Usize => "usize".into(),
                Ty::Isize => "isize".into(),
                Ty::Char => "char".into(),
                Ty::Str => "str".into(),
                Ty::Bool => "bool".into(),
                Ty::Unit => "()".into(),
                Ty::Never => "!".into(),
                Ty::ConstPtr(ty) => format!("*{ty}"),
                Ty::MutPtr(ty) => format!("*mut {ty}"),
                Ty::ConstRef(ty) => format!("&{ty}"),
                Ty::MutRef(ty) => format!("&mut {ty}"),
                Ty::Slice(ty) => format!("[{ty}]"),
                Ty::Arr { ty, size } => format!("[{ty}; {size}]"),
                Ty::Mut(ty) => format!("mut {ty}"),
                Ty::Unknown => "?".into(),
            }
        )
    }
}
