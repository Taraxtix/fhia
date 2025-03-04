mod expr;
mod ops;
#[cfg(test)]
mod tests;
mod types;

use std::fmt::Display;

use crate::lexer::{Lexer, Span, Token};
use expr::{Expr, ExprKind};
use ops::{BinOp, UnOp};
use types::Type;

#[derive(Debug, Clone, PartialEq)]
struct Var {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone)]
enum _Pattern {
    Wildcard,
    Var(Var),
    Lit(Expr),
}

#[derive(Debug, Clone, PartialEq)]
struct Func {
    name: String,
    args: Vec<Type>,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
struct Env {
    vars: Vec<Var>,
    functions: Vec<Func>,
}

impl Default for Env {
    fn default() -> Env {
        Env {
            vars: vec![
                Var {
                    name: String::from("argc"),
                    ty: Type::Size,
                },
                Var {
                    name: String::from("argv"),
                    ty: Type::c_ref(Type::c_ref(Type::Char)),
                },
            ],
            functions: vec![Func {
                name: String::from("dbg"),
                args: vec![Type::Any],
                ty: Type::Unit,
            }],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Scope {
    env: Env,
    parent: Option<Box<Scope>>,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pub collected: Vec<Expr>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut it = Self {
            lexer,
            collected: Vec::new(),
        };
        it.parse();
        it
    }
    fn get_tok_info_msg(curr: Option<(Span, Token)>, next: Option<(Span, Token)>) -> String {
        match (curr, next) {
            (Some((span, tok)), Some((span2, tok2))) => format!(
                "on token {tok} at {} with next token {tok2} at {} ",
                span.start, span2.start
            ),
            (Some((span, tok)), None) => {
                format!("on token {tok} at {} at the end of input ", span.start)
            }
            (None, None) => String::from("at the end of input"),
            (None, Some((span, tok))) => format!("with next token {tok} at {}", span.start),
        }
    }

    fn report_parsing_error(&mut self, curr: Option<(Span, Token)>, msg: impl Display) -> ! {
        let tok_info_msg = Self::get_tok_info_msg(curr, self.lexer.next());
        println!(
            "[ERROR]: {}:{}: Parsing Error {tok_info_msg}: {msg}",
            self.lexer.path, self.lexer.pos
        );
        std::process::exit(1);
    }

    fn get_scope(&self, new_env: Option<Env>) -> Scope {
        match (new_env, self.collected.last().map(|e| &e.scope)) {
            (None, None) => Scope {
                env: Env::default(),
                parent: None,
            },
            (None, Some(scope)) => scope.clone(),
            (Some(new_env), None) => Scope {
                env: new_env,
                parent: Some(Box::new(Scope {
                    env: Env::default(),
                    parent: None,
                })),
            },
            (Some(new_env), Some(scope)) => Scope {
                env: new_env,
                parent: Some(Box::new(scope.clone())),
            },
        }
    }

    fn parse(&mut self) {
        while let Some((span, tok)) = self.lexer.next() {
            use Token as T;
            let expr = match tok {
                T::Plus
                | T::Minus
                | T::Times
                | T::Power
                | T::Divide
                | T::Modulo
                | T::LAnd
                | T::BAnd
                | T::LOr
                | T::BOr
                | T::NEqual
                | T::LAngle
                | T::LShift
                | T::LEq
                | T::RAngle
                | T::RShift
                | T::GEq
                | T::Equal
                | T::Xor => self.parse_binop((span, tok)),

                T::Semicolon => {
                    if self.collected.is_empty() {
                        self.report_parsing_error(
                            Some((span, tok)),
                            "expected expression before semicolon",
                        );
                    }
                    let mut expr = self.collected.pop().unwrap();
                    expr.span.end = span.end.clone();
                    expr.ty = Some(Type::Unit);
                    expr
                }

                T::Ident(_) => todo!(),
                T::Let => todo!(),
                T::If => todo!(),
                T::Else => todo!(),
                T::While => todo!(),
                T::For => todo!(),
                T::In => todo!(),
                T::PlusAssign => todo!(),
                T::MinusAssign => todo!(),
                T::TimesAssign => todo!(),
                T::DivideAssign => todo!(),
                T::ModuloAssign => todo!(),
                T::AndAssign => todo!(),
                T::OrAssign => todo!(),
                T::LShiftAssign => todo!(),
                T::RShiftAssign => todo!(),
                T::Assign => todo!(),
                T::XorAssign => todo!(),
                T::RBracket => todo!(),
                T::RParen => todo!(),
                T::LParen => todo!(),
                T::LBrace => todo!(),
                T::RBrace => todo!(),
                T::Colon => todo!(),
                T::Dot => todo!(),

                _ => self.parse_expr((span, tok)),
            };
            self.collected.push(expr);
        }
    }

    fn parse_expr(&mut self, (span, tok): (Span, Token)) -> Expr {
        use Token as T;
        match tok {
            T::Unit => Expr {
                kind: ExprKind::Unit,
                ty: Some(Type::Unit),
                span,
                scope: self.get_scope(None),
            },

            T::StrLit(_)
            | T::CharLit(_)
            | T::U32Lit(_)
            | T::U64Lit(_)
            | T::U128Lit(_)
            | T::FLit(_)
            | T::BoolLit(_) => Expr::from_lit((span, tok), self.get_scope(None)),

            T::LBracket => self.parse_array_lit((span, tok)),

            T::Minus
            // | T::Increment
            // | T::Decrement
            | T::ConstDeref
            | T::MutDeref
            | T::Bang
            | T::BNeg => self.parse_unop((span, tok)),

            T::Ident(_) => todo!(),
            T::Let => todo!(),
            T::If => todo!(),
            T::Else => todo!(),
            T::While => todo!(),
            T::For => todo!(),
            T::In => todo!(),
            T::PlusAssign => todo!(),
            T::MinusAssign => todo!(),
            T::TimesAssign => todo!(),
            T::DivideAssign => todo!(),
            T::ModuloAssign => todo!(),
            T::AndAssign => todo!(),
            T::OrAssign => todo!(),
            T::LShiftAssign => todo!(),
            T::RShiftAssign => todo!(),
            T::Assign => todo!(),
            T::XorAssign => todo!(),
            T::RBracket => todo!(),
            T::RParen => todo!(),
            T::LParen => todo!(),
            T::LBrace => todo!(),
            T::RBrace => todo!(),
            T::Semicolon => todo!(),
            T::Colon => todo!(),
            T::Dot => todo!(),
            tok => {
                self.report_parsing_error(
                    Some((span, tok.clone())),
                    format!("Expected expression, got {tok}"),
                );
            }
        }
    }

    fn parse_array_lit(&mut self, curr: (Span, Token)) -> Expr {
        let mut elements: Vec<Expr> = vec![];
        let mut expect_comma = false;

        while let Some((span, tok)) = self.lexer.next() {
            match tok {
                Token::RBracket if expect_comma || elements.is_empty() => {
                    return Expr {
                        ty: None,
                        kind: expr::ExprKind::Array(elements),
                        span: Span::new(curr.0.start, span.end.clone()),
                        scope: self.get_scope(None),
                    };
                }
                Token::Comma if expect_comma => expect_comma = false,
                _ if expect_comma => {
                    self.report_parsing_error(Some((span, tok)), "Expected comma after expr")
                }
                _ => {
                    let expr = self.parse_expr((span.clone(), tok.clone()));
                    elements.push(expr);
                    expect_comma = true;
                }
            }
        }
        self.report_parsing_error(Some(curr), "Unclosed array literal")
    }

    fn parse_binop(&mut self, curr: (Span, Token)) -> Expr {
        if self.collected.is_empty() {
            if curr.1 == Token::Minus {
                return self.parse_unop(curr);
            }
            self.report_parsing_error(Some(curr), "Expected an expression before operator");
        }
        if let Some(Type::Unit) = self.collected.last().unwrap().ty {
            if curr.1 == Token::Minus {
                return self.parse_unop(curr);
            }
        }
        let lhs = self.collected.pop().unwrap();
        let next = self.lexer.next().unwrap_or_else(|| {
            self.report_parsing_error(Some(curr.clone()), "Expected an expression after operator")
        });
        let rhs = self.parse_expr(next);
        BinOp::from_token(&curr.1).get_expr(lhs, rhs)
    }

    fn parse_unop(&mut self, curr: (Span, Token)) -> Expr {
        let next = self.lexer.next().unwrap_or_else(|| {
            self.report_parsing_error(Some(curr.clone()), "Expected an expression after operator")
        });
        let arg = self.parse_expr(next);
        UnOp::from_token(&curr.1).get_expr(arg)
    }
}
