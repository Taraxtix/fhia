pub mod expr;
mod ops;
#[cfg(test)]
mod tests;
pub mod types;

use std::fmt::Display;

use crate::lexer::{Lexer, Span, Token};
use expr::{Expr, ExprKind};
use ops::{BinOp, UnOp};
use types::Type;

#[derive(Debug, Clone, PartialEq)]
struct Var {
    name: String,
    ty: Option<Type>,
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
pub struct Env {
    vars: Vec<Var>,
    functions: Vec<Func>,
}

impl Default for Env {
    fn default() -> Env {
        Env {
            vars: vec![
                Var {
                    name: String::from("argc"),
                    ty: Some(Type::Size),
                },
                Var {
                    name: String::from("argv"),
                    ty: Some(Type::c_ref(Type::c_ref(Type::Char))),
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
    pub env: Env,
    pub parent: Option<Box<Scope>>,
}
impl Scope {
    fn get_var(&self, name: &str) -> Option<&Var> {
        for var in &self.env.vars {
            if var.name == name {
                return Some(var);
            }
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(name);
        }
        None
    }

    fn get_func(&self, name: &str) -> Option<&Func> {
        for func in &self.env.functions {
            if func.name == name {
                return Some(func);
            }
        }
        if let Some(parent) = &self.parent {
            return parent.get_func(name);
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),
    Semicolon,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pub collected: Vec<Item>,
    debug: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, debug: bool) -> Self {
        let mut it = Self {
            lexer,
            collected: Vec::new(),
            debug,
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

    fn debug(&self, msg: impl Display) {
        if self.debug {
            println!("[DEBUG]: {}", msg);
        }
    }

    fn peek_last_expr(&self) -> Option<&Expr> {
        for item in self.collected.iter().rev() {
            if let Item::Expr(expr) = item {
                return Some(expr);
            }
        }
        None
    }

    fn pop_last_expr(&mut self) -> Option<Expr> {
        match self.collected.pop() {
            Some(Item::Expr(expr)) => Some(expr),
            Some(Item::Semicolon) => self.pop_last_expr(),
            _ => None,
        }
    }

    fn get_scope(&self, new_env: Option<Env>) -> Scope {
        match (new_env, self.peek_last_expr()) {
            (None, None) => Scope {
                env: Env::default(),
                parent: None,
            },
            // (None, Some(Expr{scope, kind: ExprKind::Block(_), ..})) => *scope.parent.unwrap(),
            (None, Some(Expr { scope, .. })) => scope.clone(),
            (Some(new_env), None) => Scope {
                env: new_env,
                parent: Some(Box::new(Scope {
                    env: Env::default(),
                    parent: None,
                })),
            },
            (Some(new_env), Some(Expr { scope, .. })) => Scope {
                env: new_env,
                parent: Some(Box::new(scope.clone())),
            },
        }
    }

    fn parse(&mut self) {
        while let Some((span, tok)) = self.lexer.next() {
            use Token as T;
            self.debug(format!("parse( {}, {} )", span.start, tok));
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
                    self.collected.push(Item::Semicolon);
                    continue;
                }

                T::LBracket => self.parse_index((span, tok)),

                T::Ident(_) => self.parse_ident((span, tok)),

                T::Let => todo!(),
                T::If => todo!(),
                T::Else => todo!(),
                T::While => todo!(),
                T::For => todo!(),
                T::In => todo!(),

                T::Assign => todo!(),
                T::PlusAssign => todo!(),
                T::MinusAssign => todo!(),
                T::TimesAssign => todo!(),
                T::DivideAssign => todo!(),
                T::ModuloAssign => todo!(),
                T::AndAssign => todo!(),
                T::OrAssign => todo!(),
                T::LShiftAssign => todo!(),
                T::RShiftAssign => todo!(),
                T::XorAssign => todo!(),

                T::RParen => todo!(),
                T::LParen => todo!(),
                T::LBrace => todo!(),
                T::RBrace => todo!(),

                T::Colon => todo!(),
                T::Dot => todo!(),

                _ => self.parse_expr((span, tok)),
            };
            self.collected.push(Item::Expr(expr));
        }
    }
    fn parse_expr(&mut self, (span, tok): (Span, Token)) -> Expr {
        use Token as T;
        self.debug(format!("parse_expr( {}, {} )", span.start, tok));
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
            | T::Bang
            | T::BNeg => self.parse_unop((span, tok)),

            T::Ident(_) => self.parse_ident((span, tok)),

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
        self.debug(format!(
            "call parse_array_lit( {}, {} )",
            curr.0.start, curr.1
        ));
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
        self.debug(format!("call parse_binop( {}, {} )", curr.0.start, curr.1));

        let lhs = match self.collected.pop() {
            Some(Item::Semicolon) if curr.1 == Token::Minus => return self.parse_unop(curr),
            Some(Item::Expr(expr)) => expr,
            None if curr.1 == Token::Minus => return self.parse_unop(curr),
            _ => match self.pop_last_expr() {
                Some(expr) => expr,
                None => {
                    self.report_parsing_error(Some(curr), "Expected an expression before operator")
                }
            },
        };

        let next = self.lexer.next().unwrap_or_else(|| {
            self.report_parsing_error(Some(curr.clone()), "Expected an expression after operator")
        });
        let rhs = self.parse_expr(next);
        BinOp::from_token(&curr.1).get_expr(lhs, rhs)
    }

    fn parse_unop(&mut self, curr: (Span, Token)) -> Expr {
        self.debug(format!("call parse_unop( {}, {} )", curr.0.start, curr.1));
        let next = self.lexer.next().unwrap_or_else(|| {
            self.report_parsing_error(Some(curr.clone()), "Expected an expression after operator")
        });
        let arg = self.parse_expr(next);
        UnOp::from_token(&curr.1).get_expr(arg)
    }

    fn parse_index(&mut self, curr: (Span, Token)) -> Expr {
        self.debug(format!("call parse_index( {}, {} )", curr.0.start, curr.1));

        let lhs = match self.collected.pop() {
            Some(Item::Semicolon) => return self.parse_array_lit(curr),
            Some(Item::Expr(expr)) => expr,
            None => return self.parse_array_lit(curr),
        };

        let next = self.lexer.next().unwrap_or_else(|| {
            self.report_parsing_error(Some(curr.clone()), "Expected an expression after operator")
        });
        let rhs = self.parse_expr(next);

        match self.lexer.next() {
            None => self.report_parsing_error(None, "Expected closing bracket after index"),
            Some((span, Token::RBracket)) => Expr {
                ty: None,
                span: Span::new(lhs.span.start.clone(), span.end.clone()),
                scope: self.get_scope(None),
                kind: ExprKind::Index {
                    expr: Box::new(lhs),
                    index: Box::new(rhs),
                },
            },
            Some((span, tok)) => {
                self.report_parsing_error(Some((span, tok)), "Expected closing bracket after index")
            }
        }
    }

    fn parse_ident(&mut self, (span, tok): (Span, Token)) -> Expr {
        let scope = self.get_scope(None);
        let Token::Ident(name) = tok.clone() else {
            unreachable!()
        };
        if let Some(var) = scope.get_var(&name) {
            Expr {
                ty: var.ty.clone(),
                span: Span::new(span.start.clone(), span.end.clone()),
                scope: self.get_scope(None),
                kind: ExprKind::Var(name),
            }
        } else if let Some(func) = scope.get_func(&name) {
            Expr {
                kind: ExprKind::FuncCall {
                    args: func
                        .args
                        .iter()
                        .map(|_| {
                            let next = self.lexer.next().unwrap_or_else(|| {
                                self.report_parsing_error(
                                    None,
                                    format!("Not enough arguments to call {}", &name),
                                )
                            });
                            self.parse_expr(next)
                        })
                        .collect(),
                    name,
                },
                ty: Some(func.ty.clone()),
                span,
                scope,
            }
        } else {
            self.report_parsing_error(Some((span, tok)), "Unknown identifier")
        }
    }
}
