mod expr;
mod ops;
#[cfg(test)]
mod tests;
mod types;

use crate::lexer::{Lexer, Span, Token};
use expr::Expr;
use ops::BinOp;
use types::Type;

#[derive(Debug, Clone)]
struct Var {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone)]
enum Pattern {
    Wildcard,
    Var(Var),
    Lit(Expr),
}

#[derive(Debug, Clone)]
struct Func {
    name: String,
    args: Vec<Type>,
    ty: Type,
}

#[derive(Debug, Clone)]
struct Env {
    vars: Vec<Var>,
    functions: Vec<Func>,
}

impl Env {
    #[inline]
    pub fn default() -> Env {
        Env {
            vars: vec![
                Var {
                    name: String::from("argc"),
                    ty: Type::Size,
                },
                Var {
                    name: String::from("argv"),
                    ty: Type::ConstRef(Box::new(Type::ConstRef(Box::new(Type::Char)))),
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

#[derive(Debug, Clone)]
struct Scope {
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

    fn report_parsing_error(&mut self, curr: Option<(Span, Token)>, msg: &str) -> ! {
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
            let expr = self.parse_expr((span, tok));
            self.collected.push(expr);
        }
    }

    fn parse_expr(&mut self, curr: (Span, Token)) -> Expr {
        use Token as T;
        match curr.1 {
            T::StrLit(_) | T::CharLit(_) | T::ILit(_) | T::FLit(_) | T::BoolLit(_) => {
                Expr::from_lit(curr, self.get_scope(None))
            }
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
            | T::Xor => self.parse_binop(curr),
            T::Ident(_) => todo!(),
            T::Mut => todo!(),
            T::Let => todo!(),
            T::If => todo!(),
            T::Else => todo!(),
            T::While => todo!(),
            T::For => todo!(),
            T::In => todo!(),
            T::PlusAssign => todo!(),
            T::Increment => todo!(),
            T::Decrement => todo!(),
            T::MinusAssign => todo!(),
            T::ConstDeref => todo!(),
            T::MutDeref => todo!(),
            T::TimesAssign => todo!(),
            T::DivideAssign => todo!(),
            T::ModuloAssign => todo!(),
            T::AndAssign => todo!(),
            T::OrAssign => todo!(),
            T::Bang => todo!(),
            T::LShiftAssign => todo!(),
            T::RShiftAssign => todo!(),
            T::Assign => todo!(),
            T::BNeg => todo!(),
            T::XorAssign => todo!(),
            T::LBracket => todo!(),
            T::RBracket => todo!(),
            T::Comma => todo!(),
            T::RParen => todo!(),
            T::LParen => todo!(),
            T::LBrace => todo!(),
            T::RBrace => todo!(),
            T::Semicolon => todo!(),
            T::Colon => todo!(),
            T::Dot => todo!(),
            T::I8 => todo!(),
            T::I16 => todo!(),
            T::I32 => todo!(),
            T::I64 => todo!(),
            T::I128 => todo!(),
            T::U8 => todo!(),
            T::U16 => todo!(),
            T::U32 => todo!(),
            T::U64 => todo!(),
            T::U128 => todo!(),
            T::Size => todo!(),
            T::F32 => todo!(),
            T::F64 => todo!(),
            T::F128 => todo!(),
            T::Bool => todo!(),
            T::Char => todo!(),
            T::Str => todo!(),
            T::Unit => todo!(),
            T::ConstRef => todo!(),
            T::MutRef => todo!(),
            T::Array { ty, size } => todo!(),
            T::Wildcard => todo!(),
        }
    }

    fn parse_binop(&mut self, curr: (Span, Token)) -> Expr {
        if self.collected.is_empty() {
            self.report_parsing_error(Some(curr), "Expected an expression before operator");
        }
        let lhs = self.collected.pop().unwrap();
        let next = self.lexer.next().unwrap_or_else(|| {
            self.report_parsing_error(Some(curr.clone()), "Expected an expression after operator")
        });
        let rhs = self.parse_expr(next);
        use BinOp as Op;
        use Token as T;
        let kind = match curr.1 {
            T::Plus => Op::Add,
            T::Minus => Op::Minus,
            T::Times => Op::Mul,
            T::Power => Op::Power,
            T::Divide => Op::Divide,
            T::Modulo => Op::Modulo,
            T::LAnd => Op::LAnd,
            T::BAnd => Op::BAnd,
            T::LOr => Op::LOr,
            T::BOr => Op::BOr,
            T::NEqual => Op::NEqual,
            T::LShift => Op::LShift,
            T::LEq => Op::LEq,
            T::RShift => Op::RShift,
            T::GEq => Op::GEq,
            T::Equal => Op::Equal,
            T::Xor => Op::Xor,
            T::LAngle => Op::Lt,
            T::RAngle => Op::Gt,
            _ => unreachable!(),
        };
        kind.get_expr(curr, lhs, rhs)
    }
}
