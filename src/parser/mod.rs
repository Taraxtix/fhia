use crate::lexer::{Lexer, Position, Span, Token};

#[cfg(test)]
mod tests;

#[allow(dead_code)]
#[derive(Debug)]
pub enum BinOp {
    Add,
    Minus,
    //...
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum UnOp {
    Minus,

    Decrement,
    Increment,

    DerefMut,
    DerefConst,
    //...
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ExprKind {
    Var(String),
    FuncCall(String, Vec<Expr>),

    ILit(i32),
    FLit(f64),
    StrLit(String),
    BoolLit(bool),
    ArrayLit(Vec<Expr>),

    BinOp {
        kind: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnOp {
        kind: UnOp,
        arg: Box<Expr>,
    },
    //...
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum Type {
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
    Str,
    Char,
    Bool,
    Unit,
    Never,
    Array { ty: Box<Type>, len: usize },
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    ty: Option<Type>,
    span: Span,
    // Add Env
}

impl Expr {
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

    fn report_warning(&self, lexer: &Lexer<'_>, msg: &str) {
        println!("[WARNING]: {}:{}: {msg}", lexer.path, self.span.start);
    }

    fn report_parsing_error(
        mut lexer: Lexer<'_>,
        pos: Position,
        curr: Option<(Span, Token)>,
        msg: &str,
    ) -> ! {
        let tok_info_msg = Self::get_tok_info_msg(curr, lexer.next());
        println!(
            "[ERROR]: {}:{pos}: Parsing Error {tok_info_msg}: {msg}",
            lexer.path,
        );
        std::process::exit(1);
    }

    #[inline]
    pub fn parse(lexer: Lexer<'_>) -> Self {
        Self::parse_expr(lexer, vec![])
    }

    fn return_expr(lexer: Lexer<'_>, mut collected: Vec<Expr>) -> Expr {
        {
            let pos = lexer.pos.clone();
            if collected.is_empty() {
                Self::report_parsing_error(lexer, pos, None, "expected an expression, got nothing")
            }
            let expr = collected.pop().unwrap();

            for rest in collected
                .iter()
                .filter(|expr| !matches!(expr.ty, Some(Type::Unit)))
            {
                rest.report_warning(&lexer, "The value returned by this expression is not used");
            }

            let mut never = collected
                .iter()
                .skip_while(|expr| !matches!(expr.ty, Some(Type::Never)));

            if let Some(never) = never.next() {
                never.report_warning(
                    &lexer,
                    "This expression never returns and will stop the program",
                );
            }

            for rest in never {
                rest.report_warning(&lexer, "This expression is unreachable");
            }

            expr
        }
    }

    fn parse_expr(mut lexer: Lexer<'_>, mut collected: Vec<Expr>) -> Expr {
        let (span, tok) = match lexer.next() {
            Some((span, tok)) => (span, tok),
            None => return Self::return_expr(lexer, collected),
        };

        let (ty, kind) = match tok {
            Token::ILit(i) => (Type::I32, ExprKind::ILit(i)),
            Token::FLit(f) => (Type::F64, ExprKind::FLit(f)),
            Token::StrLit(s) => (Type::Str, ExprKind::StrLit(s)),
            Token::BoolLit(b) => (Type::Bool, ExprKind::BoolLit(b)),
            _ => todo!(),
        };

        collected.push(Expr {
            kind,
            ty: Some(ty),
            span,
        });

        Self::parse_expr(lexer, collected)
    }
}
