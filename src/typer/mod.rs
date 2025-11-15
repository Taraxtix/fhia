use crate::Error;
use crate::lexer::Span;
use crate::parser::{BinOp, Expr, ExprKind, Parser, Pattern, Ty};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Decla {
        name: String,
        ty: Ty,
        expr: Box<TypedExprKind>,
    },
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F64(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Application(Box<TypedExpr>, Box<TypedExpr>),
    Ident(String, Ty),
}

impl TypedExprKind {
    pub fn ty(&self) -> Ty {
        match self {
            TypedExprKind::Decla { ty, .. } => ty.clone(),
            TypedExprKind::U32(_) => Ty::U32,
            TypedExprKind::I8(_) => Ty::I8,
            TypedExprKind::I16(_) => Ty::I16,
            TypedExprKind::I32(_) => Ty::I32,
            TypedExprKind::I64(_) => Ty::I64,
            TypedExprKind::I128(_) => Ty::I128,
            TypedExprKind::U8(_) => Ty::U8,
            TypedExprKind::U16(_) => Ty::U16,
            TypedExprKind::U64(_) => Ty::U64,
            TypedExprKind::U128(_) => Ty::U128,
            TypedExprKind::F64(_) => Ty::F64,
            TypedExprKind::Bool(_) => Ty::Bool,
            TypedExprKind::Str(_) => Ty::Str,
            TypedExprKind::Char(_) => Ty::Char,
            TypedExprKind::Application(func, _) => match func.ty() {
                Ty::Arrow { param: _, ret } => *ret.clone(),
                _ => unreachable!(),
            },
            TypedExprKind::Ident(_, ty) => ty.clone(),
        }
    }
}

impl Display for TypedExprKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExprKind::Decla { name, ty, expr } => {
                f.write_fmt(format_args!("{}: {} =\n\t{}", name, ty, expr))
            }
            TypedExprKind::I8(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::I16(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::I32(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::I64(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::I128(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::U8(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::U16(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::U32(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::U64(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::U128(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::F64(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::Bool(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::Str(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::Char(val) => f.write_fmt(format_args!("{val}")),
            TypedExprKind::Application(func, arg) => f.write_fmt(format_args!("{func}({arg})")),
            TypedExprKind::Ident(name, _) => f.write_fmt(format_args!("{name}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    filename: String,
    span: Span,
    kind: TypedExprKind,
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl TypedExpr {
    pub fn ty(&self) -> Ty {
        self.kind.ty()
    }

    pub fn check_and_convert(self, should_match: &Ty) -> Result<Self, Error> {
        macro_rules! lit_convert {
            ($kind:expr, $from:ty, $to:ty, $val:expr) => {
                $kind(if $val > <$to>::MIN as $from && $val < <$to>::MAX as $from {
                    $val.try_into().unwrap()
                } else {
                    return Err(Error {
                        prefix: "TYPING ERROR",
                        filename: self.filename,
                        span: self.span,
                        message: format!(
                            "Cannot convert {} to {} because value is out of bound of target type",
                            self.kind.ty(),
                            should_match
                        ),
                    });
                })
            };
            ($to_kind:expr, $to:ty, $to_str_repr:expr) => {
                match self.kind {
                    TypedExprKind::Decla { .. } => {
                        unreachable!("check_and_convert should not be called on decla")
                    }
                    TypedExprKind::I8(val) => lit_convert!(TypedExprKind::I8, i8, $to, val),
                    TypedExprKind::I16(val) => lit_convert!(TypedExprKind::I16, i16, $to, val),
                    TypedExprKind::I32(val) => lit_convert!(TypedExprKind::I32, i32, $to, val),
                    TypedExprKind::I64(val) => lit_convert!(TypedExprKind::I64, i64, $to, val),
                    TypedExprKind::I128(val) => lit_convert!(TypedExprKind::I128, i128, $to, val),
                    TypedExprKind::U8(val) => lit_convert!(TypedExprKind::U8, u8, $to, val),
                    TypedExprKind::U16(val) => lit_convert!(TypedExprKind::U16, u16, $to, val),
                    TypedExprKind::U32(val) => lit_convert!(TypedExprKind::U32, u32, $to, val),
                    TypedExprKind::U64(val) => lit_convert!(TypedExprKind::U64, u64, $to, val),
                    TypedExprKind::U128(val) => lit_convert!(TypedExprKind::U128, u128, $to, val),
                    TypedExprKind::F64(val) => lit_convert!(TypedExprKind::F64, f64, $to, val),
                    TypedExprKind::Bool(val) => $to_kind(if val { 1 } else { 0 } as $to),
                    TypedExprKind::Str(_) => {return Err(Error {
                        prefix: "TYPING ERROR",
                        filename: self.filename,
                        span: self.span,
                        message: format!("Expected type `{}` but got `str`", $to_str_repr),
                    })},
                    TypedExprKind::Char(c) => $to_kind(c as u32 as $to),
                    TypedExprKind::Application(_, _) => {return Err(Error {
                        prefix: "TYPING ERROR",
                        filename: self.filename,
                        span: self.span,
                        message: format!("Expected type `{}` but got a function call", $to_str_repr),
                    })},
                    TypedExprKind::Ident(name, ty) => {
                        if ty == *should_match {
                            TypedExprKind::Ident(name, ty)
                        } else {return Err(Error {
                        prefix: "TYPING ERROR",
                        filename: self.filename,
                        span: self.span,
                        message: format!("Expected type `{}` but got identifier {name} of type {ty}", $to_str_repr),
                    })}
                    }
                }
            };
        }
        // unreachable!("check_and_convert should not be called on decla"),
        Ok(Self {
            kind: match should_match {
                Ty::I8 => lit_convert!(TypedExprKind::I8, i8, "i8"),
                Ty::I16 => lit_convert!(TypedExprKind::I16, i16, "i16"),
                Ty::I32 => lit_convert!(TypedExprKind::I32, i32, "i32"),
                Ty::I64 => lit_convert!(TypedExprKind::I64, i64, "i64"),
                Ty::I128 => lit_convert!(TypedExprKind::I128, i128, "i128"),
                Ty::U8 => lit_convert!(TypedExprKind::U8, u8, "u8"),
                Ty::U16 => lit_convert!(TypedExprKind::U16, u16, "u16"),
                Ty::U32 => lit_convert!(TypedExprKind::U32, u32, "u32"),
                Ty::U64 => lit_convert!(TypedExprKind::U64, u64, "u64"),
                Ty::U128 => lit_convert!(TypedExprKind::U128, u128, "u128"),
                Ty::F32 => lit_convert!(TypedExprKind::F64, f64, "f64"),
                Ty::F64 => lit_convert!(TypedExprKind::F64, f64, "f64"),
                Ty::F128 => todo!(),
                Ty::Usize => todo!(),
                Ty::Isize => todo!(),
                Ty::Char => todo!(),
                Ty::Str => todo!(),
                Ty::ConstPtr(_) => todo!(),
                Ty::MutPtr(_) => todo!(),
                Ty::ConstRef(_) => todo!(),
                Ty::MutRef(_) => todo!(),
                Ty::Slice(_) => todo!(),
                Ty::Arr { .. } => todo!(),
                Ty::Unknown => self.kind,
                Ty::Bool => todo!(),
                Ty::Unit => todo!(),
                Ty::Mut(_) => todo!(),
                Ty::Arrow { .. } => todo!(),
                Ty::Never => todo!(),
            },
            filename: self.filename,
            span: self.span,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    raw_name: String,
    idx: usize,
    ty: Ty,
}

type Scope = Vec<HashMap<String, Symbol>>;

pub(crate) struct Typer<'a> {
    expressions: Vec<TypedExpr>,
    parser: Parser<'a>,
}

impl Iterator for Typer<'_> {
    type Item = TypedExpr;

    fn next(&mut self) -> Option<Self::Item> {
        if self.expressions.is_empty() {
            return None;
        }
        self.expressions.rotate_left(1);
        self.expressions.pop()
    }
}

impl<'a> Typer<'a> {
    fn check(mut self, mut scope: Scope) -> Result<Self, Error> {
        let Some(Expr {
            filename,
            span,
            kind,
        }) = self.parser.next()
        else {
            return Ok(self);
        };

        scope.push(HashMap::new());

        match kind {
            ExprKind::Decla { name, ty, expr } => {
                let (decla_scope, ret_ty) = Self::new_scope_from_ty(ty, &scope, name.clone());
                let typed = expr.type_check(ret_ty, decla_scope)?;
                let map = scope.last_mut().unwrap();
                map.insert(
                    name.clone(),
                    Symbol {
                        raw_name: name,
                        idx: map.len(),
                        ty: typed.ty(),
                    },
                );
                self.expressions.push(typed);
                self.check(scope)
            }
            _ => Err(Error {
                prefix: "ERROR",
                filename,
                span,
                message: format!("Expected declaration at top level but got {kind} instead"),
            }),
        }
    }

    fn new_scope_from_ty(ty: Ty, previous: &Scope, parent_prefix: String) -> (Scope, Ty) {
        let mut scope = previous.clone();
        let new = HashMap::new();
        scope.push(new);
        match ty {
            Ty::Arrow { .. } => {
                let ret_ty = Self::aggregate_args(ty, scope.last_mut().unwrap(), parent_prefix);
                (scope, ret_ty)
            }
            _ => (scope, ty),
        }
    }

    fn aggregate_args(ty: Ty, map: &mut HashMap<String, Symbol>, parent_prefix: String) -> Ty {
        let Ty::Arrow { param, ret } = ty else {
            unreachable!()
        };
        match *param {
            Pattern::Typed { name, ty } => {
                map.insert(
                    name.clone(),
                    Symbol {
                        raw_name: format!("{parent_prefix}#{name}"),
                        idx: map.len(),
                        ty,
                    },
                );
            }
            Pattern::NamedWildcard(name) => {
                map.insert(
                    name.clone(),
                    Symbol {
                        raw_name: format!("{parent_prefix}#{name}"),
                        idx: map.len(),
                        ty: Ty::Unknown,
                    },
                );
            }
            Pattern::Wildcard => {}
        };

        match *ret {
            Ty::Arrow { param, ret } => {
                Self::aggregate_args(Ty::Arrow { param, ret }, map, parent_prefix)
            }
            ty => ty,
        }
    }

    fn push(mut self, expr: TypedExpr) -> Self {
        self.expressions.push(expr);
        self
    }
}

impl<'a> Parser<'a> {
    pub fn check(self) -> Result<Typer<'a>, Error> {
        Typer {
            expressions: vec![],
            parser: self,
        }
        .check(vec![])
    }
}

impl BinOp {
    pub fn type_check(self, lhs: Expr, rhs: Expr, scope: Scope) -> Result<TypedExprKind, Error> {
        let lhs = lhs.type_check(Ty::Unknown, scope.clone())?;
        let rhs = rhs.type_check(Ty::Unknown, scope.clone())?;
        match self {
            BinOp::Plus => match (lhs.ty(), rhs.ty()) {
                _ => todo!(),
            },
            BinOp::Minus => todo!(),
            BinOp::Times => todo!(),
            BinOp::Divide => todo!(),
            BinOp::Power => todo!(),
            BinOp::Modulo => todo!(),
            BinOp::BAnd => todo!(),
            BinOp::LAnd => todo!(),
            BinOp::BOr => todo!(),
            BinOp::LOr => todo!(),
            BinOp::Xor => todo!(),
            BinOp::LShift => todo!(),
            BinOp::RShift => todo!(),
            BinOp::Equal => todo!(),
            BinOp::NEqual => todo!(),
            BinOp::Gt => todo!(),
            BinOp::GEq => todo!(),
            BinOp::Lt => todo!(),
            BinOp::LEq => todo!(),
            BinOp::Assign => todo!(),
            BinOp::PlusAssign => todo!(),
            BinOp::MinusAssign => todo!(),
            BinOp::TimesAssign => todo!(),
            BinOp::DivideAssign => todo!(),
            BinOp::ModuloAssign => todo!(),
            BinOp::AndAssign => todo!(),
            BinOp::OrAssign => todo!(),
            BinOp::XorAssign => todo!(),
            BinOp::LShiftAssign => todo!(),
            BinOp::RShiftAssign => todo!(),
            BinOp::Dot => todo!(),
        }
    }

    fn allowed_lhs(&self) -> Vec<Ty> {
        match self {
            BinOp::Plus | BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Power => vec![
                Ty::I8,
                Ty::I16,
                Ty::I32,
                Ty::I64,
                Ty::I128,
                Ty::U8,
                Ty::U16,
                Ty::U32,
                Ty::U64,
                Ty::U128,
                Ty::F32,
                Ty::F64,
                Ty::F128,
                Ty::MutRef(Box::new(Ty::Unknown)),
            ],
            BinOp::Modulo => vec![Ty::I32, Ty::I64, Ty::I128, Ty::U32, Ty::U64, Ty::U128],
            BinOp::BAnd | BinOp::LAnd | BinOp::BOr | BinOp::LOr | BinOp::Xor => vec![
                Ty::I8,
                Ty::I16,
                Ty::I32,
                Ty::I64,
                Ty::I128,
                Ty::U8,
                Ty::U16,
                Ty::U32,
                Ty::U64,
                Ty::U128,
            ],
            BinOp::LShift => todo!(),
            BinOp::RShift => todo!(),
            BinOp::Equal => todo!(),
            BinOp::NEqual => todo!(),
            BinOp::Gt => todo!(),
            BinOp::GEq => todo!(),
            BinOp::Lt => todo!(),
            BinOp::LEq => todo!(),
            BinOp::Assign => todo!(),
            BinOp::PlusAssign => todo!(),
            BinOp::MinusAssign => todo!(),
            BinOp::TimesAssign => todo!(),
            BinOp::DivideAssign => todo!(),
            BinOp::ModuloAssign => todo!(),
            BinOp::AndAssign => todo!(),
            BinOp::OrAssign => todo!(),
            BinOp::XorAssign => todo!(),
            BinOp::LShiftAssign => todo!(),
            BinOp::RShiftAssign => todo!(),
            BinOp::Dot => todo!(),
        }
    }
}

impl Expr {
    pub fn type_check(self, should_match: Ty, scope: Scope) -> Result<TypedExpr, Error> {
        let Expr {
            filename,
            span,
            kind,
        } = self.clone();

        Ok(TypedExpr {
            kind: match kind {
                ExprKind::Decla { .. } => todo!("TypeCheck nested declaration"),
                ExprKind::Ident(name) => {
                    println!("Typing the identifier {name} which should match {should_match}");
                    let symbol = self.find_in_scope(&scope, &name)?;
                    println!("\tFound symbol: {symbol:?}");
                    TypedExprKind::Ident(name, symbol.ty)
                }
                ExprKind::U32(val) => TypedExprKind::U32(val),
                ExprKind::U64(val) => TypedExprKind::U64(val),
                ExprKind::U128(val) => TypedExprKind::U128(val),
                ExprKind::F64(val) => TypedExprKind::F64(val),
                ExprKind::Bool(val) => TypedExprKind::Bool(val),
                ExprKind::Str(val) => TypedExprKind::Str(val),
                ExprKind::Char(val) => TypedExprKind::Char(val),
                ExprKind::Sequence { .. } => todo!(),
                ExprKind::Unit(_) => todo!(),
                ExprKind::BinOp { op, lhs, rhs } => op.type_check(*lhs, *rhs, scope.clone())?,
                ExprKind::Paren(expr) => {
                    return expr.type_check(should_match.clone(), scope);
                }
                ExprKind::Application(func, arg) => {
                    println!(
                        "Typing an Application of {func} by {arg} which should match {should_match}"
                    );
                    let arg_typed = arg.type_check(Ty::Unknown, scope.clone())?;
                    println!("\targ_type is {}", arg_typed.ty());
                    let func_should_match =
                        Self::update_application_type(arg_typed.ty(), &should_match);
                    let func_typed = func.type_check(func_should_match, scope)?;
                    TypedExprKind::Application(func_typed.into(), arg_typed.into())
                }
            },
            filename,
            span,
        }
        .check_and_convert(&should_match)?)
    }

    fn update_application_type(arg_type: Ty, should_match: &Ty) -> Ty {
        Ty::Arrow {
            param: Box::new(Pattern::Typed {
                ty: arg_type,
                name: "".to_string(),
            }),
            ret: Box::new(should_match.clone()),
        }
    }

    fn find_in_scope(&self, scope: &Scope, name: &String) -> Result<Symbol, Error> {
        for local_scope in scope {
            if let Some(symbol) = local_scope.get(name) {
                return Ok(symbol.clone());
            }
        }
        Err(Error {
            prefix: "ERROR",
            filename: self.filename.clone(),
            span: self.span,
            message: format!("Cannot find {name} in scope"),
        })
    }
}
