use std::{fmt::Display, num::NonZero};

use crate::{Spanned, program::env::Env};

#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
pub enum Ty {
    Int { signed: bool, width: NonZero<u32> },
    F32,
    F64,
    Isize,
    Usize,
    // F128,
    // Arrow(Box<Ty>, Box<Ty>), // X -> Y
    Unit,
    Unknown, // Marker for typer
    IntLit,  // Unresolved integer literal — resolved to a concrete Int type by the typer
}

impl Ty {
    pub const fn is_signed(self) -> bool {
        matches!(self, Self::Int { signed: true, .. } | Self::Isize)
    }

    pub const fn is_llvm_int(self) -> bool {
        matches!(self, Self::Int { .. } | Self::Isize | Self::Usize)
    }
}

impl TryFrom<&str> for Ty {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value
        {
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            "isize" => Ok(Self::Isize),
            "usize" => Ok(Self::Usize),
            ty_str if ty_str.starts_with(['i', 'u']) =>
            {
                let width: NonZero<u32> = ty_str[1..].parse().map_err(|_| ())?;
                if width.get() > 128
                {
                    return Err(());
                }
                Ok(Self::Int {
                    signed: ty_str.starts_with('i'),
                    width,
                })
            },
            _ => Err(()),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Self::Int {
                signed: true,
                width,
            } => f.write_fmt(format_args!("i{width}")),
            Self::Int {
                signed: false,
                width,
            } => f.write_fmt(format_args!("u{width}")),
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::Unit => f.write_str("()"),
            Self::Unknown => f.write_str("?"),
            Self::Isize => f.write_str("isize"),
            Self::Usize => f.write_str("usize"),
            Self::IntLit => f.write_str("{int}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Declaration {
        kind: DeclKind,
        name: &'src str,
        ty:   Ty,
        expr: Box<Spanned<Self>>,
    },
    IntLit {
        ty:    Ty,
        value: u128,
    },
    F64(f64),
    // F128(f128)

    // Ex: `u32 42` would turn into `Cast(U32, IntLit { ty: U32, value: 42 })`
    Cast(Ty, Box<Spanned<Self>>),
    Ident {
        name: &'src str,
        ty:   Ty,
    },
}

impl<'src> Expr<'src> {
    pub const fn kind_name(&self) -> &'static str {
        match self
        {
            Self::Declaration { .. } => "declaration",
            Self::IntLit { .. } => "int literal",
            Self::F64(_) => "f64 litteral",
            Self::Cast(_, _) => "cast expression",
            Self::Ident { .. } => "identifier",
        }
    }

    pub fn deps(&self) -> Vec<&'src str> {
        let mut out = Vec::new();
        self.collect_deps(&mut out);
        out.sort_unstable();
        out.dedup();
        out
    }

    fn collect_deps(&self, out: &mut Vec<&'src str>) {
        match self
        {
            Self::Ident { name, .. } => out.push(name),
            Self::Cast(_, inner) => inner.0.collect_deps(out),
            Self::IntLit { .. } | Self::F64(_) =>
            {},
            Self::Declaration { .. } => unreachable!("nested declaration in dep collection"),
        }
    }

    #[allow(clippy::match_same_arms)]
    pub fn const_value<'a>(&'a self, env: &'a mut Env<'src>) -> Option<ConstValue> {
        match self
        {
            Self::Declaration { name, expr, .. } =>
            {
                let value = expr.as_ref().0.const_value(env)?;
                env.declare_const(name, value);
                Some(value)
            },
            Self::IntLit { ty, value } => match ty
            {
                Ty::Isize | Ty::Int { signed: true, .. } =>
                {
                    Some(ConstValue::Int(i128::try_from(*value).expect(
                        "Should not happend. This should be caught by earlier stage of the typer",
                    )))
                },
                Ty::Usize | Ty::Int { signed: false, .. } => Some(ConstValue::Uint(*value)),
                Ty::F32 | Ty::F64 | Ty::Unit | Ty::Unknown | Ty::IntLit =>
                {
                    unreachable!("Should not be able to pass the typer")
                },
            },
            Self::F64(val) => Some(ConstValue::Float(*val)),
            Self::Cast(_ty, _expr) => None, // TODO: Handle cast in const expr
            Self::Ident { name, .. } => env.lookup_const(name).copied(),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Expr::Declaration {
                kind,
                name,
                ty,
                expr,
            } => f.write_fmt(format_args!("{kind} {name}: {ty} = ({})", expr.0)),
            Expr::IntLit { value, .. } => f.write_fmt(format_args!("{value}")),
            Expr::F64(lit) => f.write_fmt(format_args!("f{lit}")),
            Expr::Cast(ty, expr) => f.write_fmt(format_args!("{ty} ({})", expr.0)),
            Expr::Ident { name, ty } => f.write_fmt(format_args!("{name}: {ty}")),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DeclKind {
    Const,
    Let { is_mut: bool },
}

impl Display for DeclKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Self::Const => f.write_str("const"),
            Self::Let { is_mut: false } => f.write_str("let"),
            Self::Let { is_mut: true } => f.write_str("let mut"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConstValue {
    Uint(u128),
    Int(i128),
    Float(f64),
}
