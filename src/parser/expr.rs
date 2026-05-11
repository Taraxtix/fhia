use std::fmt::Display;

use crate::Spanned;

#[derive(PartialEq, Eq, Clone, Debug, Copy, Hash)]
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
    Isize,
    Usize,
    // F128,
    // Arrow(Box<Ty>, Box<Ty>), // X -> Y
    Unit,
    Unknown, // Marker for typer
}

impl Ty {
    pub const fn is_signed(self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128 | Self::Isize
        )
    }

    pub const fn is_llvm_int(self) -> bool {
        matches!(
            self,
            Self::I8
                | Self::I16
                | Self::I32
                | Self::I64
                | Self::I128
                | Self::Isize
                | Self::U8
                | Self::U16
                | Self::U32
                | Self::U64
                | Self::U128
                | Self::Usize
        )
    }
}

impl TryFrom<&str> for Ty {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value
        {
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "i128" => Ok(Self::I128),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "u128" => Ok(Self::U128),
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            "isize" => Ok(Self::Isize),
            "usize" => Ok(Self::Usize),
            _ => Err(()),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Self::I8 => f.write_str("i8"),
            Self::I16 => f.write_str("i16"),
            Self::I32 => f.write_str("i32"),
            Self::I64 => f.write_str("i64"),
            Self::I128 => f.write_str("i128"),
            Self::U8 => f.write_str("u8"),
            Self::U16 => f.write_str("u16"),
            Self::U32 => f.write_str("u32"),
            Self::U64 => f.write_str("u64"),
            Self::U128 => f.write_str("u128"),
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::Unit => f.write_str("()"),
            Self::Unknown => f.write_str("?"),
            Self::Isize => f.write_str("isize"),
            Self::Usize => f.write_str("usize"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Declaration {
        name: &'src str,
        ty:   Ty,
        expr: Box<Spanned<Self>>,
    },
    // I32(i32),
    I64(i64),
    // I128(i128),
    F64(f64),
    // F128(f128)

    // Ex: `u32 42` would turn into `Cast(U32, I32(42))`
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
            Self::I64(_) => "i64 litteral",
            Self::F64(_) => "f64 litteral",
            Self::Cast(_, _) => "cast expression",
            Self::Ident { .. } => "identifier",
        }
    }

    pub fn deps(&self) -> Vec<&'src str> {
        let mut out = Vec::new();
        self.collect_deps(&mut out);
        out
    }

    fn collect_deps(&self, out: &mut Vec<&'src str>) {
        match self
        {
            Self::Ident { name, .. } => out.push(name),
            Self::Cast(_, inner) => inner.0.collect_deps(out),
            Self::I64(_) | Self::F64(_) =>
            {},
            Self::Declaration { .. } => unreachable!("nested declaration in dep collection"),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
        {
            Expr::Declaration { name, ty, expr } =>
            {
                f.write_fmt(format_args!("{name}: {ty} = ({})", expr.0))
            },
            Expr::I64(lit) => f.write_fmt(format_args!("{lit}")),
            Expr::F64(lit) => f.write_fmt(format_args!("f{lit}")),
            Expr::Cast(ty, expr) => f.write_fmt(format_args!("{ty} ({})", expr.0)),
            Expr::Ident { name, ty } => f.write_fmt(format_args!("{name}: {ty}")),
        }
    }
}
