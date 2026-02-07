use std::fmt::Display;

#[derive(PartialEq, Clone, Debug)]
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
    Unknown, // Marker for typer
}

impl TryFrom<&str> for Ty {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i8" => Ok(Ty::I8),
            "i16" => Ok(Ty::I16),
            "i32" => Ok(Ty::I32),
            "i64" => Ok(Ty::I64),
            "i128" => Ok(Ty::I128),
            "u8" => Ok(Ty::U8),
            "u16" => Ok(Ty::U16),
            "u32" => Ok(Ty::U32),
            "u64" => Ok(Ty::U64),
            "u128" => Ok(Ty::U128),
            "f32" => Ok(Ty::F32),
            "f64" => Ok(Ty::F64),
            "isize" => Ok(Ty::Isize),
            "usize" => Ok(Ty::Usize),
            _ => Err(()),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::I8 => f.write_str("i8"),
            Ty::I16 => f.write_str("i16"),
            Ty::I32 => f.write_str("i32"),
            Ty::I64 => f.write_str("i64"),
            Ty::I128 => f.write_str("i128"),
            Ty::U8 => f.write_str("u8"),
            Ty::U16 => f.write_str("u16"),
            Ty::U32 => f.write_str("u32"),
            Ty::U64 => f.write_str("u64"),
            Ty::U128 => f.write_str("u128"),
            Ty::F32 => f.write_str("f32"),
            Ty::F64 => f.write_str("f64"),
            Ty::Unknown => f.write_str("?"),
            Ty::Isize => f.write_str("isize"),
            Ty::Usize => f.write_str("usize"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Declaration {
        name: &'src str,
        ty: Ty,
        expr: Box<Expr<'src>>,
    },
    // I32(i32),
    I64(i64),
    // I128(i128),
    F64(f64),
    // F128(f128)

    // Ex: `u32 42` would turn into `Cast(U32, I32(42))`
    Cast(Ty, Box<Expr<'src>>),
    Ident {
        name: &'src str,
        ty: Ty,
    },
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Declaration { name, ty, expr } => {
                f.write_fmt(format_args!("{name}: {ty} = ({expr})"))
            }
            Expr::I64(lit) => f.write_fmt(format_args!("{lit}")),
            Expr::F64(lit) => f.write_fmt(format_args!("f{lit}")),
            Expr::Cast(ty, expr) => f.write_fmt(format_args!("{ty} ({expr})")),
            Expr::Ident { name, ty } => f.write_fmt(format_args!("{name}: {ty}")),
        }
    }
}
