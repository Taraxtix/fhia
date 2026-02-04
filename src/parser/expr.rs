use std::fmt::Display;

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
    // F128,
    Arrow(Box<Ty>, Box<Ty>), // X -> Y
    Unknown,                 // Marker for typer
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
            Ty::Arrow(ty, ty1) => f.write_fmt(format_args!("{} -> {}", ty, ty1)),
            Ty::Unknown => f.write_str("?"),
        }
    }
}

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
    // Ident {
    //     name: &'src str,
    //     ty: Ty,
    // },
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Declaration { name, ty, expr } => {
                f.write_fmt(format_args!("{name}: {ty} = ({expr})"))
            }
            Expr::I64(lit) => f.write_fmt(format_args!("{lit}")),
            Expr::F64(lit) => f.write_fmt(format_args!("{lit}")),
            Expr::Cast(ty, expr) => f.write_fmt(format_args!("{ty} ({expr})")),
        }
    }
}
