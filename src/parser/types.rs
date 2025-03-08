#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    // I128,
    U8,
    U16,
    U32,
    U64,
    // U128,
    Size,
    F32,
    F64,
    // F128,
    Str,
    Char,
    Bool,
    Unit,
    Never,
    Array { ty: Option<Box<Type>>, len: usize },
    ConstRef(Option<Box<Type>>),
    MutRef(Option<Box<Type>>),
    Any, // FIXME: Find another way to represent this
}

impl Type {
    pub fn c_ref(ty: Type) -> Self {
        Type::ConstRef(Some(Box::new(ty)))
    }
}

use std::fmt::Display;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            // Type::I128 => write!(f, "i128"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            // Type::U128 => write!(f, "u128"),
            Type::Size => write!(f, "size"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            // Type::F128 => write!(f, "f128"),
            Type::Str => write!(f, "string"),
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Never => write!(f, "!"),
            Type::Array { ty, len } => write!(
                f,
                "[{}; {len}]",
                ty.as_ref()
                    .map(|t| t.to_string())
                    .unwrap_or("any".to_string())
            ),
            Type::ConstRef(ty) => write!(
                f,
                "&const {}",
                ty.as_ref()
                    .map(|t| t.to_string())
                    .unwrap_or("any".to_string())
            ),
            Type::MutRef(ty) => write!(
                f,
                "&mut {}",
                ty.as_ref()
                    .map(|t| t.to_string())
                    .unwrap_or("any".to_string())
            ),
            Type::Any => write!(f, "any"),
        }
    }
}
