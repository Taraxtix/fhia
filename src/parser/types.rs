#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
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
    Size,
    F32,
    F64,
    F128,
    Str,
    Char,
    Bool,
    Unit,
    Never,
    Array { ty: Box<Type>, len: usize },
    ConstRef(Box<Type>),
    MutRef(Box<Type>),
    Any, // FIXME: Find another way to represent this
}

use std::fmt::Display;

use Type as T;

impl T {
    fn can_auto_cast(&self, to: &T) -> bool {
        match (self, to) {
            (from, to) if from == to => true,
            (T::Unit, _) => false,
            (T::Never, _) => false,
            (T::ConstRef(from_ref), T::ConstRef(to_ref)) if from_ref.can_auto_cast(&to_ref) => true,
            (T::MutRef(from_ref), T::MutRef(to_ref)) if from_ref.can_auto_cast(&to_ref) => true,
            (T::Array { ty: from_ref, .. }, T::Array { ty: to_ref, .. })
                if from_ref.can_auto_cast(&to_ref) =>
            {
                true
            }
            (T::I8, T::I16 | T::I32 | T::I64 | T::I128 | T::F32 | T::F64 | T::F128) => true,
            (
                T::U8,
                T::U16
                | T::U32
                | T::U64
                | T::U128
                | T::Size
                | T::I16
                | T::I32
                | T::I64
                | T::I128
                | T::F32
                | T::F64
                | T::F128,
            ) => true,
            (T::I16, T::I32 | T::I64 | T::I128) => true,
            (
                T::U16,
                T::U32
                | T::U64
                | T::U128
                | T::Size
                | T::I32
                | T::I64
                | T::I128
                | T::F32
                | T::F64
                | T::F128,
            ) => true,
            (T::I32, T::I64 | T::I128 | T::Size | T::F64 | T::F128) => true,
            (T::U32, T::U64 | T::U128 | T::Size | T::I64 | T::I128 | T::F64 | T::F128) => true,
            (T::I64, T::I128 | T::F128) => true,
            (T::U64, T::U128 | T::Size | T::I128 | T::F128) => true,
            (T::F32, T::F64 | T::F128) => true,
            (T::F64, T::F128) => true,
            (T::Size, T::U64 | T::U128 | T::I128) => true,
            (_, T::Any) => true,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::I128 => write!(f, "i128"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U128 => write!(f, "u128"),
            Type::Size => write!(f, "size"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::F128 => write!(f, "f128"),
            Type::Str => write!(f, "string"),
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Never => write!(f, "!"),
            Type::Array { ty, len } => write!(f, "[{ty}; {len}]"),
            Type::ConstRef(ty) => write!(f, "&const {ty}"),
            Type::MutRef(ty) => write!(f, "&mut {ty}"),
            Type::Any => write!(f, "any"),
        }
    }
}
