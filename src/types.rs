#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    String,
    Pointer(Box<Type>),
    Array { inner: Box<Type>, length: usize },
    Struct {names: Vec<String>, types: Vec<Type>}
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Struct { names: _, types } => types.iter().map(|ty| ty.size()).sum(),
            Type::Array { inner, length } => *length as u32 * inner.size(),
            _ => 8
        }
    }
    pub fn pointer(&self) -> Option<Type> {
        match self {
            Type::Pointer(p) | Type::Array { inner: p, length: _ } => Some(*p.clone()),
            _ => None
        }
    }
    pub fn stack(&self) -> Option<Type> {
        match self {
            Type::Array { inner: p, length: _ } => Some(*p.clone()),
            _ => None
        }
    }
    pub fn salloc(&self) -> bool {
        match self {
            Type::Array { inner: _, length: _ } | Type::Struct { names: _, types: _ } => true,
            _ => false
        }
    }
}