use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Char,
    String,
    Pointer(Box<Type>),
    Array { inner: Box<Type>, length: usize },
    TaggedArray { inner: Box<Type> },
    Struct(HashMap<String, (Type, u32)>),
    Tuple(Vec<Type>)
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Struct(map) => map.iter().map(|(_, (ty, _))| ty.size()).sum(),
            Type::Array { inner, length } => *length as u32 * inner.size(),
            Type::Tuple(tys) => tys.iter().map(|ty| ty.size()).sum(),
            Type::String | Type::TaggedArray { .. } => 16,
            Type::Char | Type::Bool => 1,
            _ => 8
        }
    }
    pub fn aligned_size(&self) -> u32 {
        Self::align(self.size())
    }
    pub fn align(size: u32) -> u32 {
        (size + 7) & !7  // round up to the next multiple of 8
    }
    pub fn dereference(&self) -> Option<Type> {
        match self {
            Type::Pointer(p) | Type::Array { inner: p, length: _ } => Some(*p.clone()),
            _ => None
        }
    }
    pub fn address_of(&self) -> Option<Type> {
        match self {
            Type::Array { inner: p, length: _ } => Some(*p.clone()),
            _ => None
        }
    }
    pub fn salloc(&self) -> bool {
        match self {
            Type::Array { .. } | Type::Struct(_) | Type::String | Type::Tuple(_) | Type::TaggedArray { .. } => true,
            _ => false
        }
    }
}