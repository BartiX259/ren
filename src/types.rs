use std::{collections::HashMap, fmt::Display};

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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::String => write!(f, "str"),
            Type::Pointer(p) => write!(f, "*{p}"),
            Type::Array { inner, length } => write!(f, "{inner}[{length}]"),
            Type::TaggedArray { inner } => write!(f, "{inner}[]"),
            Type::Struct(s) => {
                let mut sorted: Vec<_> = s.iter().collect();
                sorted.sort_by_key(|(_, (_, offset))| *offset);
                write!(f, "(")?;
                let mut start = true;
                for (name, (ty, _)) in sorted {
                    if !start {
                       write!(f, ", ")?; 
                    } else {
                        start = false;
                    }
                    write!(f, "{name}: {ty}")?;
                }
                write!(f, ")")
            }
            Type::Tuple(tys) => {
                write!(f, "(")?;
                let mut start = true;
                for ty in tys {
                    if !start {
                       write!(f, ", ")?; 
                    } else {
                        start = false;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }
        }
    }
}
