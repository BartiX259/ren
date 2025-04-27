use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Char,
    String,
    Range,
    Any,
    Generic(String),
    Pointer(Box<Type>),
    Array { inner: Box<Type>, length: usize },
    List { inner: Box<Type> },
    Slice { inner: Box<Type> },
    Struct(HashMap<String, (Type, u32)>),
    Tuple(Vec<Type>)
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Struct(map) => map.iter().map(|(_, (ty, _))| ty.size()).sum(),
            Type::Array { inner, length } => *length as u32 * inner.size(),
            Type::Tuple(tys) => tys.iter().map(|ty| ty.size()).sum(),
            Type::List { .. } => 24,
            Type::String | Type::Range | Type::Slice { .. } => 16,
            Type::Int | Type::Float | Type::Pointer(_) => 8,
            Type::Char | Type::Bool | Type::Any | Type::Void => 1,
            Type::Generic(_) => panic!("Generic size called")
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
            Type::Pointer(p) | Type::Array { inner: p, .. } => Some(*p.clone()),
            _ => None
        }
    }
    pub fn address_of(&self) -> Option<Type> {
        match self {
            Type::Array { inner: p, length: _ } => Some(*p.clone()),
            _ => None
        }
    }
    pub fn inner(&self) -> &Type {
        match self {
            Type::Pointer(p) | Type::Array { inner: p, .. } | Type::List { inner: p } | Type::Slice { inner: p }  => p.inner(),
            _ => self
        }
    }
    pub fn inner_mut(&mut self) -> &mut Type {
        match self {
            Type::Pointer(p) | Type::Array { inner: p, .. } | Type::List { inner: p } | Type::Slice { inner: p }  => p.inner_mut(),
            _ => self
        }
    }
    pub fn wrap(inner: &Type, outer: &Type) -> Type {
        let mut new = outer.clone();
        *new.inner_mut() = inner.clone();
        new
    }
    pub fn salloc(&self) -> bool {
        match self {
            Type::Array { .. } | Type::Struct(_) | Type::String | Type::Tuple(_) | Type::Slice { .. } | Type::List { .. } => true,
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
            Type::Range => write!(f, "range"),
            Type::Any => write!(f, "any"),
            Type::Generic(s) => write!(f, "{s}"),
            Type::Pointer(p) => write!(f, "*{p}"),
            Type::Array { inner, length } => write!(f, "{inner}[{length}]"),
            Type::Slice { inner } => write!(f, "<{inner}>"),
            Type::List { inner } => write!(f, "[{inner}]"),
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
