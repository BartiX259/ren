#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    String,
    Pointer(Box<Type>),
    Array(Box<Type>),
    Struct {names: Vec<String>, types: Vec<Type>}
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Struct { names: _, types } => types.iter().map(|ty| ty.size()).sum(),
            _ => 8
        }
    }
    pub fn pointer(&self) -> Option<Type> {
        match self {
            Type::Pointer(p) | Type::Array(p) => Some(*p.clone()),
            _ => None
        }
    }
    pub fn stack(&self) -> Option<Type> {
        match self {
            Type::Array(p) => Some(*p.clone()),
            _ => None
        }
    }
    pub fn salloc(&self) -> bool {
        match self {
            Type::Array(_) | Type::Struct { names: _, types: _ } => true,
            _ => false
        }
    }
}