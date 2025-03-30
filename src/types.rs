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
    pub fn size(&self) -> usize {
        return 8;
    }
    pub fn pointer(&self) -> Option<Type> {
        match self {
            Type::Pointer(p) | Type::Array(p) => Some(*p.clone()),
            _ => None
        }
    }
}