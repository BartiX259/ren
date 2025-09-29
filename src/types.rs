use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    Char,
    Range,
    Any,
    Generic(String),
    Pointer(Box<Type>),
    Array { inner: Box<Type>, length: usize },
    List { inner: Box<Type> },
    Slice { inner: Box<Type> },
    Struct(HashMap<String, (Type, u32)>),
    Tuple(Vec<Type>),
    Enum(Vec<(String, Option<Type>)>),
    Result(Box<Type>, Box<Type>),
    Option(Box<Type>),
    HashMap { key: Box<Type>, value: Box<Type> },
    Fn { arg_types: Vec<Type>, ret: Box<Type> }
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::Struct(map) => map.iter().map(|(_, (ty, _))| ty.aligned_size()).sum(),
            Type::Array { inner, length } => *length as u32 * inner.size(),
            Type::Tuple(tys) => tys.iter().map(|ty| ty.aligned_size()).sum(),
            Type::Result(ty, err) => ty.size().max(err.size()) + 8,
            Type::Option(ty) => ty.size() + 8,
            Type::Range | Type::Slice { .. } | Type::List { .. } => 16,
            Type::HashMap { .. } => 8,
            Type::Enum(variants) => {
                let max_variant_size = variants.iter().map(|(_, opt_ty)| opt_ty.as_ref().map_or(0, |ty| ty.size())).max().unwrap_or(0); // In case Enum is empty
                let tag_bits = (variants.len() as f64).log2().ceil() as u32;
                let tag_size = ((tag_bits + 7) / 8).max(1); // At least 1 byte
                max_variant_size + tag_size
            }
            Type::Int | Type::Float | Type::Pointer(_) | Type::Fn { .. } => 8,
            Type::Char | Type::Bool | Type::Any | Type::Void => 1,
            Type::Generic(_) => panic!("Generic size called ({self})"),
        }
    }
    pub fn aligned_size(&self) -> u32 {
        Self::align(self.size())
    }
    pub fn align(size: u32) -> u32 {
        (size + 7) & !7 // round up to the next multiple of 8
    }
    pub fn dereference(&self) -> Option<Type> {
        match self {
            Type::Pointer(p) | Type::Array { inner: p, .. } => Some(*p.clone()),
            Type::Range => Some(Type::Int),
            _ => None,
        }
    }
    // pub fn address_of(&self) -> Option<Type> {
    //     match self {
    //         Type::Array { inner: p, length: _ } => Some(*p.clone()),
    //         _ => None,
    //     }
    // }
    pub fn inner(&self, max_depth: usize) -> &Type {
        if max_depth == 0 {
            return self;
        }
        match self {
            Type::Pointer(p) | Type::Array { inner: p, .. } | Type::List { inner: p } | Type::Slice { inner: p } | Type::Option(p) => p.inner(max_depth - 1),
            _ => self,
        }
    }
    pub fn salloc(&self) -> bool {
        match self {
            Type::Array { .. } | Type::Struct(_) | Type::Tuple(_) | Type::Slice { .. } | Type::List { .. } | Type::Range => true,
            _ => false,
        }
    }
    pub fn match_generics(&self, expected: &Type, generics: &mut HashMap<String, Type>) -> Result<(), String> {
        match (self, expected) {
            (Type::Generic(name), other) => {
                match generics.get(name) {
                    Some(existing) => {
                        if existing != other {
                            return Err(format!("Conflicting generic binding for {}: {} vs {}", name, existing, other));
                        }
                    }
                    None => {
                        generics.insert(name.clone(), other.clone());
                    }
                }
                Ok(())
            }
            (Type::Pointer(a), Type::Pointer(b)) | (Type::Option(a), Type::Option(b)) | (Type::List { inner: a }, Type::List { inner: b }) | (Type::Slice { inner: a }, Type::Slice { inner: b }) => {
                a.match_generics(b, generics)
            }
            (Type::Array { inner: a, length: len1 }, Type::Array { inner: b, length: len2 }) => {
                if len1 != len2 {
                    return Err("Array lengths differ".to_string());
                }
                a.match_generics(b, generics)
            }
            (Type::Struct(a_fields), Type::Struct(b_fields)) => {
                if a_fields.len() != b_fields.len() {
                    return Err("Struct field count mismatch".to_string());
                }
                for (name, (a_ty, _)) in a_fields {
                    let Some((b_ty, _)) = b_fields.get(name) else {
                        return Err(format!("Struct field name mismatch: missing field {}", name));
                    };
                    a_ty.match_generics(b_ty, generics)?;
                }
                Ok(())
            }
            (Type::Tuple(a), Type::Tuple(b)) => {
                if a.len() != b.len() {
                    return Err("Tuple lengths differ".to_string());
                }
                for (x, y) in a.iter().zip(b.iter()) {
                    x.match_generics(y, generics)?;
                }
                Ok(())
            }
            (Type::Result(ok1, err1), Type::Result(ok2, err2)) => {
                ok1.match_generics(ok2, generics)?;
                err1.match_generics(err2, generics)
            }
            (Type::HashMap { key: k1, value: v1 }, Type::HashMap { key: k2, value: v2 }) => {
                k1.match_generics(k2, generics)?;
                v1.match_generics(v2, generics)
            }
            (Type::Fn { arg_types: a_args, ret: a_ret }, Type::Fn { arg_types: b_args, ret: b_ret }) => {
                if a_args.len() != b_args.len() {
                    return Err("Function argument count mismatch".to_string());
                }
                for (a_arg, b_arg) in a_args.iter().zip(b_args.iter()) {
                    a_arg.match_generics(b_arg, generics)?;
                }
                a_ret.match_generics(b_ret, generics)
            }
            _ => {
                if self == expected {
                    Ok(())
                } else {
                    Err(format!("Type mismatch: {} vs {}", self, expected))
                }
            }
        }
    }
    pub fn substitute_generics(&self, generics: &HashMap<String, Type>) -> Type {
        match self {
            Type::Generic(s) => generics.get(s).cloned().unwrap_or_else(|| self.clone()),
            Type::Pointer(inner) => Type::Pointer(Box::new(inner.substitute_generics(generics))),
            Type::Option(inner) => Type::Option(Box::new(inner.substitute_generics(generics))),
            Type::List { inner } => Type::List {
                inner: Box::new(inner.substitute_generics(generics)),
            },
            Type::Slice { inner } => Type::Slice {
                inner: Box::new(inner.substitute_generics(generics)),
            },
            Type::Array { inner, length } => Type::Array {
                inner: Box::new(inner.substitute_generics(generics)),
                length: *length,
            },
            Type::Tuple(items) => Type::Tuple(items.iter().map(|t| t.substitute_generics(generics)).collect()),
            Type::Result(ok, err) => Type::Result(Box::new(ok.substitute_generics(generics)), Box::new(err.substitute_generics(generics))),
            Type::HashMap { key, value } => Type::HashMap {
                key: Box::new(key.substitute_generics(generics)),
                value: Box::new(value.substitute_generics(generics)),
            },
            Type::Fn { arg_types, ret } => Type::Fn {
                arg_types: arg_types.iter().map(|t| t.substitute_generics(generics)).collect(),
                ret: Box::new(ret.substitute_generics(generics)),
            },
            Type::Struct(fields) => {
                let mut i = 0;
                let mut map: HashMap<String, (Type, u32)> = HashMap::new();
                let mut items: Vec<_> = fields.iter().collect();
                items.sort_by_key(|(_, (_, offset))| *offset);
                for (name, (ty, _)) in items {
                    let new_ty = ty.substitute_generics(generics);
                    if let Type::Generic(_) = new_ty {
                        return self.clone();
                    }
                    let add = new_ty.size();
                    map.insert(name.clone(), (new_ty, i));
                    i += add;
                }
                Type::Struct(map)
            }
            _ => self.clone(),
        }
    }
}

fn write_vec(f: &mut std::fmt::Formatter<'_>, tys: &Vec<Type>) -> std::fmt::Result {
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
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
            Type::Tuple(tys) => write_vec(f, tys),
            Type::Enum(_) => write!(f, "enum"),
            Type::Result(ty, err) => write!(f, "{ty} ? {err}"),
            Type::Option(ty) => write!(f, "?{ty}"),
            Type::HashMap { key, value } => write!(f, "{{{key}: {value}}}"),
            Type::Fn { arg_types, ret } => {
                write!(f, "fn")?;
                write_vec(f, arg_types)?;
                write!(f, " -> {ret}")
            }
        }
    }
}
