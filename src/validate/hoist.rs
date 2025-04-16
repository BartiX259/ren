use super::{Validate, SemanticError};
use crate::node;
use crate::types::Type;
use crate::ir::{Symbol, Block};

impl Validate {
    pub fn hoist_type(&mut self, stmt: &mut node::Stmt, public: bool) -> Result<(), SemanticError> {
        if let node::Stmt::TypeDecl(decl) = stmt {
            if self.symbol_table.contains_key(&decl.name.str) {
                return Err(SemanticError::SymbolExists(decl.name.clone()));
            }
            let ty = self.r#type(&decl.r#type, false)?;
            self.symbol_table.insert(decl.name.str.clone(), Symbol::Type { ty });
        } else if let node::Stmt::Let(decl) = stmt {
            if self.symbol_table.contains_key(&decl.name.str) {
                return Err(SemanticError::SymbolExists(decl.name.clone()));
            }
            let str = self.const_expr(&decl.expr)?;
            let ty = self.expr(&mut decl.expr)?;
            self.symbol_table.insert(decl.name.str.clone(), Symbol::Data { ty, str });
        } else if let node::Stmt::Decl(decl) = stmt {
            if self.symbol_table.contains_key(&decl.name.str) {
                return Err(SemanticError::SymbolExists(decl.name.clone()));
            }
            let ty = self.r#type(&decl.r#type, false)?;
            let size = ((ty.size() + 7) / 8) as usize;
            let str = std::iter::repeat("0").take(size).collect::<Vec<_>>().join(", ");
            self.symbol_table.insert(decl.name.str.clone(), Symbol::Data { ty, str });
        }
        Ok(())
    }

    pub fn hoist_func(&mut self, stmt: &mut node::Stmt, public: bool) -> Result<(), SemanticError> {
        if let node::Stmt::Fn(decl) = stmt {
            let ty;
            if let Some(t) = &decl.decl_type {
                ty = self.r#type(t, false)?;
            } else {
                ty = Type::Void;
            }
            let mut arg_symbols: Vec<(String, Symbol)> = Vec::new();
            let mut types = Vec::new();
            for arg in decl.arg_names.iter().zip(decl.arg_types.iter()) {
                if self.symbol_table.contains_key(&arg.0.str) {
                    return Err(SemanticError::SymbolExists(arg.0.clone()));
                }
                for existing in arg_symbols.iter() {
                    if existing.0 == arg.0.str {
                        return Err(SemanticError::SymbolExists(arg.0.clone()));
                    }
                }
                let ty = self.r#type(arg.1, false)?;
                types.push(ty.clone());
                let sym = Symbol::Var { ty };
                arg_symbols.push((arg.0.str.clone(), sym.clone()));
            }
            if public {
                let s;
                if decl.name.str == "main" {
                    if self.symbol_table.contains_key("main") {
                        return Err(SemanticError::SymbolExists(decl.name.clone()));
                    }
                    s = "main".to_string();
                } else {
                    let len;
                    if let Some(sigs) = self.fn_map.get_mut(&decl.name.str) {
                        if sigs.iter().any(|(tys, _)| *tys == types) {
                            return Err(SemanticError::SymbolExists(decl.name.clone()));
                        }
                        len = sigs.len() + 1;
                        sigs.push((types, len));
                    } else {
                        self.fn_map.insert(decl.name.str.clone(), vec![(types, 1)]);
                        len = 1;
                    }
                    s = format!("{}.{}", decl.name.str, len);
                    decl.name.str = s.clone();
                }
                self.symbol_table.insert(s, Symbol::ExternFunc { 
                    ty,
                    args: arg_symbols.iter().map(|(_, sym)| { let Symbol::Var { ty } = sym.clone() else { unreachable!() }; ty }).collect()
                }); 
            } else {
                let split: Vec<&str> = decl.name.str.split('.').collect();
                if split.len() < 2 {
                    self.symbol_table.insert(decl.name.str.clone(), Symbol::Func { 
                        ty, module: self.cur_module.clone(),
                        block: Block::new(),
                        symbols: vec![arg_symbols],
                    });
                } else {
                    let id = split.last().unwrap().parse::<usize>().unwrap();
                    let name = split[..split.len() - 1].join(".");
                    if let Some(sigs) = self.fn_map.get_mut(&name) {
                        if sigs.iter().any(|(tys, _)| *tys == types) {
                            return Err(SemanticError::SymbolExists(decl.name.clone()));
                        }
                        sigs.push((types, id));
                    } else {
                        self.fn_map.insert(name, vec![(types, id)]);
                    }
                    self.symbol_table.insert(decl.name.str.clone(), Symbol::Func { 
                        ty, module: self.cur_module.clone(),
                        block: Block::new(),
                        symbols: vec![arg_symbols],
                    });
                }
            }
        } else if let node::Stmt::Syscall(decl) = stmt {
            let ty;
            if let Some(t) = &decl.decl_type {
                ty = self.r#type(t, false)?;
            } else {
                ty = Type::Void;
            }
            let mut types = Vec::new();
            for arg in decl.types.iter() {
                let ty = self.r#type(arg, false)?;
                types.push(ty.clone());
            }
            if public {
                let s;
                if decl.name.str == "main" {
                    if self.symbol_table.contains_key("main") {
                        return Err(SemanticError::SymbolExists(decl.name.clone()));
                    }
                    s = "main".to_string();
                } else {
                    let len;
                    if let Some(sigs) = self.fn_map.get_mut(&decl.name.str) {
                        if sigs.iter().any(|(tys, _)| *tys == types) {
                            return Err(SemanticError::SymbolExists(decl.name.clone()));
                        }
                        len = sigs.len() + 1;
                        sigs.push((types.clone(), len));
                    } else {
                        self.fn_map.insert(decl.name.str.clone(), vec![(types.clone(), 1)]);
                        len = 1;
                    }
                    s = format!("{}.{}", decl.name.str, len);
                    decl.name.str = s.clone();
                }
                self.symbol_table.insert(s, Symbol::Syscall { id: decl.id, ty, args: types });
            } else {
                let split: Vec<&str> = decl.name.str.split('.').collect();
                if split.len() < 2 {
                    self.symbol_table.insert(decl.name.str.clone(), Symbol::Syscall { id: decl.id, ty, args: types });
                } else {
                    let id = split.last().unwrap().parse::<usize>().unwrap();
                    let name = split[..split.len() - 1].join(".");
                    if let Some(sigs) = self.fn_map.get_mut(&name) {
                        if sigs.iter().any(|(tys, _)| *tys == types) {
                            return Err(SemanticError::SymbolExists(decl.name.clone()));
                        }
                        sigs.push((types.clone(), id));
                    } else {
                        self.fn_map.insert(name, vec![(types.clone(), id)]);
                    }
                    self.symbol_table.insert(decl.name.str.clone(), Symbol::Syscall { id: decl.id, ty, args: types });
                }
            }
        }
        Ok(())
    }

}