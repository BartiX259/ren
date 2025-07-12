use super::{SemanticError, Validate};
use crate::ir::{Block, Symbol};
use crate::node;
use crate::types::Type;

impl Validate {
    pub fn hoist_type(&mut self, stmt: &mut node::Stmt, public: bool) -> Result<(), SemanticError> {
        let mut stmt_ref = stmt; // local mutable reference
        let mut is_public = false;

        if let node::Stmt::Decorator(dec) = stmt_ref {
            stmt_ref = dec.inner.as_mut(); // swap the reference, not the actual AST
            if dec.kinds.contains(&node::DecoratorKind::Pub) {
                is_public = true;
            }
        }

        if public && !is_public {
            return Ok(());
        }

        match stmt_ref {
            node::Stmt::TypeDecl(decl) => {
                if self.symbol_table.contains_key(&decl.name.str) {
                    return Err(SemanticError::SymbolExists(decl.name.clone()));
                }
                let ty = self.r#type(&decl.r#type, false)?;
                self.symbol_table.insert(decl.name.str.clone(), Symbol::Type { ty });
            }
            node::Stmt::Enum(e) => {
                if self.symbol_table.contains_key(&e.name.str) {
                    return Err(SemanticError::SymbolExists(e.name.clone()));
                }
                let ty = Type::Enum(
                    e.variants
                        .iter()
                        .map(|(p, t)| {
                            let typ = t
                                .as_ref()
                                .map(|tt| self.r#type(tt, false))
                                .transpose()?; // Converts Option<Result<T>> to Result<Option<T>>
                            Ok((p.str.clone(), typ))
                        })
                        .collect::<Result<_, _>>()? // Collect into Result<Vec<_>, _>
                );
                self.symbol_table.insert(e.name.str.clone(), Symbol::Type { ty });
            }
            node::Stmt::Let(decl) => {
                let node::Capture::Single(pos_str) = &decl.capture else {
                    return Err(SemanticError::InvalidCapture(decl.capture.clone(), decl.expr.ty.clone()));
                };
                if self.symbol_table.contains_key(&pos_str.str) {
                    return Err(SemanticError::SymbolExists(pos_str.clone()));
                }
                let str = self.const_expr(&decl.expr)?;
                let ty = self.expr(&mut decl.expr)?;
                self.symbol_table.insert(pos_str.str.clone(), Symbol::Data { ty, str });
            }
            node::Stmt::Decl(decl) => {
                if self.symbol_table.contains_key(&decl.name.str) {
                    return Err(SemanticError::SymbolExists(decl.name.clone()));
                }
                let ty = self.r#type(&decl.r#type, false)?;
                let size = ((ty.size() + 7) / 8) as usize;
                let str = std::iter::repeat("0").take(size).collect::<Vec<_>>().join(", ");
                self.symbol_table.insert(decl.name.str.clone(), Symbol::Data { ty, str });
            }
            _ => {}
        }

        Ok(())
    }

    pub fn hoist_func(&mut self, stmt: &mut node::Stmt, public: bool) -> Result<(), SemanticError> {
        let mut stmt_ref = stmt;
        let mut is_public = false;

        if let node::Stmt::Decorator(dec) = stmt_ref {
            stmt_ref = dec.inner.as_mut();
            if dec.kinds.contains(&node::DecoratorKind::Pub) {
                is_public = true;
            }
        }

        if let node::Stmt::Fn(decl) = stmt_ref {
            decl.generics.clone_into(&mut self.cur_generics);
            let ty;
            if let Some(t) = &decl.decl_type {
                ty = self.r#type(t, false)?;
            } else {
                ty = Type::Void;
            }
            let mut arg_names: Vec<String> = Vec::new();
            let mut types = Vec::new();
            for arg in decl.arg_names.iter().zip(decl.arg_types.iter()) {
                if self.symbol_table.contains_key(&arg.0.str) {
                    return Err(SemanticError::SymbolExists(arg.0.clone()));
                }
                for existing in arg_names.iter() {
                    if *existing == arg.0.str {
                        return Err(SemanticError::SymbolExists(arg.0.clone()));
                    }
                }
                let ty = self.r#type(arg.1, false)?;
                types.push(ty.clone());
                arg_names.push(arg.0.str.clone());
            }
            self.hoist_func_with_types(decl, ty, types, public, is_public)?;
            if self.cur_generics.len() > 0 {
                let mut found = false;
                for (name, _) in self.gfns.iter() {
                    if *name == decl.name.str {
                        found = true;
                        break;
                    }
                }
                if !found {
                    self.gfns.push((decl.name.str.clone(), 1));
                }
                self.cur_generics.clear();
            }
        } else if let node::Stmt::Syscall(decl) = stmt_ref {
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
                if is_public {
                    self.symbol_table.insert(s, Symbol::Syscall { id: decl.id, ty, args: types });
                }
            } else {
                let split: Vec<&str> = decl.name.str.split('.').collect();
                if split.len() != 2 {
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

    pub fn hoist_func_with_types(&mut self, decl: &mut node::Fn, ty: Type, types: Vec<Type>, public: bool, is_public: bool) -> Result<(), SemanticError> {
        if public {
            let s;
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
            if is_public {
                self.symbol_table.insert(s, Symbol::ExternFunc { ty, args: types });
            }
        } else {
            let split: Vec<&str> = decl.name.str.split('.').collect();
            if split.len() != 2 {
                self.symbol_table.insert(
                    decl.name.str.clone(),
                    Symbol::Func {
                        ty,
                        module: self.cur_module.clone(),
                        block: Block::new(),
                        args: types,
                        public: is_public,
                    },
                );
            } else {
                let id = split.get(1).unwrap().parse::<usize>().unwrap();
                let name = split.first().unwrap().to_string();
                if let Some(sigs) = self.fn_map.get_mut(&name) {
                    if sigs.iter().any(|(tys, _)| *tys == types) {
                        return Err(SemanticError::SymbolExists(decl.name.clone()));
                    }
                    sigs.push((types.clone(), id));
                } else {
                    self.fn_map.insert(name, vec![(types.clone(), id)]);
                }
                self.symbol_table.insert(
                    decl.name.str.clone(),
                    Symbol::Func {
                        ty,
                        module: self.cur_module.clone(),
                        block: Block::new(),
                        args: types,
                        public: is_public,
                    },
                );
            }
        }
        Ok(())
    }
}
