use crate::ir::{Op, Symbol, Term};
use crate::lower::Lower;
use crate::node::{self, StringFragment};
use crate::types::Type;

impl<'a> Lower<'a> {
    pub fn arr_lit(&mut self, arr_lit: &node::ArrLit, expr: &node::Expr) -> Term {
        let ptr;
        let Some(inner) = expr.ty.dereference() else {
            panic!("Salloc not a pointer");
        };
        let size = inner.size();
        let inner_salloc = inner.salloc();
        if let Some(p) = &self.cur_salloc {
            ptr = p.clone();
        } else {
            self.stack_count += 1;
            ptr = Term::Stack(self.stack_count);
            self.cur_salloc = Some(ptr.clone());
            self.push_op(
                Op::Decl {
                    term: ptr.clone(),
                    size: Type::align(arr_lit.exprs.len() as u32 * size),
                },
                arr_lit.pos_id,
            );
        }
        for expr in &arr_lit.exprs {
            let r = self.expr(expr);
            if !inner_salloc {
                self.temp_count += 1;
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: ptr.clone(),
                        offset: self.salloc_offset,
                        op: "=".to_string(),
                        term: r,
                        size: expr.ty.size(),
                    },
                    arr_lit.pos_id,
                );
                self.salloc_offset += size as i64;
            }
        }
        if Some(ptr.clone()) == self.cur_salloc {
            self.cur_salloc = None;
            self.salloc_offset = 0;
        }
        ptr
    }
    pub fn list_lit(&mut self, arr_lit: &node::ArrLit, alloc_fn: &String, expr: &node::Expr) -> Term {
        let ptr;
        let Type::List { inner } = &expr.ty else {
            panic!("not a list");
        };
        let size = inner.size();
        let inner_salloc = inner.salloc();
        let len = arr_lit.exprs.len();
        let mut is_salloc = true;
        if let Some(p) = &self.cur_salloc {
            ptr = p.clone();
            is_salloc = false;
        } else {
            self.stack_count += 1;
            ptr = Term::Stack(self.stack_count);
            self.push_op(Op::Decl { term: ptr.clone(), size: 16 }, expr.span.end);
        }
        let salloc_offset = self.salloc_offset;
        self.push_op(Op::BeginScope, expr.span.end);
        // len
        self.push_op(
            Op::Store {
                res: None,
                ptr: ptr.clone(),
                offset: salloc_offset,
                op: "=".to_string(),
                term: Term::IntLit(len as i64),
                size: 8,
            },
            expr.span.end,
        );
        // *any = alloc(len)
        self.push_op(Op::BeginCall { params: vec![] }, expr.span.end);
        self.push_op(
            Op::Param {
                term: Term::IntLit(Type::align(len as u32 * size) as i64),
            },
            expr.span.end,
        );
        self.temp_count += 1;
        self.push_op(
            Op::Call {
                res: Some(Term::Temp(self.temp_count)),
                func: alloc_fn.to_string(),
            },
            expr.span.end,
        );
        self.push_op(
            Op::Store {
                res: None,
                ptr: ptr.clone(),
                offset: salloc_offset + 8,
                op: "=".to_string(),
                term: Term::Temp(self.temp_count),
                size: 8,
            },
            expr.span.end,
        );
        // ptr = &*any
        self.pointer_count += 1;
        let temp_ptr = Term::Pointer(self.pointer_count);
        self.temp_count += 1;
        self.push_op(
            Op::Read {
                res: Term::Temp(self.temp_count),
                ptr: ptr.clone(),
                offset: salloc_offset + 8,
                size: 8,
            },
            expr.span.end,
        );
        self.push_op(
            Op::Let {
                res: temp_ptr.clone(),
                term: Term::Temp(self.temp_count),
            },
            expr.span.end,
        );
        self.cur_salloc = Some(temp_ptr.clone());
        self.salloc_offset = 0;

        for expr in &arr_lit.exprs {
            let r = self.expr(expr);
            if !inner_salloc {
                self.temp_count += 1;
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: temp_ptr.clone(),
                        offset: self.salloc_offset,
                        op: "=".to_string(),
                        term: r,
                        size: expr.ty.size(),
                    },
                    arr_lit.pos_id,
                );
                self.salloc_offset += size as i64;
            }
        }
        if is_salloc {
            self.cur_salloc = None;
            self.salloc_offset = 0;
        } else {
            self.cur_salloc = Some(ptr.clone());
            self.salloc_offset = salloc_offset + 16;
        }
        self.push_op(Op::EndScope, expr.span.end);
        ptr
    }
    pub fn struct_lit(&mut self, lit: &node::StructLit, expr: &node::Expr) -> Term {
        let ptr;
        let ty = &expr.ty;
        let mut is_salloc = true;
        if let Some(p) = &self.cur_salloc {
            ptr = p.clone();
            is_salloc = false;
        } else {
            self.stack_count += 1;
            ptr = Term::Stack(self.stack_count);
            self.cur_salloc = Some(ptr.clone());
            self.push_op(
                Op::Decl {
                    term: ptr.clone(),
                    size: ty.aligned_size(),
                },
                expr.span.end,
            );
        }
        for expr in &lit.field_exprs {
            let r = self.expr(expr);
            if !expr.ty.salloc() {
                self.temp_count += 1;
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: ptr.clone(),
                        offset: self.salloc_offset,
                        op: "=".to_string(),
                        term: r,
                        size: expr.ty.size(),
                    },
                    expr.span.end,
                );
                self.salloc_offset += expr.ty.size() as i64;
            }
        }
        if is_salloc {
            self.cur_salloc = None;
            self.salloc_offset = 0;
        }
        ptr
    }
    pub fn string_lit(&mut self, frags: &Vec<StringFragment>, alloc_fn: &String, expr: &node::Expr) -> Term {
        if frags.len() != 1 {
            let ptr;
            let mut is_salloc = true;
            if let Some(p) = &self.cur_salloc {
                ptr = p.clone();
                self.cur_salloc = None;
                is_salloc = false;
            } else {
                self.stack_count += 1;
                ptr = Term::Stack(self.stack_count);
                self.push_op(Op::Decl { term: ptr.clone(), size: 16 }, expr.span.end);
            }
            let salloc_offset = self.salloc_offset;
            self.push_op(Op::BeginScope, expr.span.end);
            // Get length of string literals
            let mut len = 0;
            for f in frags {
                if let node::StringFragment::Lit(string_lit) = f {
                    len += string_lit.len();
                }
            }
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: ptr.clone(),
                    offset: salloc_offset,
                    op: "=".to_string(),
                    term: Term::IntLit(len as i64),
                    size: 8,
                },
                expr.span.end,
            );
            // Call str functions, store results in 'slices' and add to the length
            let mut slices = vec![];
            for f in frags {
                if let node::StringFragment::Expr { expr, str_fn, .. } = f {
                    self.push_op(Op::BeginCall { params: vec![] }, expr.span.end);
                    let term = self.expr(expr);
                    for p in self.param(term.clone(), expr.ty.size(), expr.ty.aligned_size(), expr.span.end) {
                        self.push_op(Op::Param { term: p }, expr.span.end);
                    }
                    self.double_count += 1;
                    self.push_op(
                        Op::Call {
                            res: Some(Term::Double(self.double_count)),
                            func: str_fn.to_string(),
                        },
                        expr.span.end,
                    );
                    let new = self.make_stack(Term::Double(self.double_count), &expr.ty, expr.span.end);
                    self.push_op(
                        Op::Store {
                            res: None,
                            ptr: ptr.clone(),
                            offset: salloc_offset,
                            op: "+=".to_string(),
                            term: new.clone(),
                            size: 8,
                        },
                        expr.span.end,
                    );
                    slices.push(new);
                }
            }
            // Call alloc with length
            self.temp_count += 1;
            self.push_op(
                Op::BeginCall {
                    params: vec![Term::Temp(self.temp_count)],
                },
                expr.span.end,
            );
            self.push_op(
                Op::Read {
                    res: Term::Temp(self.temp_count),
                    ptr: ptr.clone(),
                    offset: salloc_offset,
                    size: 8,
                },
                expr.span.end,
            );
            self.push_op(Op::Param { term: Term::Temp(self.temp_count) }, expr.span.end);
            self.temp_count += 1;
            self.push_op(
                Op::Call {
                    res: Some(Term::Temp(self.temp_count)),
                    func: alloc_fn.to_string(),
                },
                expr.span.end,
            );
            // Store in stack alloc
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: ptr.clone(),
                    offset: salloc_offset + 8,
                    op: "=".to_string(),
                    term: Term::Temp(self.temp_count),
                    size: 8,
                },
                expr.span.end,
            );
            // Duplicate pointer
            self.pointer_count += 1;
            let temp_ptr = Term::Pointer(self.pointer_count);
            let temp_pa = Term::PointerArithmetic(self.pointer_count);
            self.temp_count += 1;
            self.push_op(
                Op::Read {
                    res: Term::Temp(self.temp_count),
                    ptr: ptr.clone(),
                    offset: salloc_offset + 8,
                    size: 8,
                },
                expr.span.end,
            );
            self.push_op(
                Op::Let {
                    res: temp_ptr.clone(),
                    term: Term::Temp(self.temp_count),
                },
                expr.span.end,
            );
            let mut i = 0;
            for f in frags {
                match f {
                    node::StringFragment::Lit(lit) => {
                        if let Some(sym) = self.string_map.get(lit) {
                            self.push_op(
                                Op::Copy {
                                    from: Term::Data(sym.clone()),
                                    to: temp_ptr.clone(),
                                    size: Term::IntLit(Type::align(lit.len() as u32) as i64),
                                },
                                expr.span.end,
                            );
                        } else {
                            self.string_id += 1;
                            let new_sym = format!("s.{}", self.string_id);
                            self.string_map.insert(lit.clone(), new_sym.clone());
                            self.ir.insert(
                                new_sym.clone(),
                                Symbol::Data {
                                    ty: Type::Slice { inner: Box::new(Type::Char) },
                                    str: lit.to_string(),
                                },
                            );
                            self.push_op(
                                Op::Copy {
                                    from: Term::Data(new_sym),
                                    to: temp_ptr.clone(),
                                    size: Term::IntLit(Type::align(lit.len() as u32) as i64),
                                },
                                expr.span.end,
                            );
                        }
                        self.push_op(
                            Op::Store {
                                res: None,
                                ptr: temp_pa.clone(),
                                offset: 0,
                                op: "+=".to_string(),
                                term: Term::IntLit(lit.len() as i64),
                                size: 8,
                            },
                            expr.span.end,
                        );
                    }
                    node::StringFragment::Expr { .. } => {
                        let slice = slices.get(i).unwrap();
                        i += 1;
                        self.temp_count += 1;
                        self.push_op(
                            Op::Read {
                                res: Term::Temp(self.temp_count),
                                ptr: slice.clone(),
                                offset: 8,
                                size: 8,
                            },
                            expr.span.end,
                        );
                        self.push_op(
                            Op::Copy {
                                from: Term::Temp(self.temp_count),
                                to: temp_ptr.clone(),
                                size: slice.clone(),
                            },
                            expr.span.end,
                        );
                        self.push_op(
                            Op::Store {
                                res: None,
                                ptr: temp_pa.clone(),
                                offset: 0,
                                op: "+=".to_string(),
                                term: slice.clone(),
                                size: 8,
                            },
                            expr.span.end,
                        );
                    }
                }
            }
            if is_salloc {
                self.cur_salloc = None;
                self.salloc_offset = 0;
            } else {
                self.cur_salloc = Some(ptr.clone());
                self.salloc_offset = salloc_offset + 16;
            }
            self.push_op(Op::EndScope, expr.span.end);
            ptr
        } else if let Some(node::StringFragment::Lit(lit)) = frags.get(0) {
            let ptr;
            let mut is_salloc = true;
            if let Some(p) = &self.cur_salloc {
                ptr = p.clone();
                is_salloc = false;
            } else {
                self.stack_count += 1;
                ptr = Term::Stack(self.stack_count);
                self.cur_salloc = Some(ptr.clone());
                self.push_op(Op::Decl { term: ptr.clone(), size: 16 }, expr.span.end);
            }
            self.temp_count += 1;
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: ptr.clone(),
                    offset: self.salloc_offset,
                    op: "=".to_string(),
                    term: Term::IntLit(lit.len() as i64),
                    size: 8,
                },
                expr.span.end,
            );
            self.salloc_offset += 8;
            if let Some(sym) = self.string_map.get(lit) {
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: ptr.clone(),
                        offset: self.salloc_offset,
                        op: "=".to_string(),
                        term: Term::Data(sym.clone()),
                        size: 8,
                    },
                    expr.span.end,
                );
            } else {
                self.string_id += 1;
                let new_sym = format!("s.{}", self.string_id);
                self.string_map.insert(lit.clone(), new_sym.clone());
                self.ir.insert(
                    new_sym.clone(),
                    Symbol::Data {
                        ty: Type::Slice { inner: Box::new(Type::Char) },
                        str: lit.to_string(),
                    },
                );
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: ptr.clone(),
                        offset: self.salloc_offset,
                        op: "=".to_string(),
                        term: Term::Data(new_sym),
                        size: 8,
                    },
                    expr.span.end,
                );
            }
            self.salloc_offset += 8;
            if is_salloc {
                self.cur_salloc = None;
                self.salloc_offset = 0;
            }
            ptr
        } else {
            panic!("len == 1, not lit");
        }
    }
    pub fn tuple_lit(&mut self, exprs: &Vec<node::Expr>, expr: &node::Expr) -> Term {
        let ptr;
        let mut is_salloc = true;
        if let Some(p) = &self.cur_salloc {
            ptr = p.clone();
            is_salloc = false;
        } else {
            self.stack_count += 1;
            ptr = Term::Stack(self.stack_count);
            self.cur_salloc = Some(ptr.clone());
            self.push_op(
                Op::Decl {
                    term: ptr.clone(),
                    size: expr.ty.aligned_size(),
                },
                expr.span.end,
            );
        }
        for expr in exprs {
            let r = self.expr(expr);
            if !expr.ty.salloc() {
                self.temp_count += 1;
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: ptr.clone(),
                        offset: self.salloc_offset,
                        op: "=".to_string(),
                        term: r,
                        size: expr.ty.size(),
                    },
                    expr.span.end,
                );
                self.salloc_offset += expr.ty.size() as i64;
            }
        }
        if is_salloc {
            self.cur_salloc = None;
            self.salloc_offset = 0;
        }
        ptr
    }
}
