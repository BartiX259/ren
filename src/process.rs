use crate::node::{self, StringFragment};
use crate::types::Type;

/// Modify the ast after validation to express syntax sugar with simpler statements
pub fn process(module: node::Module) -> Vec<node::Stmt> {
    let mut res = Vec::new();
    let mut l = Process::new();
    for s in module.stmts.into_iter() {
        let new_stmt = l.stmt(s);
        res.push(new_stmt);
    }
    return res;
}

struct Process {
    pub stmts: Vec<Vec<node::Stmt>>,
}
impl Process {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }

    fn push(&mut self, stmt: node::Stmt) {
        self.stmts.last_mut().unwrap().push(stmt);
    }

    fn stmt(&mut self, stmt: node::Stmt) -> node::Stmt {
        match stmt {
            node::Stmt::Expr(expr) => node::Stmt::Expr(self.expr(expr)),
            node::Stmt::Let(decl) => node::Stmt::Let(node::Let {
                name: decl.name,
                expr: self.expr(decl.expr),
            }),
            node::Stmt::LetElseScope(r#else) => node::Stmt::LetElseScope(node::ElseScope {
                unpack: self.unpack(r#else.unpack),
                capture: r#else.capture,
                pos_str: r#else.pos_str,
                scope: self.scope(r#else.scope),
            }),
            node::Stmt::Fn(decl) => node::Stmt::Fn(node::Fn {
                name: decl.name,
                arg_names: decl.arg_names,
                arg_types: decl.arg_types,
                decl_type: decl.decl_type,
                generics: vec![],
                scope: self.scope(decl.scope),
            }),
            node::Stmt::Decorator(dec) => self.stmt(*dec.inner),
            node::Stmt::Ret(ret) => node::Stmt::Ret(node::Ret {
                pos_id: ret.pos_id,
                expr: self.opt_expr(ret.expr),
            }),
            node::Stmt::If(r#if) => node::Stmt::If(self.r#if(r#if)),
            node::Stmt::Loop(r#loop) => node::Stmt::Loop(node::Loop {
                pos_id: r#loop.pos_id,
                scope: self.scope(r#loop.scope),
            }),
            node::Stmt::While(r#while) => node::Stmt::While(node::While {
                pos_id: r#while.pos_id,
                expr: self.expr(r#while.expr),
                scope: self.scope(r#while.scope),
            }),
            node::Stmt::For(r#for) => node::Stmt::For(node::For {
                pos_id: r#for.pos_id,
                init: self.let_or_expr(r#for.init),
                cond: self.expr(r#for.cond),
                incr: self.expr(r#for.incr),
                scope: self.scope(r#for.scope),
            }),
            node::Stmt::ForIn(r#for) => node::Stmt::ForIn(node::ForIn {
                pos_id: r#for.pos_id,
                capture: r#for.capture,
                expr: self.expr(r#for.expr),
                scope: self.scope(r#for.scope),
            }),
            node::Stmt::Match(m) => node::Stmt::Match(node::Match {
                pos_id: m.pos_id,
                match_expr: self.expr(m.match_expr),
                branches: m.branches.into_iter().map(|(e, scope)| (self.expr(e), self.scope(scope))).collect(),
            }),
            node::Stmt::MatchType(m) => node::Stmt::MatchType(node::MatchType {
                pos_id: m.pos_id,
                generics: m.generics,
                match_type: m.match_type,
                branches: m.branches.into_iter().map(|(t, scope, active)| (t, if active { self.scope(scope) } else { scope }, active)).collect(),
            }),
            _ => stmt,
        }
    }

    fn opt_expr(&mut self, expr: Option<node::Expr>) -> Option<node::Expr> {
        return expr.map(|e| self.expr(e));
    }

    fn let_or_expr(&mut self, e: node::LetOrExpr) -> node::LetOrExpr {
        match e {
            node::LetOrExpr::Let(r#let) => node::LetOrExpr::Let(r#let),
            node::LetOrExpr::Expr(expr) => node::LetOrExpr::Expr(self.expr(expr)),
        }
    }

    fn expr_list(&mut self, exprs: Vec<node::Expr>) -> Vec<node::Expr> {
        let mut res = Vec::new();
        for expr in exprs {
            res.push(self.expr(expr));
        }
        res
    }

    fn expr(&mut self, expr: node::Expr) -> node::Expr {
        let new_expr = match expr.kind {
            node::ExprKind::ArrLit(arr_lit) => node::ExprKind::ArrLit(node::ArrLit {
                exprs: self.expr_list(arr_lit.exprs),
                pos_id: arr_lit.pos_id,
            }),
            node::ExprKind::ListLit(arr_lit, alloc_fn) => node::ExprKind::ListLit(
                node::ArrLit {
                    exprs: self.expr_list(arr_lit.exprs),
                    pos_id: arr_lit.pos_id,
                },
                alloc_fn,
            ),
            node::ExprKind::StringLit(frags, alloc_fn) => {
                let mut list = vec![];
                for f in frags {
                    if let StringFragment::Expr { expr, str_fn } = f {
                        list.push(StringFragment::Expr { expr: self.expr(expr), str_fn });
                    } else {
                        list.push(f);
                    }
                }
                node::ExprKind::StringLit(list, alloc_fn)
            }
            node::ExprKind::StructLit(lit) => node::ExprKind::StructLit(node::StructLit {
                field_names: lit.field_names,
                field_exprs: self.expr_list(lit.field_exprs),
            }),
            node::ExprKind::TupleLit(lit) => node::ExprKind::TupleLit(self.expr_list(lit)),
            node::ExprKind::MapLit(lit) => node::ExprKind::MapLit(node::MapLit {
                keys: self.expr_list(lit.keys),
                values: self.expr_list(lit.values),
                init_fn: lit.init_fn,
                pos_id: lit.pos_id,
            }),
            node::ExprKind::BinExpr(bin) => self.bin_expr(bin),
            node::ExprKind::Call(call) => node::ExprKind::Call(node::Call {
                name: call.name,
                args: self.expr_list(call.args),
            }),
            node::ExprKind::UnExpr(un) => self.un_expr(un),
            node::ExprKind::PostUnExpr(un) => node::ExprKind::PostUnExpr(node::UnExpr {
                expr: Box::new(self.expr(*un.expr)),
                op: un.op,
            }),
            node::ExprKind::TypeCast(cast) => node::ExprKind::TypeCast(node::TypeCast {
                r#type: cast.r#type,
                expr: Box::new(self.expr(*cast.expr)),
            }),
            node::ExprKind::BuiltIn(built_in) => node::ExprKind::BuiltIn(node::BuiltIn {
                kind: built_in.kind,
                args: self.expr_list(built_in.args),
                type_args: built_in.type_args,
                tys: built_in.tys,
            }),
            _ => expr.kind,
        };
        node::Expr {
            kind: new_expr,
            ty: expr.ty,
            span: expr.span,
        }
    }

    fn scope(&mut self, scope: Vec<node::Stmt>) -> Vec<node::Stmt> {
        self.stmts.push(Vec::new());
        for s in scope.into_iter() {
            let new_stmt = self.stmt(s);
            self.push(new_stmt);
        }
        return self.stmts.pop().unwrap();
    }

    fn unpack(&mut self, unpack: node::Unpack) -> node::Unpack {
        node::Unpack {
            lhs: unpack.lhs,
            rhs: unpack.rhs,
            brackets: unpack.brackets,
            expr: self.expr(unpack.expr),
        }
    }

    fn r#if(&mut self, r#if: node::If) -> node::If {
        let cond = match r#if.cond {
            node::IfKind::Expr(expr) => node::IfKind::Expr(self.expr(expr)),
            node::IfKind::Unpack(unpack) => node::IfKind::Unpack(self.unpack(unpack)),
            node::IfKind::None => node::IfKind::None,
        };
        return node::If {
            pos_id: r#if.pos_id,
            cond,
            scope: self.scope(r#if.scope),
            els: r#if.els.map(|e| Box::new(self.r#if(*e))),
        };
    }

    fn pos_str(&self, str: String) -> node::PosStr {
        node::PosStr { str, pos_id: 0 }
    }

    fn calc_bin_expr(&self, bin: node::BinExpr) -> node::ExprKind {
        // If both sides are integer literals, constant fold them
        if !(bin.is_bool() || bin.is_cmp()) {
            if let node::ExprKind::IntLit(l) = &bin.lhs.kind {
                if let node::ExprKind::IntLit(r) = &bin.rhs.kind {
                    return node::ExprKind::IntLit(match bin.op.str.as_str() {
                        "+" => l + r,
                        "-" => l - r,
                        "*" => l * r,
                        "/" => l / r,
                        "%" => l % r,
                        ">>" => l >> r,
                        "<<" => l << r,
                        "|" => l | r,
                        "&" => l & r,
                        _ => panic!("Can't calculate {l} {} {r}", bin.op.str),
                    });
                }
            }

            // Basic constant folding when one side is 0 or 1
            match bin.op.str.as_str() {
                "+" | "-" => {
                    if let node::ExprKind::IntLit(0) = bin.rhs.kind {
                        return bin.lhs.kind;
                    }
                    if let node::ExprKind::IntLit(0) = bin.lhs.kind {
                        if bin.op.str == "+" {
                            return bin.rhs.kind;
                        } else {
                            return node::ExprKind::UnExpr(node::UnExpr { expr: bin.rhs, op: bin.op });
                        }
                    }
                }
                "*" => {
                    if let node::ExprKind::IntLit(0) = bin.lhs.kind {
                        return node::ExprKind::IntLit(0);
                    }
                    if let node::ExprKind::IntLit(0) = bin.rhs.kind {
                        return node::ExprKind::IntLit(0);
                    }
                    if let node::ExprKind::IntLit(1) = bin.lhs.kind {
                        return bin.rhs.kind;
                    }
                    if let node::ExprKind::IntLit(1) = bin.rhs.kind {
                        return bin.lhs.kind;
                    }
                }
                "/" => {
                    if let node::ExprKind::IntLit(1) = bin.rhs.kind {
                        return bin.lhs.kind;
                    }
                }
                "&" => {
                    if let node::ExprKind::IntLit(-1) = bin.rhs.kind {
                        return bin.lhs.kind;
                    }
                    if let node::ExprKind::IntLit(-1) = bin.lhs.kind {
                        return bin.rhs.kind;
                    }
                }
                "|" => {
                    if let node::ExprKind::IntLit(0) = bin.lhs.kind {
                        return bin.rhs.kind;
                    }
                    if let node::ExprKind::IntLit(0) = bin.rhs.kind {
                        return bin.lhs.kind;
                    }
                }
                _ => {}
            }
        }
        node::ExprKind::BinExpr(bin)
    }

    fn bin_expr(&mut self, bin: node::BinExpr) -> node::ExprKind {
        if bin.op.str == ".." {
            return node::ExprKind::TupleLit(vec![self.expr(*bin.lhs), self.expr(*bin.rhs)]);
        }
        if bin.op.str == "." {
            // Member access -> add &, apply offset and dereference
            if let Type::Enum(vars) = &bin.lhs.ty {
                if let node::ExprKind::Variable(pos_str) = &bin.rhs.kind {
                    let i = vars.iter().position(|(s, _)| *s == pos_str.str).unwrap();
                    return node::ExprKind::IntLit(i as i64);
                } else if let node::ExprKind::Call(call) = &bin.rhs.kind {
                    let i = vars.iter().position(|(s, _)| *s == call.name.str).unwrap();
                    let mut i_expr = bin.rhs.clone();
                    i_expr.ty = Type::Int;
                    i_expr.kind = node::ExprKind::IntLit(i as i64);
                    return node::ExprKind::TupleLit(vec![*i_expr, self.expr(call.args.get(0).unwrap().clone())]);
                } else {
                    unreachable!();
                }
            }
            let lhs;
            let map;
            if let Type::Pointer(p) = &bin.lhs.ty {
                let Type::Struct(m) = *p.clone() else {
                    panic!("Member access lhs not a struct 1");
                };
                map = m.clone();
                lhs = Box::new(node::Expr {
                    ty: Type::Struct(m),
                    span: bin.lhs.span,
                    kind: bin.lhs.kind,
                });
            } else {
                let Type::Struct(m) = &bin.lhs.ty else {
                    panic!("Member access lhs not a struct 2");
                };
                map = m.clone();
                lhs = Box::new(node::Expr {
                    ty: Type::Struct(m.clone()),
                    span: bin.lhs.span,
                    kind: node::ExprKind::UnExpr(node::UnExpr {
                        expr: bin.lhs.clone(),
                        op: self.pos_str("&".to_string()),
                    }),
                });
            }
            let node::ExprKind::Variable(pos_str) = bin.rhs.kind else {
                panic!("Member access rhs not an identifier");
            };
            let (ty, offset) = map.get(&pos_str.str).unwrap();
            return node::ExprKind::UnExpr(node::UnExpr {
                op: self.pos_str("*".to_string()),
                expr: Box::new(node::Expr {
                    ty: ty.clone(),
                    span: bin.lhs.span.add(bin.rhs.span),
                    kind: self.bin_expr(node::BinExpr {
                        lhs,
                        rhs: Box::new(node::Expr {
                            ty: Type::Int,
                            span: bin.rhs.span,
                            kind: node::ExprKind::IntLit(*offset as i64),
                        }),
                        op: self.pos_str("+".to_string()),
                    }),
                }),
            });
        }
        let ltype = bin.lhs.ty.clone();
        let mut lhs = self.expr(*bin.lhs);
        let rtype = bin.rhs.ty.clone();
        let mut rhs = self.expr(*bin.rhs);
        if let Some(p) = ltype.dereference() {
            if rtype.dereference().is_none() && rhs.ty != Type::Range {
                // Pointer arithmetic - multiply by size of inner type
                let rspan = rhs.span;
                rhs = node::Expr {
                    ty: rhs.ty.clone(),
                    span: rspan,
                    kind: self.calc_bin_expr(node::BinExpr {
                        lhs: Box::new(rhs),
                        rhs: Box::new(node::Expr {
                            ty: Type::Int,
                            span: rspan,
                            kind: node::ExprKind::IntLit(p.size() as i64),
                        }),
                        op: self.pos_str("*".to_string()),
                    }),
                };
                if ltype.salloc() {
                    // Add & to stack allocations
                    lhs = node::Expr {
                        ty: Type::Pointer(Box::new(lhs.ty.clone())),
                        span: lhs.span,
                        kind: node::ExprKind::UnExpr(node::UnExpr {
                            expr: Box::new(lhs),
                            op: self.pos_str("&".to_string()),
                        }),
                    }
                }
            }
        }
        if bin.op.str.starts_with("=") && bin.op.str != "==" {
            if let node::ExprKind::Call(c) = &lhs.kind {
                let ins_fn = bin.op.str.split("=").last().unwrap();
                let mut args = c.args.clone();
                let e = args.get(0).unwrap();
                args[0] = node::Expr {
                    ty: Type::Pointer(Box::new(e.ty.clone())),
                    span: e.span,
                    kind: node::ExprKind::UnExpr(node::UnExpr {
                        expr: Box::new(e.clone()),
                        op: self.pos_str("&".to_string()),
                    }),
                };
                args.push(rhs);
                return node::ExprKind::Call(node::Call {
                    name: self.pos_str(ins_fn.to_string()),
                    args,
                });
            }
        }
        if bin.op.str.starts_with("[]") {
            let inner = lhs.ty.inner(1).clone();
            if rhs.ty == Type::Range {
                // Slice
                let start;
                let end;
                if let node::ExprKind::TupleLit(range) = &rhs.kind {
                    start = range.get(0).unwrap().clone();
                    end = range.get(1).unwrap().clone();
                } else if let node::ExprKind::Variable(_) = &rhs.kind {
                    let mut stemp = rhs.clone();
                    let mut etemp = rhs.clone();
                    stemp.ty = Type::Int;
                    etemp.ty = Type::Int;
                    let mut r = rhs.clone();
                    let mut add = rhs.clone();
                    let mut addtemp = rhs.clone();
                    addtemp.kind = node::ExprKind::IntLit(8 as i64);
                    r.kind = node::ExprKind::UnExpr(node::UnExpr {
                        expr: Box::new(rhs.clone()),
                        op: self.pos_str("&".to_string()),
                    });
                    add.kind = node::ExprKind::BinExpr(node::BinExpr {
                        lhs: Box::new(r),
                        rhs: Box::new(addtemp),
                        op: self.pos_str("+".to_string()),
                    });
                    etemp.kind = node::ExprKind::UnExpr(node::UnExpr {
                        expr: Box::new(add),
                        op: self.pos_str("*".to_string()),
                    });
                    start = stemp;
                    end = etemp;
                } else {
                    unreachable!();
                }
                return node::ExprKind::TupleLit(vec![
                    node::Expr {
                        // length
                        kind: self.calc_bin_expr(node::BinExpr {
                            lhs: Box::new(end),
                            rhs: Box::new(start.clone()),
                            op: node::PosStr {
                                str: "-".to_string(),
                                pos_id: bin.op.pos_id,
                            },
                        }),
                        ty: Type::Int,
                        span: rhs.span,
                    },
                    self.expr(node::Expr {
                        // pointer
                        kind: node::ExprKind::BinExpr(node::BinExpr {
                            lhs: Box::new(match lhs.ty {
                                Type::Array { .. } | Type::Pointer(_) => lhs,
                                Type::Slice { .. } | Type::List { .. } => {
                                    let mut ptr = lhs.clone();
                                    let mut add = lhs.clone();
                                    let mut addtemp = lhs.clone();
                                    ptr.ty = Type::Pointer(Box::new(inner.clone()));
                                    addtemp.kind = node::ExprKind::IntLit(8 as i64);
                                    add.kind = node::ExprKind::BinExpr(node::BinExpr {
                                        lhs: Box::new(lhs),
                                        rhs: Box::new(addtemp),
                                        op: self.pos_str("+".to_string()),
                                    });
                                    ptr.kind = node::ExprKind::UnExpr(node::UnExpr {
                                        expr: Box::new(add),
                                        op: self.pos_str("*".to_string()),
                                    });
                                    ptr
                                }
                                _ => unreachable!(),
                            }),
                            rhs: Box::new(start),
                            op: node::PosStr {
                                str: "+".to_string(),
                                pos_id: bin.op.pos_id,
                            },
                        }),
                        ty: Type::Pointer(Box::new(inner)),
                        span: rhs.span,
                    }),
                ]);
            } else {
                // Array access -> add and dereference
                let ptr;
                let offset;
                match &lhs.ty {
                    Type::Tuple(v) => {
                        let node::ExprKind::IntLit(i) = rhs.kind else { unreachable!() };
                        let o: u32 = v.iter().take(i as usize).map(|ty| ty.size()).sum();
                        let mut temp = rhs;
                        temp.kind = node::ExprKind::IntLit(o as i64);
                        if let Type::Pointer(_) = lhs.ty {
                            ptr = lhs;
                        } else {
                            ptr = node::Expr {
                                ty: lhs.ty.clone(),
                                span: lhs.span,
                                kind: node::ExprKind::UnExpr(node::UnExpr {
                                    expr: Box::new(lhs),
                                    op: self.pos_str("&".to_string()),
                                }),
                            }
                        }
                        offset = temp;
                    }
                    Type::Slice { .. } | Type::List { .. } => {
                        let mut temp = lhs.clone();
                        let mut add = lhs.clone();
                        let mut addtemp = rhs.clone();
                        let mut multemp = rhs.clone();
                        multemp.kind = node::ExprKind::IntLit(inner.size() as i64);
                        temp.ty = Type::Pointer(Box::new(inner));
                        addtemp.kind = node::ExprKind::IntLit(8 as i64);
                        add.kind = node::ExprKind::BinExpr(node::BinExpr {
                            lhs: Box::new(lhs),
                            rhs: Box::new(addtemp),
                            op: self.pos_str("+".to_string()),
                        });
                        temp.kind = node::ExprKind::UnExpr(node::UnExpr {
                            expr: Box::new(add),
                            op: self.pos_str("*".to_string()),
                        });
                        rhs.kind = self.calc_bin_expr(node::BinExpr {
                            lhs: Box::new(rhs.clone()),
                            rhs: Box::new(multemp),
                            op: self.pos_str("*".to_string()),
                        });
                        ptr = temp;
                        offset = rhs;
                    }
                    Type::HashMap { .. } => {
                        let get_fn = bin.op.str.split("]").last().unwrap();
                        return node::ExprKind::Call(node::Call {
                            name: self.pos_str(get_fn.to_string()),
                            args: vec![lhs, rhs],
                        });
                    }
                    _ => {
                        ptr = lhs;
                        offset = rhs;
                    }
                }
                return node::ExprKind::UnExpr(node::UnExpr {
                    op: self.pos_str("*".to_string()),
                    expr: Box::new(node::Expr {
                        ty: ptr.ty.clone(),
                        span: ptr.span.add(offset.span),
                        kind: self.calc_bin_expr(node::BinExpr {
                            lhs: Box::new(ptr),
                            rhs: Box::new(offset),
                            op: self.pos_str("+".to_string()),
                        }),
                    }),
                });
            }
        }
        self.calc_bin_expr(node::BinExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: bin.op,
        })
    }

    fn un_expr(&mut self, un: node::UnExpr) -> node::ExprKind {
        if un.op.str.starts_with("+") {
            if let node::ExprKind::ArrLit(arr_lit) = un.expr.kind {
                let alloc_fn = un.op.str.split("+").last().unwrap().to_string();
                return node::ExprKind::ListLit(arr_lit, alloc_fn);
            }
        }
        if un.op.str == "&" {
            return node::ExprKind::UnExpr(un);
        }
        node::ExprKind::UnExpr(node::UnExpr {
            expr: Box::new(self.expr(*un.expr)),
            op: un.op,
        })
    }
}
