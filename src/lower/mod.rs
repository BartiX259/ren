mod salloc;

use std::collections::HashMap;

use crate::helpers::StringLit;
use crate::ir::{Block, Op, OpLoc, Symbol, Term};
use crate::node;
use crate::types::Type;

/// Transform the ast into the IR
pub fn lower(stmts: Vec<node::Stmt>, ir: &mut HashMap<String, Symbol>) {
    let mut l = Lower::new(ir);
    for s in stmts.into_iter() {
        l.stmt(&s);
    }
}

struct Lower<'a> {
    ir: &'a mut HashMap<String, Symbol>,
    cur_salloc: Option<Term>,
    ret_salloc: Option<Term>,
    salloc_offset: i64,
    cur_block: Option<Block>,
    var_map: HashMap<String, Term>,
    temp_count: usize,
    double_count: usize,
    stack_count: usize,
    pointer_count: usize,
    stack_return: Option<Term>,
    label_count: u16,
    scope_depth: usize,
    loop_start: Vec<u16>,
    loop_exit: Vec<(u16, usize)>,
    string_map: HashMap<StringLit, String>,
    string_id: usize,
}
impl<'a> Lower<'a> {
    pub fn new(ir: &'a mut HashMap<String, Symbol>) -> Self {
        Self {
            ir,
            cur_salloc: None,
            ret_salloc: None,
            salloc_offset: 0,
            cur_block: None,
            var_map: HashMap::new(),
            temp_count: 0,
            double_count: 0,
            stack_count: 0,
            pointer_count: 0,
            stack_return: None,
            label_count: 0,
            scope_depth: 0,
            loop_start: Vec::new(),
            loop_exit: Vec::new(),
            string_map: HashMap::new(),
            string_id: 0,
        }
    }
    fn push_op(&mut self, op: Op, pos_id: usize) {
        if let Some(ref mut block) = self.cur_block {
            block.ops.push(op);
            block.locs.push(OpLoc { start_id: pos_id, end_id: pos_id });
        } else {
            // panic!("No cur_block!");
        }
    }

    fn clear_res(&mut self) {
        match self.cur_block.as_mut().unwrap().ops.last_mut() {
            Some(Op::BinOp { res, .. }) | Some(Op::Store { res, .. }) | Some(Op::Call { res, .. }) => {
                if *res == Some(Term::Temp(self.temp_count)) {
                    self.temp_count -= 1;
                }
                *res = None;
            }
            _ => (),
        }
        self.stack_return = None;
        // println!("CLEAR RES {:?}", self.cur_block.as_ref().unwrap().ops.last());
    }

    fn stmt(&mut self, stmt: &node::Stmt) {
        match stmt {
            node::Stmt::Expr(expr) => {
                self.expr(expr);
                self.clear_res();
            }
            node::Stmt::Let(decl) => self.r#let(decl),
            node::Stmt::Decl(decl) => self.r#decl(decl),
            node::Stmt::LetElseExpr(r#else) => self.else_expr(r#else),
            node::Stmt::LetElseScope(r#else) => self.else_scope(r#else),
            node::Stmt::Fn(decl) => self.r#fn(decl),
            node::Stmt::MainFn(_) => unreachable!(),
            node::Stmt::Decorator(dec) => self.stmt(dec.inner.as_ref()),
            node::Stmt::Ret(ret) => self.ret(ret),
            node::Stmt::If(r#if) => self.r#if(r#if),
            node::Stmt::Loop(r#loop) => self.r#loop(r#loop),
            node::Stmt::While(r#while) => self.r#while(r#while),
            node::Stmt::For(r#for) => self.r#for(r#for),
            node::Stmt::ForIn(r#for) => self.for_in(r#for),
            node::Stmt::Break(pos_id) => {
                self.push_op(
                    Op::BreakScope {
                        depth: self.scope_depth - self.loop_exit.last().unwrap().1,
                    },
                    *pos_id,
                );
                self.push_op(
                    Op::Jump {
                        label: self.loop_exit.last().unwrap().0,
                    },
                    *pos_id,
                )
            }
            node::Stmt::Continue(pos_id) => {
                self.push_op(
                    Op::BreakScope {
                        depth: self.scope_depth - self.loop_exit.last().unwrap().1,
                    },
                    *pos_id,
                );
                self.push_op(
                    Op::Jump {
                        label: *self.loop_start.last().unwrap(),
                    },
                    *pos_id,
                );
            }
            node::Stmt::TypeDecl(_) => (),
            node::Stmt::Enum(_) => (),
            node::Stmt::Extern(_) => (),
            node::Stmt::Syscall(_) => (),
        }
    }

    fn expr(&mut self, expr: &node::Expr) -> Term {
        match &expr.kind {
            node::ExprKind::IntLit(val) => Term::IntLit(*val),
            node::ExprKind::CharLit(val) => Term::IntLit(*val as i64),
            node::ExprKind::BoolLit(val) => Term::IntLit(*val as i64),
            node::ExprKind::Null => Term::IntLit(0),
            node::ExprKind::None => self.none(&expr.ty, expr.span.end),
            node::ExprKind::Variable(pos_str) => {
                if let Some(var) = self.var_map.get(&pos_str.str) {
                    var.clone()
                } else {
                    Term::Data(pos_str.str.clone())
                }
            }
            node::ExprKind::ArrLit(arr_lit) => self.arr_lit(arr_lit, &expr),
            node::ExprKind::ListLit(arr_lit, alloc_fn) => self.list_lit(arr_lit, alloc_fn, &expr),
            node::ExprKind::StructLit(struct_lit) => self.struct_lit(struct_lit, &expr),
            node::ExprKind::StringLit(frags, alloc_fn) => self.string_lit(frags, alloc_fn, &expr),
            node::ExprKind::TupleLit(exprs) => self.tuple_lit(exprs, &expr),
            node::ExprKind::MapLit(map) => self.map_lit(map),
            node::ExprKind::Call(call) => self.call(call),
            node::ExprKind::BuiltIn(built_in) => self.built_in(built_in, expr.span.end),
            node::ExprKind::BinExpr(bin_expr) => self.bin_expr(bin_expr, expr.ty.size()),
            node::ExprKind::UnExpr(un_expr) => self.un_expr(un_expr, &expr.ty),
            node::ExprKind::PostUnExpr(un_expr) => self.post_un_expr(un_expr),
            node::ExprKind::TypeCast(cast) => self.type_cast(cast, &expr.ty),
        }
    }

    fn r#let(&mut self, decl: &node::Let) {
        let r = self.expr(&decl.expr);
        if r.is_stack() && !self.is_var(&r) {
            self.var_map.insert(decl.name.str.clone(), r);
        } else {
            let t = self.make_stack(r, &decl.expr.ty, decl.name.pos_id);
            self.var_map.insert(decl.name.str.clone(), t);
        }
    }
    fn r#decl(&mut self, decl: &node::Decl) {
        self.stack_count += 1;
        let id = self.stack_count;
        self.var_map.insert(decl.name.str.clone(), Term::Stack(id));
        self.push_op(
            Op::Decl {
                term: Term::Stack(id),
                size: decl.ty.aligned_size(),
            },
            decl.name.pos_id,
        );
    }
    fn is_var(&self, term: &Term) -> bool {
        for var in self.var_map.values() {
            if var == term {
                return true;
            }
        }
        return false;
    }
    fn make_stack(&mut self, term: Term, ty: &Type, pos_id: usize) -> Term {
        let res;
        let size = ty.aligned_size();

        if term.is_stack() && !self.is_var(&term) {
            res = term;
        } else if let Term::Double(_) = term {
            self.stack_count += 1;
            res = Term::Stack(self.stack_count);
            self.push_op(Op::Decl { term: res.clone(), size: 16 }, pos_id);
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: res.clone(),
                    offset: 0,
                    op: "=".to_string(),
                    term,
                    size: 16,
                },
                pos_id,
            );
        } else if let Type::Pointer(_) = ty {
            self.pointer_count += 1;
            res = Term::Pointer(self.pointer_count);
            self.push_op(Op::Let { term, res: res.clone() }, pos_id);
        } else if let Some(s) = &self.stack_return {
            res = s.clone();
            self.clear_res();
        } else if size > 8 {
            if self.is_var(&term) {
                self.stack_count += 1;
                res = Term::Stack(self.stack_count);
                self.push_op(Op::Decl { term: res.clone(), size }, pos_id);
                self.push_op(
                    Op::Copy {
                        from: term,
                        to: res.clone(),
                        size: Term::IntLit(size as i64),
                    },
                    pos_id,
                );
            } else if matches!(term, Term::IntLit(_)) {
                self.stack_count += 1;
                res = Term::Stack(self.stack_count);
                self.push_op(Op::Decl { term: res.clone(), size }, pos_id);
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: res.clone(),
                        offset: 0,
                        op: "=".to_string(),
                        term,
                        size: 8,
                    },
                    pos_id,
                );
            } else {
                res = term;
            }
        } else if let Term::Pointer(_) = term {
            self.stack_count += 1;
            res = Term::Stack(self.stack_count);
            self.temp_count += 1;
            self.push_op(
                Op::Read {
                    res: Term::Temp(self.temp_count),
                    ptr: term,
                    offset: 0,
                    size: 8,
                },
                pos_id,
            );
            self.push_op(
                Op::Let {
                    res: res.clone(),
                    term: Term::Temp(self.temp_count),
                },
                pos_id,
            );
        } else {
            self.stack_count += 1;
            res = Term::Stack(self.stack_count);
            self.push_op(Op::Let { term, res: res.clone() }, pos_id);
        }
        res
    }

    fn r#fn(&mut self, decl: &node::Fn) {
        self.cur_block = Some(Block::new());
        // self.temp_count = 0;
        // self.stack_count = 0;
        self.var_map.clear();
        match self.ir.get(&decl.name.str).cloned() {
            Some(Symbol::Func { ty, args, .. }) | Some(Symbol::MainFunc { ty, args, .. }) => {
                if ty.size() > 16 {
                    self.pointer_count += 1;
                    self.ret_salloc = Some(Term::Pointer(self.pointer_count));
                    self.push_op(
                        Op::Arg {
                            term: Term::Pointer(self.pointer_count),
                            double: false,
                        },
                        decl.name.pos_id,
                    );
                }
                for (s, ty) in decl.arg_names.iter().zip(args) {
                    if ty.size() > 16 {
                        self.pointer_count += 1;
                        let id = self.pointer_count;
                        self.var_map.insert(s.str.clone(), Term::Pointer(id));
                        self.push_op(
                            Op::Arg {
                                term: Term::Pointer(id),
                                double: false,
                            },
                            s.pos_id,
                        );
                    } else if let Type::Pointer(_) = ty {
                        self.pointer_count += 1;
                        let id = self.pointer_count;
                        self.var_map.insert(s.str.clone(), Term::Pointer(id));
                        self.push_op(
                            Op::Arg {
                                term: Term::Pointer(id),
                                double: false,
                            },
                            s.pos_id,
                        );
                    } else {
                        self.stack_count += 1;
                        let id = self.stack_count;
                        self.var_map.insert(s.str.clone(), Term::Stack(id));
                        self.push_op(
                            Op::Arg {
                                term: Term::Stack(id),
                                double: ty.size() > 8,
                            },
                            s.pos_id,
                        );
                    }
                }
            }
            _ => panic!("Couldn't find symbol '{}'", decl.name.str),
        }
        self.scope(&decl.scope, 0);
        self.ret_salloc = None;
        // println!("VAR MAP {}", decl.name.str);
        // println!("{:?}", self.var_map);
        match self.ir.get_mut(&decl.name.str) {
            Some(Symbol::Func { block, .. }) | Some(Symbol::MainFunc { block, .. }) => *block = self.cur_block.clone().unwrap(),
            _ => panic!("Couldn't find symbol '{}'", decl.name.str),
        }
        self.cur_block = None;
    }

    fn scope(&mut self, scope: &Vec<node::Stmt>, offset: i64) {
        if offset != 0 {
            self.push_op(Op::BeginScopeOffset(offset), 0);
        } else {
            self.push_op(Op::BeginScope, 0);
        }
        self.scope_depth += 1;
        for s in scope.iter() {
            self.stmt(s);
        }
        self.scope_depth -= 1;
        self.push_op(Op::EndScope, 0);
    }

    fn ret(&mut self, ret: &node::Ret) {
        if let Some(expr) = &ret.expr {
            if let Some(r) = self.ret_salloc.clone() {
                self.cur_salloc = Some(r.clone());
                let mut from = self.expr(expr);
                if let Some(ret) = &self.stack_return {
                    from = ret.clone();
                    self.clear_res();
                }
                if from.is_stack() {
                    if from != r {
                        self.push_op(
                            Op::Copy {
                                from,
                                to: r.clone(),
                                size: Term::IntLit(ret.expr.as_ref().unwrap().ty.aligned_size() as i64),
                            },
                            ret.pos_id,
                        );
                    }
                }
                self.push_op(Op::Return { term: Some(r) }, ret.pos_id);
                self.cur_salloc = None;
                self.salloc_offset = 0;
            } else {
                let r = self.expr(expr);
                if expr.ty.size() > 8 && !matches!(r, Term::Double(_)) {
                    self.double_count += 1;
                    self.push_op(
                        Op::Read {
                            res: Term::Double(self.double_count),
                            ptr: r,
                            offset: 0,
                            size: 16,
                        },
                        ret.pos_id,
                    );
                    self.push_op(
                        Op::Return {
                            term: Some(Term::Double(self.double_count)),
                        },
                        ret.pos_id,
                    );
                } else {
                    self.push_op(Op::Return { term: Some(r) }, ret.pos_id);
                }
            }
        } else {
            self.push_op(Op::Return { term: None }, ret.pos_id);
        }
    }

    fn r#if(&mut self, r#if: &node::If) {
        let mut if_chain = vec![r#if];
        let mut labels = Vec::new();
        let mut els = &r#if.els;
        while let Some(i) = els {
            if_chain.push(i);
            els = &i.els;
        }
        let mut has_else = false;
        for i in if_chain.iter() {
            match &i.cond {
                node::IfKind::Expr(expr) => {
                    self.push_op(Op::BeginScope, r#if.pos_id);
                    let r = self.expr(expr);
                    self.label_count += 1;
                    labels.push(self.label_count);
                    self.push_op(Op::CondJump { cond: r, label: self.label_count }, i.pos_id);
                }
                node::IfKind::Unpack(unpack) => {
                    let (_, _, ok_label) = self.unpack(unpack, true);
                    self.push_op(Op::EndScope, i.pos_id);
                    labels.push(ok_label);
                }
                node::IfKind::None => {
                    self.label_count += 1;
                    labels.push(self.label_count);
                    self.push_op(Op::Jump { label: self.label_count }, i.pos_id);
                    has_else = true;
                }
            }
        }
        self.label_count += 1;
        let end_label = self.label_count;
        if !has_else {
            self.push_op(Op::Jump { label: end_label }, r#if.pos_id);
        }
        for (i, l) in if_chain.iter().zip(labels) {
            self.push_op(Op::Label { label: l }, i.pos_id);
            let offset = if let node::IfKind::Unpack(u) = &i.cond { u.expr.ty.aligned_size() as i64 } else { 0 };
            self.scope(&i.scope, offset);
            if l != end_label - 1 {
                self.push_op(Op::Jump { label: end_label }, i.pos_id);
            }
        }
        self.push_op(Op::Label { label: end_label }, r#if.pos_id);
    }

    fn bin_expr(&mut self, bin: &node::BinExpr, size: u32) -> Term {
        if let node::ExprKind::UnExpr(u) = &bin.lhs.kind {
            if u.op.str == "*" && bin.is_assign() {
                let term = self.expr(&bin.rhs);
                let ptr = self.expr(&u.expr);
                if bin.rhs.ty.size() > 8 {
                    self.push_op(
                        Op::Copy {
                            from: term,
                            to: ptr,
                            size: Term::IntLit(bin.rhs.ty.aligned_size() as i64),
                        },
                        u.op.pos_id,
                    );
                } else {
                    self.temp_count += 1;
                    self.push_op(
                        Op::Store {
                            res: Some(Term::Temp(self.temp_count)),
                            ptr,
                            offset: 0,
                            op: bin.op.str.clone(),
                            term,
                            size,
                        },
                        u.op.pos_id,
                    );
                }
                return Term::Temp(self.temp_count);
            }
        }
        let lhs = self.expr(&bin.lhs);
        if bin.is_bool() {
            self.label_count += 1;
            let l = self.label_count;
            self.stack_count += 1;
            let s = Term::Stack(self.stack_count);
            self.push_op(Op::Let { res: s.clone(), term: lhs.clone() }, bin.op.pos_id);
            self.push_op(Op::BeginScope, bin.op.pos_id);
            if bin.op.str == "&&" {
                self.temp_count += 1;
                let t = Term::Temp(self.temp_count);
                self.push_op(
                    Op::UnOp {
                        res: t.clone(),
                        op: "!".to_string(),
                        term: s.clone(),
                        size,
                    },
                    bin.op.pos_id,
                );
                self.push_op(Op::CondJump { label: l, cond: t }, bin.op.pos_id);
            } else if bin.op.str == "||" {
                self.push_op(Op::CondJump { label: l, cond: s.clone() }, bin.op.pos_id);
            }
            let r = self.expr(&bin.rhs);
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: s.clone(),
                    offset: 0,
                    op: "=".to_string(),
                    term: r,
                    size: 1,
                },
                bin.op.pos_id,
            );
            self.push_op(Op::Label { label: l }, bin.op.pos_id);
            return s;
        }
        if bin.is_assign() {
            self.cur_salloc = Some(lhs.clone());
        }
        let rhs = self.expr(&bin.rhs);
        if bin.is_assign() {
            self.cur_salloc = None;
        }
        self.temp_count += 1;
        if let Term::Double(_) = lhs {
            self.stack_count += 1;
            self.push_op(
                Op::Decl {
                    term: Term::Stack(self.stack_count),
                    size: 16,
                },
                bin.op.pos_id,
            );
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: Term::Stack(self.stack_count),
                    offset: 0,
                    size: 8,
                    op: "=".to_string(),
                    term: lhs.clone(),
                },
                bin.op.pos_id,
            );
            self.push_op(
                Op::UnOp {
                    res: Term::Temp(self.temp_count),
                    op: "&".to_string(),
                    term: Term::Stack(self.stack_count),
                    size: 8,
                },
                bin.op.pos_id,
            );
            self.temp_count += 1;
            self.push_op(
                Op::BinOp {
                    res: Some(Term::Temp(self.temp_count)),
                    lhs: Term::Temp(self.temp_count - 1),
                    op: "+".to_string(),
                    rhs,
                    size,
                },
                bin.op.pos_id,
            );
        } else if bin.is_assign() {
            let is_stack = if let Term::Stack(_) = lhs { true } else { false };
            if bin.lhs.ty.size() > 8 && is_stack {
                if lhs != rhs {
                    self.push_op(
                        Op::Copy {
                            from: rhs,
                            to: lhs,
                            size: Term::IntLit(bin.lhs.ty.aligned_size() as i64),
                        },
                        bin.op.pos_id,
                    );
                }
            } else if let Term::Pointer(p) = lhs {
                self.push_op(
                    Op::Store {
                        res: Some(Term::Temp(self.temp_count)),
                        ptr: Term::PointerArithmetic(p),
                        offset: 0,
                        op: bin.op.str.clone(),
                        term: rhs,
                        size,
                    },
                    bin.op.pos_id,
                );
            } else {
                self.push_op(
                    Op::Store {
                        res: Some(Term::Temp(self.temp_count)),
                        ptr: lhs,
                        offset: 0,
                        op: bin.op.str.clone(),
                        term: rhs,
                        size,
                    },
                    bin.op.pos_id,
                );
            }
        } else {
            let is_stack = if let Term::Stack(_) = lhs { true } else { false };
            let size = if bin.is_cmp() { bin.lhs.ty.size() } else { size };
            if bin.lhs.ty.salloc() && is_stack {
                self.push_op(
                    Op::UnOp {
                        res: Term::Temp(self.temp_count),
                        op: "&".to_string(),
                        term: lhs,
                        size,
                    },
                    bin.op.pos_id,
                );
                self.temp_count += 1;
                self.push_op(
                    Op::BinOp {
                        res: Some(Term::Temp(self.temp_count)),
                        lhs: Term::Temp(self.temp_count - 1),
                        op: bin.op.str.clone(),
                        rhs,
                        size,
                    },
                    bin.op.pos_id,
                );
            } else {
                self.push_op(
                    Op::BinOp {
                        res: Some(Term::Temp(self.temp_count)),
                        lhs,
                        op: bin.op.str.clone(),
                        rhs,
                        size,
                    },
                    bin.op.pos_id,
                );
            }
        }
        Term::Temp(self.temp_count)
    }

    fn un_expr(&mut self, un: &node::UnExpr, ty: &Type) -> Term {
        let size = ty.aligned_size();
        if un.op.str == "*" && size <= 8 {
            if let node::ExprKind::BinExpr(bin) = &un.expr.kind {
                if bin.op.str == "+" {
                    if let node::ExprKind::IntLit(i) = bin.rhs.kind {
                        let ptr = self.expr(&bin.lhs);
                        self.temp_count += 1;
                        self.push_op(
                            Op::Read {
                                res: Term::Temp(self.temp_count),
                                ptr,
                                offset: i,
                                size,
                            },
                            un.op.pos_id,
                        );
                        return Term::Temp(self.temp_count);
                    }
                }
            }
        }
        let term = self.expr(&un.expr);
        if un.op.str == "*" {
            if size > 16 {
                self.stack_count += 1;
                let s = Term::Stack(self.stack_count);
                self.push_op(Op::Decl { term: s.clone(), size }, un.op.pos_id);
                self.push_op(
                    Op::Copy {
                        from: term,
                        to: s.clone(),
                        size: Term::IntLit(size as i64),
                    },
                    un.op.pos_id,
                );
                return s;
            } else if size > 8 {
                self.double_count += 1;
                let d = Term::Double(self.double_count);
                self.push_op(
                    Op::Read {
                        res: d.clone(),
                        ptr: term,
                        offset: 0,
                        size,
                    },
                    un.op.pos_id,
                );
                return d;
            }
        }
        if un.op.str.starts_with("+") {
            let alloc_fn = un.op.str.split("+").last().unwrap().to_string();
            let Type::Slice { inner } = &un.expr.ty else {
                unreachable!();
            };
            let p;
            let pa;
            let s;
            let len;
            self.pointer_count += 1;
            p = Term::Pointer(self.pointer_count);
            pa = Term::PointerArithmetic(self.pointer_count);
            if term.is_stack() && !self.is_var(&term) {
                self.push_op(
                    Op::Own {
                        res: p.clone(),
                        term: term.clone(),
                        offset: 8,
                    },
                    un.op.pos_id,
                );
                s = term.clone();
                self.push_op(Op::BeginScope, un.op.pos_id);
                self.stack_count += 1;
                len = Term::Stack(self.stack_count);
                self.push_op(Op::Let { res: len.clone(), term: term.clone() }, un.op.pos_id);
            } else {
                self.push_op(Op::Decl { term: p.clone(), size: 8 }, un.op.pos_id);
                s = self.make_stack(term.clone(), &Type::Int, un.op.pos_id);
                self.push_op(Op::BeginScope, un.op.pos_id);
                len = self.make_stack(term.clone(), &Type::Int, un.op.pos_id);
            }
            if inner.size() != 1 {
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: len.clone(),
                        offset: 0,
                        op: "*=".to_string(),
                        term: Term::IntLit(inner.size() as i64),
                        size: 8,
                    },
                    un.op.pos_id,
                );
            }
            self.push_op(Op::BeginCall { params: vec![len.clone()] }, un.op.pos_id);
            self.push_op(Op::Param { term: len.clone() }, un.op.pos_id);
            self.temp_count += 1;
            self.push_op(
                Op::Call {
                    res: Some(Term::Temp(self.temp_count)),
                    func: alloc_fn,
                },
                un.op.pos_id,
            );
            if s == term {
                self.pointer_count += 1;
                self.push_op(
                    Op::Let {
                        res: Term::Pointer(self.pointer_count),
                        term: Term::Temp(self.temp_count),
                    },
                    un.op.pos_id,
                );
                self.temp_count += 1;
                self.push_op(
                    Op::Read {
                        res: Term::Temp(self.temp_count),
                        ptr: term.clone(),
                        offset: 8,
                        size: 8,
                    },
                    un.op.pos_id,
                );
                self.push_op(
                    Op::Copy {
                        from: Term::Temp(self.temp_count),
                        to: Term::Pointer(self.pointer_count),
                        size: len.clone(),
                    },
                    un.op.pos_id,
                );
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: pa,
                        offset: 0,
                        op: "=".to_string(),
                        term: Term::Pointer(self.pointer_count),
                        size: 8,
                    },
                    un.op.pos_id,
                );
            } else {
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: pa,
                        offset: 0,
                        op: "=".to_string(),
                        term: Term::Temp(self.temp_count),
                        size: 8,
                    },
                    un.op.pos_id,
                );
                self.temp_count += 1;
                self.push_op(
                    Op::Read {
                        res: Term::Temp(self.temp_count),
                        ptr: term.clone(),
                        offset: 8,
                        size: 8,
                    },
                    un.op.pos_id,
                );
                self.push_op(
                    Op::Copy {
                        from: Term::Temp(self.temp_count),
                        to: p.clone(),
                        size: len.clone(),
                    },
                    un.op.pos_id,
                );
            }
            self.push_op(Op::EndScope, un.op.pos_id);
            return s;
        }
        if un.op.str == "?" {
            let Type::Result(_, err) = ty else {
                unreachable!();
            };
            self.stack_count += 1;
            let s = Term::Stack(self.stack_count);
            self.push_op(
                Op::Decl {
                    term: s.clone(),
                    size: Type::align(err.aligned_size()) + 8,
                },
                un.op.pos_id,
            );
            self.push_op(
                Op::Store {
                    res: None,
                    ptr: s.clone(),
                    offset: 0,
                    op: "=".to_string(),
                    term: Term::IntLit(1),
                    size: 8,
                },
                un.op.pos_id,
            );
            self.temp_count += 1;
            let t = Term::Temp(self.temp_count);
            self.push_op(
                Op::UnOp {
                    res: t.clone(),
                    op: "&".to_string(),
                    term: s.clone(),
                    size: 8,
                },
                un.op.pos_id,
            );
            self.temp_count += 1;
            let t2 = Term::Temp(self.temp_count);
            self.push_op(
                Op::BinOp {
                    res: Some(t2.clone()),
                    lhs: t,
                    op: "+".to_string(),
                    rhs: Term::IntLit(8),
                    size: 8,
                },
                un.op.pos_id,
            );
            self.push_op(
                Op::Copy {
                    from: term,
                    to: t2,
                    size: Term::IntLit(err.aligned_size() as i64),
                },
                un.op.pos_id,
            );
            return s;
        }
        if un.op.str == "&" {
            if let Term::Pointer(_) = term {
                return term;
            }
        }
        self.temp_count += 1;
        self.push_op(
            Op::UnOp {
                res: Term::Temp(self.temp_count),
                op: un.op.str.clone(),
                term,
                size,
            },
            un.op.pos_id,
        );
        Term::Temp(self.temp_count)
    }

    fn unpack(&mut self, unpack: &node::Unpack, insert_scope: bool) -> (Term, Term, u16) {
        if insert_scope {
            self.push_op(Op::BeginScope, unpack.lhs.pos_id);
        }
        let mut t = self.expr(&unpack.expr);
        let mut cmp = None;
        if let Some(name) = &unpack.rhs {
            let Type::Enum(vars) = &unpack.expr.ty else {
                panic!("Not enum {}", unpack.expr.ty);
            };
            let Some(pos) = vars.iter().position(|(p, ty)| *p == name.str && ty.is_some() == unpack.brackets.is_some()) else {
                unreachable!();
            };
            cmp = Some(Term::IntLit(pos as i64));
            t = self.make_stack(t, &unpack.expr.ty, unpack.lhs.pos_id);
            if let Some(s) = &unpack.brackets {
                self.stack_count += 1;
                let t2 = Term::Stack(self.stack_count);
                self.push_op(
                    Op::Own {
                        res: t2.clone(),
                        term: t.clone(),
                        offset: 8,
                    },
                    unpack.lhs.pos_id,
                );
                self.var_map.insert(s.str.clone(), t2);
            }
        } else {
            if matches!(&unpack.expr.ty, Type::Result(_, _) | Type::Option(_)) {
                t = self.make_stack(t, &unpack.expr.ty, unpack.lhs.pos_id);
                self.stack_count += 1;
                let t2 = Term::Stack(self.stack_count);
                self.push_op(
                    Op::Own {
                        res: t2.clone(),
                        term: t.clone(),
                        offset: 8,
                    },
                    unpack.lhs.pos_id,
                );
                self.var_map.insert(unpack.lhs.str.clone(), t2);
            } else {
                unreachable!();
            }
        }
        self.unwrap(t, cmp, unpack.expr.ty.aligned_size(), unpack.lhs.pos_id)
    }

    fn unwrap(&mut self, t: Term, cmp: Option<Term>, aligned: u32, pos_id: usize) -> (Term, Term, u16) {
        self.label_count += 1;
        let ret_label = self.label_count;
        self.label_count += 1;
        let ok_label = self.label_count;
        let s;
        let inner;
        if let Term::Temp(_) = t {
            let Some(ret) = &self.stack_return else {
                panic!("No stack return");
            };
            self.stack_count += 1;
            s = ret.clone();
            inner = Term::Stack(self.stack_count);
            self.clear_res();
            self.push_op(
                Op::Own {
                    res: inner.clone(),
                    term: s.clone(),
                    offset: 8,
                },
                pos_id,
            );
        } else if let Term::Double(_) = t {
            self.stack_count += 1;
            s = Term::Stack(self.stack_count);
            self.stack_count += 1;
            inner = Term::Stack(self.stack_count);
            self.push_op(Op::Decl { term: s.clone(), size: 16 }, pos_id);
            self.push_op(
                Op::Read {
                    res: t.clone(),
                    ptr: s.clone(),
                    offset: 0,
                    size: 16,
                },
                pos_id,
            );
            self.push_op(
                Op::Own {
                    res: inner.clone(),
                    term: s.clone(),
                    offset: 8,
                },
                pos_id,
            );
        } else if let Term::Stack(_) = t {
            s = t;
            self.stack_count += 1;
            inner = Term::Stack(self.stack_count);
            self.push_op(
                Op::Own {
                    res: inner.clone(),
                    term: s.clone(),
                    offset: 8,
                },
                pos_id,
            );
        } else if let Term::Pointer(_) = t {
            self.stack_count += 1;
            s = Term::Stack(self.stack_count);
            self.stack_count += 1;
            inner = Term::Stack(self.stack_count);
            self.push_op(Op::Decl { term: s.clone(), size: aligned }, pos_id);
            self.push_op(
                Op::Copy {
                    from: t,
                    to: s.clone(),
                    size: Term::IntLit(aligned as i64),
                },
                pos_id,
            );
            self.push_op(
                Op::Own {
                    res: inner.clone(),
                    term: s.clone(),
                    offset: 8,
                },
                pos_id,
            );
        } else {
            panic!("Not temp or double {t:?}");
        }
        self.push_op(Op::BeginScope, pos_id);
        if let Some(c) = cmp {
            self.temp_count += 1;
            self.push_op(
                Op::BinOp {
                    res: Some(Term::Temp(self.temp_count)),
                    lhs: s.clone(),
                    op: "!=".to_string(),
                    rhs: c,
                    size: 8,
                },
                pos_id,
            );
            self.push_op(
                Op::CondJump {
                    label: ret_label,
                    cond: Term::Temp(self.temp_count),
                },
                pos_id,
            );
        } else {
            self.push_op(Op::CondJump { label: ret_label, cond: s.clone() }, pos_id);
        }
        self.push_op(Op::Jump { label: ok_label }, pos_id);
        self.push_op(Op::Label { label: ret_label }, pos_id);
        (s, inner, ok_label)
    }

    fn post_un_expr(&mut self, un: &node::UnExpr) -> Term {
        let t = self.expr(&un.expr);
        let inner_size = un.expr.ty.size();
        let aligned = un.expr.ty.aligned_size();
        if un.op.str.starts_with("!") {
            let (_, inner, ok_label) = self.unwrap(t, None, aligned, un.op.pos_id);
            let panic = un.op.str.split("!").last().unwrap();
            self.push_op(Op::BeginCall { params: vec![] }, un.op.pos_id);
            if let Type::Result(_, err) = &un.expr.ty {
                for p in self.param(inner.clone(), err.size(), err.aligned_size(), un.op.pos_id) {
                    self.push_op(Op::Param { term: p }, un.op.pos_id);
                }
                self.push_op(Op::Call { res: None, func: panic.to_string() }, un.op.pos_id);
                self.push_op(Op::Label { label: ok_label }, un.op.pos_id);
                inner
            } else if let Type::Option(_) = &un.expr.ty {
                self.push_op(Op::Call { res: None, func: panic.to_string() }, un.op.pos_id);
                self.push_op(Op::Label { label: ok_label }, un.op.pos_id);
                inner
            } else {
                unreachable!();
            }
        } else {
            match un.op.str.as_str() {
                "?" => {
                    let (s, inner, ok_label) = self.unwrap(t, None, aligned, un.op.pos_id);
                    if let Some(r) = self.ret_salloc.clone() {
                        self.push_op(
                            Op::Copy {
                                from: s,
                                to: r.clone(),
                                size: Term::IntLit(inner_size as i64),
                            },
                            un.op.pos_id,
                        );
                        self.push_op(Op::Return { term: Some(r) }, un.op.pos_id);
                    } else {
                        self.push_op(
                            Op::Read {
                                res: Term::Double(self.double_count),
                                ptr: s,
                                offset: 0,
                                size: 16,
                            },
                            un.op.pos_id,
                        );
                        self.push_op(
                            Op::Return {
                                term: Some(Term::Double(self.double_count)),
                            },
                            un.op.pos_id,
                        );
                    }
                    self.push_op(Op::Label { label: ok_label }, un.op.pos_id);
                    inner
                }
                _ => unreachable!(),
            }
        }
    }

    fn else_scope(&mut self, r#else: &node::ElseScope) {
        let (_, inner, ok_label) = self.unpack(&r#else.unpack, false);
        if let Some(c) = &r#else.capture {
            self.var_map.insert(c.str.clone(), inner.clone());
        }
        self.scope(&r#else.scope, 0);
        if let Some(c) = &r#else.capture {
            self.var_map.remove(&c.str);
        }
        self.push_op(Op::Label { label: ok_label }, r#else.pos_str.pos_id);
    }

    fn else_expr(&mut self, r#else: &node::ElseExpr) {
        let (_, inner, ok_label) = self.unpack(&r#else.unpack, false);
        self.push_op(Op::BeginScope, r#else.pos_str.pos_id);
        let els = self.expr(&r#else.else_expr);
        self.push_op(
            Op::Copy {
                from: els,
                to: inner.clone(),
                size: Term::IntLit(r#else.else_expr.ty.size() as i64),
            },
            r#else.pos_str.pos_id,
        );
        self.push_op(Op::EndScope, r#else.pos_str.pos_id);
        self.push_op(Op::Label { label: ok_label }, r#else.pos_str.pos_id);
    }

    fn param(&mut self, term: Term, size: u32, aligned: u32, pos_id: usize) -> Vec<Term> {
        let mut params = vec![];
        if let Term::Stack(_) = term {
            if self.var_map.values().any(|v| *v == term) {
                if size > 16 {
                    self.stack_count += 1;
                    self.temp_count += 1;
                    self.push_op(
                        Op::Decl {
                            term: Term::Stack(self.stack_count),
                            size: aligned,
                        },
                        pos_id,
                    );
                    self.push_op(
                        Op::Copy {
                            from: term.clone(),
                            to: Term::Stack(self.stack_count),
                            size: Term::IntLit(aligned as i64),
                        },
                        pos_id,
                    );
                    self.push_op(
                        Op::UnOp {
                            res: Term::Temp(self.temp_count),
                            op: "&".to_string(),
                            term: Term::Stack(self.stack_count),
                            size: 8,
                        },
                        pos_id,
                    );
                } else {
                    self.temp_count += 1;
                    self.push_op(
                        Op::Read {
                            res: Term::Temp(self.temp_count),
                            ptr: term.clone(),
                            offset: 0,
                            size,
                        },
                        pos_id,
                    );
                    if size > 8 {
                        self.temp_count += 1;
                        self.push_op(
                            Op::Read {
                                res: Term::Temp(self.temp_count),
                                ptr: term,
                                offset: 8,
                                size: size - 8,
                            },
                            pos_id,
                        );
                        params.push(Term::Temp(self.temp_count - 1));
                    }
                }
            } else {
                if size > 16 {
                    self.temp_count += 1;
                    self.push_op(
                        Op::UnOp {
                            res: Term::Temp(self.temp_count),
                            op: "&".to_string(),
                            term: term.clone(),
                            size: 8,
                        },
                        pos_id,
                    );
                } else {
                    self.temp_count += 1;
                    self.push_op(
                        Op::Read {
                            res: Term::Temp(self.temp_count),
                            ptr: term.clone(),
                            offset: 0,
                            size: size,
                        },
                        pos_id,
                    );
                    if size > 8 {
                        // panic!("{:?}", term);
                        self.temp_count += 1;
                        self.push_op(
                            Op::Read {
                                res: Term::Temp(self.temp_count),
                                ptr: term,
                                offset: 8,
                                size: size - 8,
                            },
                            pos_id,
                        );
                        params.push(Term::Temp(self.temp_count - 1));
                    }
                }
            }
            params.push(Term::Temp(self.temp_count));
        } else {
            params.push(term);
        }
        params
    }

    fn call(&mut self, call: &node::Call) -> Term {
        let mut res = None;
        let mut is_double = false;
        let mut params = Vec::new();
        match self.ir.get(&call.name.str) {
            Some(Symbol::Func { ty, .. }) | Some(Symbol::ExternFunc { ty, .. }) | Some(Symbol::Syscall { ty, .. }) => {
                if ty.size() > 16 {
                    // Pass pointer as first argument
                    self.stack_count += 1;
                    let r = Term::Stack(self.stack_count);
                    self.push_op(
                        Op::Decl {
                            term: r.clone(),
                            size: ty.aligned_size(),
                        },
                        call.name.pos_id,
                    );
                    res = Some(r);
                } else if ty.size() > 8 {
                    is_double = true;
                }
            }
            _ => (),
        }
        let idx = self.cur_block.as_ref().unwrap().ops.len();
        self.push_op(Op::BeginCall { params: vec![] }, call.name.pos_id);
        if let Some(r) = &res {
            self.temp_count += 1;
            self.push_op(
                Op::UnOp {
                    res: Term::Temp(self.temp_count),
                    op: "&".to_string(),
                    term: r.clone(),
                    size: 8,
                },
                call.name.pos_id,
            );
            params.push(Term::Temp(self.temp_count));
        }
        for arg in call.args.iter() {
            let r = self.expr(arg);
            for p in self.param(r, arg.ty.size(), arg.ty.aligned_size(), call.name.pos_id) {
                params.push(p);
            }
        }
        for p in params.iter() {
            self.push_op(Op::Param { term: p.clone() }, call.name.pos_id);
        }
        if let Op::BeginCall { params: ps } = &mut self.cur_block.as_mut().unwrap().ops[idx] {
            *ps = params;
        } else {
            unreachable!("Expected BeginCall at index {}", idx);
        }
        if let Some(r) = res.clone() {
            self.stack_return = Some(r);
        }
        let res;
        if is_double {
            self.double_count += 1;
            res = Term::Double(self.double_count);
        } else {
            self.temp_count += 1;
            res = Term::Temp(self.temp_count);
        }
        self.push_op(
            Op::Call {
                func: call.name.str.clone(),
                res: Some(res.clone()),
            },
            call.name.pos_id,
        );
        res
    }

    fn built_in(&mut self, built_in: &node::BuiltIn, pos_id: usize) -> Term {
        match built_in.kind {
            node::BuiltInKind::Len => {
                let expr = built_in.args.get(0).unwrap();
                if let Type::Array { length, .. } = expr.ty {
                    Term::IntLit(length as i64)
                } else {
                    let s = self.expr(expr);
                    if let Term::Stack(_) = s {
                        s
                    } else {
                        self.temp_count += 1;
                        self.push_op(
                            Op::Read {
                                res: Term::Temp(self.temp_count),
                                ptr: s,
                                offset: 0,
                                size: 8,
                            },
                            pos_id,
                        );
                        Term::Temp(self.temp_count)
                    }
                }
            }
            node::BuiltInKind::Copy => {
                let from = self.expr(built_in.args.get(0).unwrap());
                let to = self.expr(built_in.args.get(1).unwrap());
                let size = self.expr(built_in.args.get(2).unwrap());
                self.push_op(Op::Copy { from, to, size }, pos_id);
                Term::IntLit(0)
            }
            node::BuiltInKind::StackPointer => {
                self.temp_count += 1;
                self.push_op(Op::StackPointer { res: Term::Temp(self.temp_count) }, pos_id);
                Term::Temp(self.temp_count)
            }
            node::BuiltInKind::Sizeof => {
                let expr = built_in.args.get(0).unwrap();
                Term::IntLit(expr.ty.size() as i64)
            }
            node::BuiltInKind::Param => {
                let expr = built_in.args.get(0).unwrap();
                let param = self.expr(expr);
                for p in self.param(param, expr.ty.size(), expr.ty.aligned_size(), pos_id) {
                    self.push_op(Op::Param { term: p }, pos_id);
                }
                Term::IntLit(0)
            }
        }
    }

    fn r#loop(&mut self, r#loop: &node::Loop) {
        self.label_count += 2;
        let s = self.label_count - 1;
        self.loop_start.push(s);
        let e = self.label_count;
        self.loop_exit.push((e, self.scope_depth));
        self.push_op(Op::Label { label: s }, r#loop.pos_id);
        self.push_op(Op::BeginLoop, 0);
        self.scope(&r#loop.scope, 0);
        self.push_op(Op::EndLoop, 0);
        self.push_op(Op::Jump { label: s }, r#loop.pos_id);
        self.push_op(Op::Label { label: e }, r#loop.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }
    fn r#while(&mut self, r#while: &node::While) {
        self.label_count += 3;
        let s = self.label_count - 2;
        let c = self.label_count - 1;
        let e = self.label_count;
        self.loop_start.push(s);
        self.loop_exit.push((e, self.scope_depth));
        self.push_op(Op::Jump { label: c }, r#while.pos_id);
        self.push_op(Op::Label { label: s }, r#while.pos_id);
        self.push_op(Op::BeginLoop, r#while.pos_id);
        self.scope(&r#while.scope, 0);
        self.push_op(Op::EndLoop, r#while.pos_id);
        self.push_op(Op::Label { label: c }, r#while.pos_id);
        self.push_op(Op::BeginScope, r#while.pos_id);
        let cond = self.expr(&r#while.expr);
        self.push_op(Op::CondJump { cond, label: s }, r#while.pos_id);
        self.push_op(Op::Label { label: e }, r#while.pos_id);
        self.push_op(Op::NaturalFlow, r#while.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }

    fn r#for(&mut self, r#for: &node::For) {
        match &r#for.init {
            node::LetOrExpr::Let(r#let) => self.r#let(r#let),
            node::LetOrExpr::Expr(expr) => {
                self.expr(expr);
            }
        }
        self.label_count += 3;
        let s = self.label_count - 2;
        let c = self.label_count - 1;
        let e = self.label_count;
        self.loop_start.push(c);
        self.loop_exit.push((e, self.scope_depth));
        self.push_op(Op::Label { label: s }, r#for.pos_id);
        self.push_op(Op::BeginLoop, 0);
        self.scope(&r#for.scope, 0);
        self.push_op(Op::EndLoop, 0);
        self.push_op(Op::Label { label: c }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.push_op(Op::BeginScope, r#for.pos_id);
        self.expr(&r#for.incr);
        self.clear_res();
        let cond = self.expr(&r#for.cond);
        self.push_op(Op::CondJump { cond, label: s }, r#for.pos_id);
        self.push_op(Op::Label { label: e }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }

    fn for_in(&mut self, r#for: &node::ForIn) {
        self.push_op(Op::BeginScope, r#for.pos_id);
        let t = self.expr(&r#for.expr);
        let st = self.make_stack(t, &r#for.expr.ty, r#for.pos_id);
        self.label_count += 4;
        let s = self.label_count - 3;
        let c = self.label_count - 2;
        let i = self.label_count - 1;
        let e = self.label_count;
        self.loop_start.push(c);
        self.loop_exit.push((e, self.scope_depth));
        match &r#for.expr.ty {
            Type::Slice { inner } => self.for_in_slice(r#for, st, inner, s, i, c),
            Type::Range => self.for_in_range(r#for, st, s, i, c),
            _ => panic!("not slice"),
        };
        self.push_op(Op::Label { label: e }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
        self.var_map.remove(&r#for.capture.str);
        self.push_op(Op::EndScope, r#for.pos_id);
    }
    fn for_in_slice(&mut self, r#for: &node::ForIn, sl: Term, inner: &Type, s: u16, i: u16, c: u16) {
        self.pointer_count += 1;
        let ptr = Term::Pointer(self.pointer_count);
        let pa = Term::PointerArithmetic(self.pointer_count);
        self.push_op(
            Op::Own {
                res: ptr.clone(),
                term: sl.clone(),
                offset: 8,
            },
            r#for.pos_id,
        );
        self.stack_count += 1;
        let el = Term::Stack(self.stack_count);
        self.push_op(
            Op::Decl {
                term: el.clone(),
                size: inner.aligned_size(),
            },
            r#for.pos_id,
        );
        let size = Term::IntLit(inner.size() as i64);
        self.var_map.insert(r#for.capture.str.clone(), el.clone());
        self.push_op(Op::Jump { label: i }, r#for.pos_id);
        self.push_op(Op::Label { label: s }, r#for.pos_id);
        self.push_op(Op::BeginLoop, 0);
        self.push_op(
            Op::Copy {
                from: ptr,
                to: el,
                size: size.clone(),
            },
            r#for.pos_id,
        );
        self.scope(&r#for.scope, 0);
        self.push_op(Op::EndLoop, 0);
        self.push_op(Op::Label { label: c }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.push_op(
            Op::Store {
                res: None,
                ptr: pa,
                offset: 0,
                op: "+=".to_string(),
                term: size.clone(),
                size: 8,
            },
            r#for.pos_id,
        );
        self.push_op(
            Op::Store {
                res: None,
                ptr: sl.clone(),
                offset: 0,
                op: "-=".to_string(),
                term: Term::IntLit(1),
                size: 8,
            },
            r#for.pos_id,
        );
        self.push_op(Op::Label { label: i }, r#for.pos_id);
        self.push_op(Op::BeginScope, r#for.pos_id);
        self.push_op(Op::CondJump { cond: sl, label: s }, r#for.pos_id);
    }

    fn for_in_range(&mut self, r#for: &node::ForIn, range: Term, s: u16, i: u16, c: u16) {
        self.stack_count += 1;
        let end = Term::Stack(self.stack_count);
        self.push_op(
            Op::Own {
                res: end.clone(),
                term: range.clone(),
                offset: 8,
            },
            r#for.pos_id,
        );
        self.var_map.insert(r#for.capture.str.clone(), range.clone());
        self.push_op(Op::Jump { label: i }, r#for.pos_id);
        self.push_op(Op::Label { label: s }, r#for.pos_id);
        self.push_op(Op::BeginLoop, 0);
        self.scope(&r#for.scope, 0);
        self.push_op(Op::EndLoop, 0);
        self.push_op(Op::Label { label: c }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.push_op(
            Op::Store {
                res: None,
                ptr: range.clone(),
                offset: 0,
                op: "+=".to_string(),
                term: Term::IntLit(1),
                size: 8,
            },
            r#for.pos_id,
        );
        self.push_op(Op::Label { label: i }, r#for.pos_id);
        self.push_op(Op::BeginScope, r#for.pos_id);
        self.temp_count += 1;
        self.push_op(
            Op::BinOp {
                res: Some(Term::Temp(self.temp_count)),
                lhs: range,
                op: "<".to_string(),
                rhs: end,
                size: 8,
            },
            r#for.pos_id,
        );
        self.push_op(
            Op::CondJump {
                cond: Term::Temp(self.temp_count),
                label: s,
            },
            r#for.pos_id,
        );
    }

    fn type_cast(&mut self, cast: &node::TypeCast, to: &Type) -> Term {
        let r = self.expr(&cast.expr);
        let id = cast.expr.span.end;
        let size = to.size();
        let res;
        match (&cast.expr.ty, to) {
            (Type::Array { inner: _, length }, Type::Slice { inner: _ }) => {
                self.stack_count += 1;
                let s = Term::Stack(self.stack_count);
                self.temp_count += 1;
                self.push_op(Op::Decl { term: s.clone(), size: 16 }, id);
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: s.clone(),
                        offset: 0,
                        op: "=".to_string(),
                        term: Term::IntLit(*length as i64),
                        size: to.size(),
                    },
                    id,
                );
                if let Term::Pointer(p) = r {
                    self.push_op(
                        Op::Store {
                            res: None,
                            ptr: s.clone(),
                            offset: 8,
                            op: "=".to_string(),
                            term: Term::PointerArithmetic(p),
                            size: 8,
                        },
                        id,
                    );
                } else {
                    self.push_op(
                        Op::UnOp {
                            res: Term::Temp(self.temp_count),
                            op: "&".to_string(),
                            term: r,
                            size,
                        },
                        id,
                    );
                    self.push_op(
                        Op::Store {
                            res: None,
                            ptr: s.clone(),
                            offset: 8,
                            op: "=".to_string(),
                            term: Term::Temp(self.temp_count),
                            size: to.size(),
                        },
                        id,
                    );
                }
                res = s;
            }
            (Type::Slice { .. }, Type::Pointer(_)) => {
                self.temp_count += 1;
                self.push_op(
                    Op::Read {
                        res: Term::Temp(self.temp_count),
                        ptr: r,
                        offset: 8,
                        size,
                    },
                    id,
                );
                res = Term::Temp(self.temp_count);
            }
            (Type::Struct(s1), Type::Struct(s2)) => {
                let mut ret = true;
                if s1.len() != s2.len() {
                    ret = false;
                } else {
                    for (n2, (_, o2)) in s2.iter() {
                        // Check if struct already matches
                        let Some((_, o1)) = s1.get(n2) else {
                            unreachable!();
                        };
                        if o1 != o2 {
                            ret = false;
                            break;
                        }
                    }
                }
                if ret {
                    res = r;
                } else {
                    self.stack_count += 1;
                    let s = Term::Stack(self.stack_count);
                    self.pointer_count += 1;
                    self.push_op(
                        Op::Decl {
                            term: s.clone(),
                            size: to.aligned_size(),
                        },
                        id,
                    );
                    for (n2, (t2, o2)) in s2.iter() {
                        let Some((_, o1)) = s1.get(n2) else {
                            unreachable!();
                        };
                        self.temp_count += 1;
                        self.push_op(
                            Op::UnOp {
                                res: Term::Temp(self.temp_count),
                                op: "&".to_string(),
                                term: r.clone(),
                                size: 8,
                            },
                            id,
                        );
                        self.temp_count += 1;
                        self.push_op(
                            Op::BinOp {
                                res: Some(Term::Temp(self.temp_count)),
                                lhs: Term::Temp(self.temp_count - 1),
                                op: "+".to_string(),
                                rhs: Term::IntLit(*o1 as i64),
                                size: 8,
                            },
                            id,
                        );
                        self.temp_count += 1;
                        self.push_op(
                            Op::UnOp {
                                res: Term::Temp(self.temp_count),
                                op: "&".to_string(),
                                term: s.clone(),
                                size: 8,
                            },
                            id,
                        );
                        self.temp_count += 1;
                        self.push_op(
                            Op::BinOp {
                                res: Some(Term::Temp(self.temp_count)),
                                lhs: Term::Temp(self.temp_count - 1),
                                op: "+".to_string(),
                                rhs: Term::IntLit(*o2 as i64),
                                size: 8,
                            },
                            id,
                        );

                        self.push_op(
                            Op::Copy {
                                from: Term::Temp(self.temp_count - 2),
                                to: Term::Temp(self.temp_count),
                                size: Term::IntLit(t2.size() as i64),
                            },
                            id,
                        );
                    }
                    res = s;
                }
            }
            (Type::Option(lhs), Type::Option(rhs)) => {
                if lhs.aligned_size() >= rhs.aligned_size() {
                    res = r;
                } else {
                    self.stack_count += 1;
                    let s = Term::Stack(self.stack_count);
                    self.push_op(
                        Op::Decl {
                            term: s.clone(),
                            size: rhs.aligned_size(),
                        },
                        id,
                    );
                    self.push_op(
                        Op::Copy {
                            from: r,
                            to: s.clone(),
                            size: Term::IntLit(rhs.aligned_size() as i64),
                        },
                        id,
                    );
                    res = s;
                }
            }
            (_, Type::Result(ok, _) | Type::Option(ok)) => {
                self.stack_count += 1;
                let s = Term::Stack(self.stack_count);
                self.push_op(
                    Op::Decl {
                        term: s.clone(),
                        size: to.aligned_size(),
                    },
                    id,
                );
                self.push_op(
                    Op::Store {
                        res: None,
                        ptr: s.clone(),
                        offset: 0,
                        op: "=".to_string(),
                        term: Term::IntLit(0),
                        size: 8,
                    },
                    id,
                );
                self.temp_count += 1;
                let t = Term::Temp(self.temp_count);
                self.push_op(
                    Op::UnOp {
                        res: t.clone(),
                        op: "&".to_string(),
                        term: s.clone(),
                        size: 8,
                    },
                    id,
                );
                self.temp_count += 1;
                let t2 = Term::Temp(self.temp_count);
                self.push_op(
                    Op::BinOp {
                        res: Some(t2.clone()),
                        lhs: t,
                        op: "+".to_string(),
                        rhs: Term::IntLit(8),
                        size: 8,
                    },
                    id,
                );
                self.push_op(
                    Op::Copy {
                        from: r,
                        to: t2,
                        size: Term::IntLit(ok.aligned_size() as i64),
                    },
                    id,
                );
                res = s;
            }
            _ => res = r,
        }
        res
    }
}
