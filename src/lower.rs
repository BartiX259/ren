use std::collections::HashMap;

use crate::ir::{Block, Symbol, Term, Op, OpLoc};
use crate::types::Type;
use crate::{node, types};
use crate::helpers::StringLit;

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
    cur_symbols: Vec<Vec<(String, Symbol)>>,
    cur_block: Option<Block>,
    var_map: HashMap<String, Term>,
    temp_count: usize,
    double_count: usize,
    stack_count: usize,
    pointer_count: usize,
    stack_return: Option<Term>,
    label_count: u16,
    scope_id: usize,
    loop_start: Vec<u16>,
    loop_exit: Vec<u16>,
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
            cur_symbols: Vec::new(),
            cur_block: None,
            var_map: HashMap::new(),
            temp_count: 0,
            double_count: 0,
            stack_count: 0,
            pointer_count: 0,
            stack_return: None,
            label_count: 0,
            scope_id: 0,
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
            panic!("No cur_block!");
        }
    }

    fn load_symbols(&mut self, id: usize) {
        if self.cur_symbols.get(id).unwrap().is_empty() {
            return;
        }
        for s in self.cur_symbols.get(id).unwrap().iter() {
            self.ir.insert(s.0.clone(), s.1.clone());
        }
    }

    fn unload_symbols(&mut self, id: usize) {
        if self.cur_symbols.get(id).unwrap().is_empty() {
            return;
        }
        for s in self.cur_symbols.get(id).unwrap().iter() {
            self.ir.remove(&s.0);
        }
    }

    fn stmt(&mut self, stmt: &node::Stmt) {
        match stmt {
            node::Stmt::Expr(expr) => { self.expr(expr); }
            node::Stmt::Let(decl) => self.r#let(decl),
            node::Stmt::Decl(decl) => self.r#decl(decl),
            node::Stmt::Fn(decl) => self.r#fn(decl),
            node::Stmt::StructDecl(_) => (),
            node::Stmt::Ret(ret) => self.ret(ret),
            node::Stmt::If(r#if) => self.r#if(r#if),
            node::Stmt::Loop(r#loop) => self.r#loop(r#loop),
            node::Stmt::While(r#while) => self.r#while(r#while),
            node::Stmt::For(r#for) => self.r#for(r#for),
            node::Stmt::Break(pos_id) => self.push_op(Op::Jump { label: *self.loop_exit.last().unwrap() }, *pos_id),
            node::Stmt::Continue(pos_id) => self.push_op(Op::Jump { label: *self.loop_start.last().unwrap() }, *pos_id),
            node::Stmt::Syscall(_) => ()
        }
    }

    fn expr(&mut self, expr: &node::Expr) -> Term {
        match &expr.kind {
            node::ExprKind::IntLit(val) => {
                Term::IntLit(*val)
            }
            node::ExprKind::ArrLit(arr_lit) => {
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
                    self.push_op(Op::Decl { term: ptr.clone(), size: arr_lit.exprs.len() as u32 * size }, arr_lit.pos_id);
                }
                for expr in &arr_lit.exprs {
                    let r = self.expr(expr);
                    if !inner_salloc {
                        self.temp_count += 1;
                        self.push_op(Op::Store { res: None, ptr: ptr.clone(), offset: self.salloc_offset, op: "=".to_string(), term: r }, arr_lit.pos_id);
                        self.salloc_offset += size as i64;
                    }
                }
                if Some(ptr.clone()) == self.cur_salloc {
                    self.cur_salloc = None;
                    self.salloc_offset = 0;
                }
                ptr
            }
            node::ExprKind::StructLit(lit) => {
                let ptr;
                let Some(Symbol::Struct { ty }) = self.ir.get(&lit.name.str) else {
                    panic!("Missing struct symbol");
                };
                if let Some(p) = &self.cur_salloc {
                    ptr = p.clone();
                } else {
                    self.stack_count += 1;
                    ptr = Term::Stack(self.stack_count);
                    self.cur_salloc = Some(ptr.clone());
                    self.push_op(Op::Decl { term: ptr.clone(), size: ty.size() }, lit.name.pos_id);
                }
                for expr in &lit.field_exprs {
                    let r = self.expr(expr);
                    if !expr.ty.salloc() {
                        self.temp_count += 1;
                        self.push_op(Op::Store { res: None, ptr: ptr.clone(), offset: self.salloc_offset, op: "=".to_string(), term: r }, lit.name.pos_id);
                        self.salloc_offset += expr.ty.size() as i64;
                    }
                }
                if Some(ptr.clone()) == self.cur_salloc {
                    self.cur_salloc = None;
                    self.salloc_offset = 0;
                }
                ptr
            }
            node::ExprKind::StringLit(lit) => {
                let ptr;
                if let Some(p) = &self.cur_salloc {
                    ptr = p.clone();
                } else {
                    self.stack_count += 1;
                    ptr = Term::Stack(self.stack_count);
                    self.cur_salloc = Some(ptr.clone());
                    self.push_op(Op::Decl { term: ptr.clone(), size: types::Type::String.size()}, expr.span.end);
                }
                self.temp_count += 1;
                self.push_op(Op::Store { res: None, ptr: ptr.clone(), offset: self.salloc_offset, op: "=".to_string(), term: Term::IntLit(lit.len() as i64) }, expr.span.end);
                self.salloc_offset += 8;
                if let Some(sym) = self.string_map.get(lit) {
                    self.push_op(
                        Op::Store { res: None, ptr: ptr.clone(), offset: self.salloc_offset, op: "=".to_string(), term: Term::Data(sym.clone()) }, expr.span.end
                    );
                } else {
                    self.string_id += 1;
                    let new_sym = format!("s.{}", self.string_id);
                    self.string_map.insert(lit.clone(), new_sym.clone());
                    self.ir.insert(new_sym.clone(), Symbol::StringLit { str: lit.to_string() });
                    self.push_op(
                        Op::Store { res: None, ptr: ptr.clone(), offset: self.salloc_offset, op: "=".to_string(), term: Term::Data(new_sym) }, expr.span.end
                    );
                }
                if Some(ptr.clone()) == self.cur_salloc {
                    self.cur_salloc = None;
                    self.salloc_offset = 0;
                }
                ptr
            }
            node::ExprKind::TupleLit(exprs) => {
                let ptr;
                if let Some(p) = &self.cur_salloc {
                    ptr = p.clone();
                } else {
                    self.stack_count += 1;
                    ptr = Term::Stack(self.stack_count);
                    self.cur_salloc = Some(ptr.clone());
                    self.push_op(Op::Decl { term: ptr.clone(), size: expr.ty.size() }, expr.span.end);
                }
                for expr in exprs {
                    let r = self.expr(expr);
                    if !expr.ty.salloc() {
                        self.temp_count += 1;
                        self.push_op(Op::Store { res: None, ptr: ptr.clone(), offset: self.salloc_offset, op: "=".to_string(), term: r }, expr.span.end);
                        self.salloc_offset += expr.ty.size() as i64;
                    }
                }
                if Some(ptr.clone()) == self.cur_salloc {
                    self.cur_salloc = None;
                    self.salloc_offset = 0;
                }
                ptr
            }
            node::ExprKind::Variable(pos_str) => {
                self.var_map.get(&pos_str.str).unwrap().clone()
            }
            node::ExprKind::Call(call) => self.call(call),
            node::ExprKind::Macro(r#macro) => panic!("no macros for now"),
            node::ExprKind::BinExpr(bin_expr) => self.bin_expr(bin_expr),
            node::ExprKind::UnExpr(un_expr) => self.un_expr(un_expr),
            node::ExprKind::TypeCast(cast) => self.type_cast(cast, &expr.ty)
        }
    }

    fn r#let(&mut self, decl: &node::Let) {
        let mut r = self.expr(&decl.expr);
        if let Some(ret) = &self.stack_return {
            r = ret.clone();
        }
        if r.is_stack() {
            self.var_map.insert(decl.name.str.clone(), r);
        } else if let Term::Double(_) = r {
            self.stack_count += 1;
            let id = self.stack_count;
            self.var_map.insert(decl.name.str.clone(), Term::Stack(id));
            self.push_op(Op::Decl { term: Term::Stack(id), size: 16 }, decl.name.pos_id);
            self.push_op(Op::Store { res: None, ptr: Term::Stack(id), offset: 0, op: "=".to_string(), term: r }, decl.name.pos_id);
        } else {
            self.stack_count += 1;
            let id = self.stack_count;
            self.var_map.insert(decl.name.str.clone(), Term::Stack(id));
            self.push_op(
                Op::Let {
                    term: r,
                    res: Term::Stack(id),
                },
                decl.name.pos_id,
            );
        }
    }
    fn r#decl(&mut self, decl: &node::Decl) {
        self.stack_count += 1;
        let id = self.stack_count;
        self.var_map.insert(decl.name.str.clone(), Term::Stack(id));
        self.push_op(Op::Decl { term: Term::Stack(id), size: decl.ty.size() }, decl.name.pos_id);
    }

    fn r#fn(&mut self, decl: &node::Fn) {
        self.cur_block = Some(Block::new());
        // self.temp_count = 0;
        // self.stack_count = 0;
        self.var_map.clear();
        if let Some(sym) = self.ir.get(&decl.name.str).cloned() {
            if let Symbol::Func { ty, block: _, symbols } = sym  {
                if ty.size() > 16 {
                    self.pointer_count += 1;
                    self.ret_salloc = Some(Term::Pointer(self.pointer_count));
                    self.push_op(Op::Arg { term: Term::Pointer(self.pointer_count), double: false }, decl.name.pos_id);
                }
                symbols.clone_into(&mut self.cur_symbols);
            }
        }
        self.scope_id = 0;
        self.load_symbols(0);
        for s in decl.arg_names.iter() {
            let Some(Symbol::Var { ty }) = self.ir.get(&s.str) else {
                println!("{:?}", self.ir.keys());
                panic!("Argument not a var ({}).", s.str);
            };
            if ty.size() > 16 {
                self.pointer_count += 1;
                let id = self.pointer_count;
                self.var_map.insert(s.str.clone(), Term::Pointer(id));
                self.push_op(Op::Arg { term: Term::Pointer(id), double: false }, s.pos_id);
            } else {
                self.stack_count += 1;
                let id = self.stack_count;
                self.var_map.insert(s.str.clone(), Term::Stack(id));
                self.push_op(Op::Arg { term: Term::Stack(id), double: ty.size() > 8 }, s.pos_id);
            }
        }
        self.scope_id += 1;
        self.scope(&decl.scope);
        self.unload_symbols(0);
        self.ret_salloc = None;
        println!("VAR MAP {}", decl.name.str);
        println!("{:?}", self.var_map);
        if let Some(Symbol::Func { ty: _, block, symbols: _, }) = self.ir.get_mut(&decl.name.str) {
            *block = self.cur_block.clone().unwrap();
        } else {
            panic!("Counldn't find {} symbol", decl.name.str);
        }
    }

    fn scope(&mut self, scope: &Vec<node::Stmt>) {
        let sc = self.scope_id;
        self.load_symbols(sc);
        self.scope_id += 1;
        for s in scope.iter() {
            self.stmt(s);
        }
        self.unload_symbols(sc);
    }

    fn ret(&mut self, ret: &node::Ret) {
        if let Some(expr) = &ret.expr {
            if let Some(r) = self.ret_salloc.clone() {
                self.cur_salloc = Some(r.clone());
                let mut from = self.expr(expr);
                if let Some(ret) = &self.stack_return {
                    from = ret.clone();
                }
                if from.is_stack() {
                    if from != r {
                        self.push_op(Op::Copy { from, to: r.clone(), size: ret.expr.as_ref().unwrap().ty.size() }, ret.pos_id);
                    }
                }
                self.push_op(Op::Return { term: Some(r) }, ret.pos_id);
                self.cur_salloc = None;
                self.salloc_offset = 0;
            } else {
                let r = self.expr(expr);
                if expr.ty.size() > 8 {
                    self.double_count += 1;
                    self.push_op(Op::Read { res: Term::Double(self.double_count), ptr: r, offset: 0 }, ret.pos_id);
                    self.push_op(Op::Return { term: Some(Term::Double(self.double_count))  }, ret.pos_id);
                } else {
                    self.push_op(Op::Return { term: Some(r)  }, ret.pos_id);
                }
            }
        } else {
            self.push_op(Op::Return { term: None }, ret.pos_id);
        }
    }

    fn r#if(&mut self, r#if: &node::If) {
        let mut if_chain = vec![r#if];
        let mut els = &r#if.els;
        while let Some(i) = els {
            if_chain.push(i);
            els = &i.els;
        }
        self.label_count += 1;
        let start_label = self.label_count;
        let mut has_else = false;
        for i in if_chain.iter() {
            if let Some(e) = &i.expr {
                let r = self.expr(e);
                self.push_op(
                    Op::CondJump {
                        cond: r,
                        label: self.label_count,
                    },
                    i.pos_id,
                );
            } else {
                self.push_op(Op::Jump { label: self.label_count }, i.pos_id);
                has_else = true;
            }
            self.label_count += 1;
        }
        let end_label = self.label_count;
        if !has_else {
            self.push_op(Op::Jump { label: end_label }, r#if.pos_id);
        }
        let mut cur_label = start_label;
        for i in if_chain.iter() {
            self.push_op(Op::Label { label: cur_label }, i.pos_id);
            self.scope(&i.scope);
            cur_label += 1;
            if cur_label != end_label - 1 {
                self.push_op(Op::Jump { label: end_label }, i.pos_id);
            }
        }
        self.push_op(Op::Label { label: end_label }, r#if.pos_id);
    }

    fn bin_expr(&mut self, bin: &node::BinExpr) -> Term {
        if let node::ExprKind::UnExpr(u) = &bin.lhs.kind {
            if u.op.str == "*" && bin.is_assign() {
                let term = self.expr(&bin.rhs);
                let ptr = self.expr(&u.expr);
                self.temp_count += 1;
                self.push_op(
                    Op::Store {
                        res: Some(Term::Temp(self.temp_count)),
                        ptr,
                        offset: 0,
                        op: bin.op.str.clone(),
                        term
                    },
                    u.op.pos_id,
                );
                return Term::Temp(self.temp_count);
            }
        }
        let lhs = self.expr(&bin.lhs);
        let rhs = self.expr(&bin.rhs);
        self.temp_count += 1;
        if bin.is_assign() {
            let is_stack = if let Term::Stack(_) = lhs { true } else { false };
            if bin.lhs.ty.salloc() && is_stack {
                if lhs != rhs {
                    self.push_op(
                        Op::Copy { 
                            from: rhs,
                            to: lhs,
                            size: bin.lhs.ty.size()
                        },
                        bin.op.pos_id
                    );
                }
            } else {
                self.push_op(
                    Op::Store { 
                        res: Some(Term::Temp(self.temp_count)),
                        ptr: lhs,
                        offset: 0,
                        op: bin.op.str.clone(),
                        term: Term::Temp(self.temp_count - 1)
                    },
                    bin.op.pos_id
                );
            }
        } else {
            let is_stack = if let Term::Stack(_) = lhs { true } else { false };
            if bin.lhs.ty.salloc() && is_stack {
                self.temp_count += 1;
                self.push_op(
                    Op::UnOp {
                        res: Term::Temp(self.temp_count - 1),
                        op: "&".to_string(),
                        term: lhs,
                    },
                    bin.op.pos_id
                );
                self.push_op(
                    Op::BinOp {
                        res: Some(Term::Temp(self.temp_count)),
                        lhs: Term::Temp(self.temp_count - 1),
                        op: bin.op.str.clone(),
                        rhs
                    },
                    bin.op.pos_id,
                );
            } else {
                self.push_op(
                    Op::BinOp {
                        res: Some(Term::Temp(self.temp_count)),
                        lhs,
                        op: bin.op.str.clone(),
                        rhs
                    },
                    bin.op.pos_id,
                );
            }
        }
        Term::Temp(self.temp_count)
    }

    fn un_expr(&mut self, un: &node::UnExpr) -> Term {
        let term = self.expr(&un.expr);
        self.temp_count += 1;
        self.push_op(
            Op::UnOp {
                res: Term::Temp(self.temp_count),
                op: un.op.str.clone(),
                term,
            },
            un.op.pos_id,
        );
        Term::Temp(self.temp_count)
    }

    fn call(&mut self, call: &node::Call) -> Term {
        let mut res = None;
        let mut is_double = false;
        let mut params = Vec::new();
        match self.ir.get(&call.name.str) {
            Some(Symbol::Func { ty, .. }) | Some(Symbol::ExternFunc { ty, .. }) | Some(Symbol::Syscall { ty, .. }) => {
                if ty.size() > 16 { // Pass pointer as first argument
                    self.stack_count += 1;
                    let r = Term::Stack(self.stack_count);
                    self.push_op(Op::Decl { term: r.clone(), size: ty.size() }, call.name.pos_id);
                    res = Some(r);
                } else if ty.size() > 8 {
                    is_double = true;
                }
            }
            _ => ()
        }
        self.push_op(Op::BeginCall, call.name.pos_id);
        if let Some(r) = &res {
            self.temp_count += 1;
            self.push_op(Op::UnOp { res: Term::Temp(self.temp_count), op: "&".to_string(), term: r.clone() }, call.name.pos_id);
            params.push(Term::Temp(self.temp_count));
        }
        for arg in call.args.iter() {
            let r = self.expr(arg);
            if let Term::Stack(_) = r {
                if self.var_map.values().any(|v| *v == r) {
                    if arg.ty.size() > 16 {
                        self.stack_count += 1;
                        self.temp_count += 1;
                        self.push_op(Op::Decl { term: Term::Stack(self.stack_count), size: arg.ty.size() }, call.name.pos_id);
                        self.push_op(Op::Copy { from: r.clone(), to: Term::Stack(self.stack_count), size: arg.ty.size() }, call.name.pos_id);
                        self.push_op(Op::UnOp { res: Term::Temp(self.temp_count), op: "&".to_string(), term: Term::Stack(self.stack_count) }, call.name.pos_id);
                    } else {
                        self.temp_count += 1;
                        self.push_op(Op::Read { res: Term::Temp(self.temp_count), ptr: r.clone(), offset: 0 }, call.name.pos_id);
                        if arg.ty.size() > 8 {
                            self.temp_count += 1;
                            self.push_op(Op::Read { res: Term::Temp(self.temp_count), ptr: r, offset: 8 }, call.name.pos_id);
                            params.push(Term::Temp(self.temp_count - 1));
                        }
                    }
                } else {
                    if arg.ty.size() > 16 {
                        self.temp_count += 1;
                        self.push_op(Op::UnOp {res: Term::Temp(self.temp_count), op: "&".to_string(), term: r.clone() }, call.name.pos_id);
                    } else {
                        self.temp_count += 1;
                        self.push_op(Op::Read { res: Term::Temp(self.temp_count), ptr: r.clone(), offset: 0 }, call.name.pos_id);
                        if arg.ty.size() > 8 {
                            self.temp_count += 1;
                            self.push_op(Op::Read { res: Term::Temp(self.temp_count), ptr: r, offset: 8 }, call.name.pos_id);
                            params.push(Term::Temp(self.temp_count - 1));
                        }
                    }
                }
                params.push(Term::Temp(self.temp_count));
            } else {
                params.push(r);
            }
        }
        for p in params {
            self.push_op(Op::Param { term: p }, call.name.pos_id);
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
                res: Some(res.clone())
            },
            call.name.pos_id,
        );
        self.push_op(Op::EndCall, call.name.pos_id);
        res
    }

    // fn r#macro(&mut self, r#macro: &node::Macro) {
    //     self.temp_count += 1;
    //     match r#macro {
    //         node::Macro::Salloc { count, ty } => {
    //             self.push_op(Op::Salloc { size: *count, res: Term::Temp(self.temp_count) }, 0);
    //         }
    //     }
    // }

    fn r#loop(&mut self, r#loop: &node::Loop) {
        self.label_count += 2;
        let s = self.label_count - 1;
        self.loop_start.push(s);
        let e = self.label_count;
        self.loop_exit.push(e);
        self.push_op(Op::Label {label: s }, r#loop.pos_id);
        self.push_op(Op::Save, 0);
        self.scope(&r#loop.scope);
        self.push_op(Op::Restore, 0);
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
        self.loop_exit.push(e);
        self.push_op(Op::Jump { label: c }, r#while.pos_id);
        self.push_op(Op::Label { label: s }, r#while.pos_id);
        self.push_op(Op::Save, 0);
        self.scope(&r#while.scope);
        self.push_op(Op::Restore, 0);
        self.push_op(Op::Label { label: c }, r#while.pos_id);
        let cond = self.expr(&r#while.expr);
        self.push_op(
            Op::CondJump {
                cond,
                label: s,
            },
            r#while.pos_id,
        );
        self.push_op(Op::Label { label: e }, r#while.pos_id);
        self.push_op(Op::NaturalFlow, r#while.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }
    fn r#for(&mut self, r#for: &node::For) {
        match &r#for.init {
            node::LetOrExpr::Let(r#let) => self.r#let(r#let),
            node::LetOrExpr::Expr(expr) => { self.expr(expr); }
        }
        self.label_count += 3;
        let s = self.label_count - 2;
        let c = self.label_count - 1;
        let e = self.label_count;
        self.loop_start.push(c);
        self.loop_exit.push(e);
        self.push_op(Op::Label { label: s }, r#for.pos_id);
        self.push_op(Op::Save, 0);
        self.scope(&r#for.scope);
        self.push_op(Op::Restore, 0);
        self.push_op(Op::Label { label: c }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.expr(&r#for.incr);
        let cond = self.expr(&r#for.cond);
        self.push_op(
            Op::CondJump {
                cond,
                label: s,
            },
            r#for.pos_id,
        );
        self.push_op(Op::Label { label: e }, r#for.pos_id);
        self.push_op(Op::NaturalFlow, r#for.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }

    fn type_cast(&mut self, cast: &node::TypeCast, to: &Type) -> Term {
        let r = self.expr(&cast.expr);
        let id = cast.expr.span.end;
        let res;
        match (&cast.expr.ty, to) {
            (Type::Array { inner: _, length }, Type::TaggedArray { inner: _ }) => {
                self.stack_count += 1;
                let s = Term::Stack(self.stack_count);
                self.temp_count += 1;
                self.push_op(Op::Decl { term: s.clone(), size: 16 }, id);
                self.push_op(Op::Store { res: None, ptr: s.clone(), offset: 0, op: "=".to_string(), term: Term::IntLit(*length as i64) }, id);
                self.push_op(Op::UnOp { res: Term::Temp(self.temp_count), op: "&".to_string(), term: r }, id);
                self.push_op(Op::Store { res: None, ptr: s.clone(), offset: 8, op: "=".to_string(), term: Term::Temp(self.temp_count) }, id);
                res = s;
            }
            _ => res = r
        }
        res
    }
}
