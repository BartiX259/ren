use std::collections::HashMap;

use crate::ir::{self, Block, Symbol};
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
    cur_symbols: Vec<Vec<(String, Symbol)>>,
    cur_block: Option<Block>,
    temp_count: usize,
    label_count: u16,
    scope_id: usize,
    loop_start: Vec<u16>,
    loop_exit: Vec<u16>,
}
impl<'a> Lower<'a> {
    pub fn new(ir: &'a mut HashMap<String, Symbol>) -> Self {
        Self {
            ir,
            cur_symbols: Vec::new(),
            cur_block: None,
            temp_count: 0,
            label_count: 0,
            scope_id: 0,
            loop_start: Vec::new(),
            loop_exit: Vec::new(),
        }
    }
    fn push_op(&mut self, op: ir::Op, pos_id: usize) {
        if let Some(ref mut block) = self.cur_block {
            block.ops.push(op);
            block.locs.push(ir::OpLoc { start_id: pos_id, end_id: pos_id });
        } else {
            panic!("No cur_block!");
        }
    }

    fn load_symbols(&mut self, id: usize) {
        if self.cur_symbols.get(id).unwrap().is_empty() {
            return;
        }
        self.push_op(ir::Op::LoadSymbols(id), 0);
    }

    fn unload_symbols(&mut self, id: usize) {
        if self.cur_symbols.get(id).unwrap().is_empty() {
            return;
        }
        self.push_op(ir::Op::UnloadSymbols(id), 0);
    }

    fn stmt(&mut self, stmt: &node::Stmt) {
        match stmt {
            node::Stmt::Expr(expr) => self.expr(expr),
            node::Stmt::Let(decl) => self.r#let(decl),
            node::Stmt::Decl(decl) => self.r#decl(decl),
            node::Stmt::Fn(decl) => self.r#fn(decl),
            node::Stmt::Ret(ret) => self.ret(ret),
            node::Stmt::If(r#if) => self.r#if(r#if),
            node::Stmt::Loop(r#loop) => self.r#loop(r#loop),
            node::Stmt::While(r#while) => self.r#while(r#while),
            node::Stmt::For(r#for) => self.r#for(r#for),
            node::Stmt::Break(pos_id) => self.push_op(ir::Op::Jump(*self.loop_exit.last().unwrap()), *pos_id),
            node::Stmt::Continue(pos_id) => self.push_op(ir::Op::Jump(*self.loop_start.last().unwrap()), *pos_id),
        }
    }

    fn expr(&mut self, expr: &node::Expr) {
        match expr {
            node::Expr::IntLit(pos_str) => {
                self.temp_count += 1;
                self.push_op(
                    ir::Op::Tac {
                        lhs: ir::Term::IntLit(pos_str.str.clone()),
                        rhs: None,
                        op: None,
                        res: Some(ir::Term::Temp(self.temp_count)),
                    },
                    pos_str.pos_id,
                );
            }
            node::Expr::ArrLit(arr_lit) => {
                self.temp_count += 1;
                let ptr = ir::Term::Temp(self.temp_count);
                let mut i = 0;
                self.push_op(ir::Op::Salloc { size: arr_lit.exprs.len() as u32 * 8, res: ptr.clone() }, 0);
                for expr in &arr_lit.exprs {
                    self.expr(expr);
                    self.temp_count += 1;
                    if i != 0 {
                        self.push_op(ir::Op::Tac { lhs: ptr.clone(), rhs: Some(ir::Term::IntLit(8.to_string())), op: Some("+".to_string()), res: Some(ir::Term::Temp(self.temp_count)) }, arr_lit.pos_id);
                    } else {
                        self.push_op(ir::Op::Tac { lhs: ptr.clone(), rhs: None, op: None, res: Some(ir::Term::Temp(self.temp_count)) }, arr_lit.pos_id);
                    }
                    i += 8;
                    self.temp_count += 1;
                    self.push_op(ir::Op::DerefAssign { term: ir::Term::Temp(self.temp_count - 2), op: "=".to_string(), ptr: ir::Term::Temp(self.temp_count - 1), res: Some(ir::Term::Temp(self.temp_count)) }, arr_lit.pos_id);
                }
                self.temp_count += 1;
                self.push_op(ir::Op::Tac { lhs: ptr, rhs: Some(ir::Term::IntLit((i-8).to_string())), op: Some("-".to_string()), res: Some(ir::Term::Temp(self.temp_count)) }, arr_lit.pos_id);
            }
            node::Expr::Variable(pos_str) => {
                self.temp_count += 1;
                self.push_op(
                    ir::Op::Tac {
                        lhs: ir::Term::Symbol(pos_str.str.clone()),
                        rhs: None,
                        op: None,
                        res: Some(ir::Term::Temp(self.temp_count)),
                    },
                    pos_str.pos_id,
                );
            }
            node::Expr::Call(call) => self.call(call),
            node::Expr::Macro(r#macro) => self.r#macro(r#macro),
            node::Expr::BinExpr(bin_expr) => self.bin_expr(bin_expr),
            node::Expr::UnExpr(un_expr) => self.un_expr(un_expr),
        }
    }

    fn r#let(&mut self, decl: &node::Let) {
        self.expr(&decl.expr);
        self.push_op(
            ir::Op::Tac {
                lhs: ir::Term::Temp(self.temp_count),
                rhs: None,
                op: None,
                res: Some(ir::Term::Symbol(decl.name.str.clone())),
            },
            decl.name.pos_id,
        );
    }
    fn r#decl(&mut self, decl: &node::Decl) {
        self.push_op(ir::Op::Decl(ir::Term::Symbol(decl.name.str.clone())), decl.name.pos_id);
    }

    fn r#fn(&mut self, decl: &node::Fn) {
        self.cur_block = Some(Block::new());
        self.ir.get(&decl.name.str).and_then(|sym| match sym {
            Symbol::Func { ty: _, block: _, symbols } => {
                symbols.clone_into(&mut self.cur_symbols);
                Some(())
            }
            _ => None,
        });
        self.scope_id = 0;
        self.load_symbols(0);
        for i in 0..decl.arg_names.len() {
            let s = &decl.arg_names.get(i).unwrap().str;
            self.push_op(ir::Op::Arg(ir::Term::Symbol(s.clone())), decl.name.pos_id);
        }
        self.scope_id += 1;
        self.scope(&decl.scope);
        self.unload_symbols(0);

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
            self.expr(expr);
            self.push_op(ir::Op::Return(ir::Term::Temp(self.temp_count)), ret.pos_id);
        } else {
            self.push_op(ir::Op::ReturnNone, ret.pos_id);
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
                self.expr(e);
                self.push_op(
                    ir::Op::CondJump {
                        cond: ir::Term::Temp(self.temp_count),
                        label: self.label_count,
                    },
                    i.pos_id,
                );
            } else {
                self.push_op(ir::Op::Jump(self.label_count), i.pos_id);
                has_else = true;
            }
            self.label_count += 1;
        }
        let end_label = self.label_count;
        if !has_else {
            self.push_op(ir::Op::Jump(end_label), r#if.pos_id);
        }
        let mut cur_label = start_label;
        for i in if_chain.iter() {
            self.push_op(ir::Op::Label(cur_label), i.pos_id);
            self.scope(&i.scope);
            cur_label += 1;
            if cur_label != end_label - 1 {
                self.push_op(ir::Op::Jump(end_label), i.pos_id);
            }
        }
        self.push_op(ir::Op::Label(end_label), r#if.pos_id);
    }

    fn bin_expr(&mut self, bin: &node::BinExpr) {
        if let node::Expr::UnExpr(u) = &*bin.lhs {
            if u.op.str == "*" && ["=", "+=", "-=", "*=", "/="].contains(&bin.op.str.as_str()) {
                self.expr(&u.expr);
                let ptr = ir::Term::Temp(self.temp_count);
                self.expr(&bin.rhs);
                let term = ir::Term::Temp(self.temp_count);
                self.temp_count += 1;
                self.push_op(
                    ir::Op::DerefAssign {
                        term,
                        op: bin.op.str.clone(),
                        ptr,
                        res: Some(ir::Term::Temp(self.temp_count)),
                    },
                    u.op.pos_id,
                );
                return;
            }
        }
        self.expr(&bin.lhs);
        let lhs = ir::Term::Temp(self.temp_count);
        self.expr(&bin.rhs);
        self.temp_count += 1;
        self.push_op(
            ir::Op::Tac {
                lhs,
                rhs: Some(ir::Term::Temp(self.temp_count - 1)),
                op: Some(bin.op.str.clone()),
                res: Some(ir::Term::Temp(self.temp_count)),
            },
            bin.op.pos_id,
        );
    }

    fn un_expr(&mut self, un: &node::UnExpr) {
        self.expr(&un.expr);
        let term = ir::Term::Temp(self.temp_count);
        self.temp_count += 1;
        self.push_op(
            ir::Op::Unary {
                term,
                op: un.op.str.clone(),
                res: ir::Term::Temp(self.temp_count),
            },
            un.op.pos_id,
        );
    }

    fn call(&mut self, call: &node::Call) {
        let mut tmps = Vec::new();
        for arg in &call.args {
            self.expr(arg);
            tmps.push(self.temp_count);
        }
        for tmp in tmps.into_iter().rev() {
            self.push_op(ir::Op::Param(ir::Term::Temp(tmp)), call.name.pos_id);
        }
        self.temp_count += 1;
        self.push_op(
            ir::Op::Call {
                func: call.name.str.clone(),
                res: ir::Term::Temp(self.temp_count),
            },
            call.name.pos_id,
        );
    }

    fn r#macro(&mut self, r#macro: &node::Macro) {
        self.temp_count += 1;
        match r#macro {
            node::Macro::Salloc { count, ty } => {
                self.push_op(ir::Op::Salloc { size: *count, res: ir::Term::Temp(self.temp_count) }, 0);
            }
        }
    }

    fn r#loop(&mut self, r#loop: &node::Loop) {
        self.label_count += 2;
        let s = self.label_count - 1;
        self.loop_start.push(s);
        let e = self.label_count;
        self.loop_exit.push(e);
        self.push_op(ir::Op::Label(s), r#loop.pos_id);
        self.push_op(ir::Op::LoopStart, 0);
        self.scope(&r#loop.scope);
        self.push_op(ir::Op::LoopEnd, 0);
        self.push_op(ir::Op::Jump(s), r#loop.pos_id);
        self.push_op(ir::Op::Label(e), r#loop.pos_id);
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
        self.push_op(ir::Op::Jump(c), r#while.pos_id);
        self.push_op(ir::Op::Label(s), r#while.pos_id);
        self.push_op(ir::Op::LoopStart, 0);
        self.scope(&r#while.scope);
        self.push_op(ir::Op::LoopEnd, 0);
        self.push_op(ir::Op::Label(c), r#while.pos_id);
        self.expr(&r#while.expr);
        self.push_op(
            ir::Op::CondJump {
                cond: ir::Term::Temp(self.temp_count),
                label: s,
            },
            r#while.pos_id,
        );
        self.push_op(ir::Op::Label(e), r#while.pos_id);
        self.push_op(ir::Op::NaturalFlow, r#while.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }
    fn r#for(&mut self, r#for: &node::For) {
        match &r#for.init {
            node::LetOrExpr::Let(r#let) => self.r#let(r#let),
            node::LetOrExpr::Expr(expr) => self.expr(expr)
        }
        self.label_count += 3;
        let s = self.label_count - 2;
        let c = self.label_count - 1;
        let e = self.label_count;
        self.loop_start.push(c);
        self.loop_exit.push(e);
        self.push_op(ir::Op::Label(s), r#for.pos_id);
        self.push_op(ir::Op::LoopStart, 0);
        self.scope(&r#for.scope);
        self.push_op(ir::Op::LoopEnd, 0);
        self.push_op(ir::Op::Label(c), r#for.pos_id);
        self.push_op(ir::Op::NaturalFlow, r#for.pos_id);
        self.expr(&r#for.incr);
        self.expr(&r#for.cond);
        self.push_op(
            ir::Op::CondJump {
                cond: ir::Term::Temp(self.temp_count),
                label: s,
            },
            r#for.pos_id,
        );
        self.push_op(ir::Op::Label(e), r#for.pos_id);
        self.push_op(ir::Op::NaturalFlow, r#for.pos_id);
        self.loop_start.pop();
        self.loop_exit.pop();
    }
}
