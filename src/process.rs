use std::vec::IntoIter;

use crate::node;
use crate::types::Type;

/// Modify the ast to express syntax sugar with simpler statements
pub fn process(stmts: Vec<node::Stmt>, expr_types: Vec<Type>) -> Vec<node::Stmt> {
    let mut res = Vec::new();
    let mut l = Process::new( expr_types.into_iter());
    for s in stmts.into_iter() {
        let new_stmt = l.stmt(s);
        res.push(new_stmt);
    }
    return res;
}

struct Process {
    pub stmts: Vec<Vec<node::Stmt>>,
    expr_types: IntoIter<Type>,
    index: usize,
    cur_type: Type,
}
impl Process {
    pub fn new( expr_types: IntoIter<Type>) -> Self {
        Self {
            stmts: Vec::new(),
            expr_types,
            index: 0,
            cur_type: Type::Void
        }
    }

    fn push(&mut self, stmt: node::Stmt) {
        self.stmts.last_mut().unwrap().push(stmt);
    }

    fn stmt(&mut self, stmt: node::Stmt) -> node::Stmt {
        match stmt {
            node::Stmt::Expr(expr) => node::Stmt::Expr(self.expr(expr)),
            node::Stmt::Let(decl) => node::Stmt::Let(node::Let {
                name: decl.name,
                expr: self.expr(decl.expr)
            }),
            node::Stmt::Fn(decl) => node::Stmt::Fn(node::Fn {
                name: decl.name,
                arg_names: decl.arg_names,
                arg_types: decl.arg_types,
                decl_type: decl.decl_type,
                scope: self.scope(decl.scope),
            }),
            node::Stmt::Ret(ret) => node::Stmt::Ret(node::Ret { pos_id: ret.pos_id, expr: self.opt_expr(ret.expr) }),
            node::Stmt::If(r#if) => node::Stmt::If(self.r#if(r#if)),
            node::Stmt::Loop(r#loop) => node::Stmt::Loop(node::Loop { pos_id: r#loop.pos_id, scope: self.scope(r#loop.scope) }),
            node::Stmt::While(r#while) => node::Stmt::While(node::While { 
                pos_id: r#while.pos_id,
                expr: self.expr(r#while.expr),
                scope: self.scope(r#while.scope)
            }),
            node::Stmt::For(r#for) => node::Stmt::For(node::For {
                pos_id: r#for.pos_id,
                init: self.let_or_expr(r#for.init),
                cond: self.expr(r#for.cond),
                incr: self.expr(r#for.incr),
                scope: self.scope(r#for.scope)
            }),
            _ => stmt
        }
    }

    fn opt_expr(&mut self, expr: Option<node::Expr>) -> Option<node::Expr> {
        return expr.map(|e| self.expr(e));
    }

    fn let_or_expr(&mut self, e: node::LetOrExpr) -> node::LetOrExpr {
        match e {
            node::LetOrExpr::Let(r#let) => node::LetOrExpr::Let(r#let),
            node::LetOrExpr::Expr(expr) => node::LetOrExpr::Expr(self.expr(expr))
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
        let new_expr = match expr {
            node::Expr::ArrLit(arr_lit) => node::Expr::ArrLit(node::ArrLit {
                exprs: self.expr_list(arr_lit.exprs),
                pos_id: arr_lit.pos_id
            }),
            node::Expr::BinExpr(bin) => self.bin_expr(bin),
            node::Expr::Call(call) => node::Expr::Call(node::Call {
                name: call.name,
                args: self.expr_list(call.args)
            }),
            node::Expr::UnExpr(un) => node::Expr::UnExpr(node::UnExpr {
                expr: Box::new(self.expr(*un.expr)),
                op: un.op
            }),
            _ => expr
        };
        self.index += 1;
        self.cur_type = self.expr_types.next().unwrap();
        println!("{}. {:?}", self.index, self.cur_type);
        return new_expr;
    }

    fn scope(&mut self, scope: Vec<node::Stmt>) -> Vec<node::Stmt> {
        self.stmts.push(Vec::new());
        for s in scope.into_iter() {
            let new_stmt = self.stmt(s);
            self.push(new_stmt);
        }
        return self.stmts.pop().unwrap();
    }

    fn r#if(&mut self, r#if: node::If) -> node::If {
        return node::If {
            pos_id: r#if.pos_id,
            expr: self.opt_expr(r#if.expr),
            scope: self.scope(r#if.scope),
            els: r#if.els.map(|e| Box::new(self.r#if(*e)))
        };
    }

    fn pos_str(&self, str: String) -> node::PosStr {
        node::PosStr {
            str, pos_id: 0
        }
    }

    fn calc_bin_expr(&self, bin: node::BinExpr) -> node::Expr {
        if let node::Expr::IntLit(lstr) = &*bin.lhs {
            if let node::Expr::IntLit(rstr) = &*bin.rhs {
                let l = lstr.str.parse::<i64>().unwrap();
                let r = rstr.str.parse::<i64>().unwrap();
                return node::Expr::IntLit(self.pos_str(
                    match bin.op.str.as_str() {
                        "+" => l + r,
                        "-" => l - r,
                        "*" => l * r,
                        "/" => l / r,
                        _ => panic!("Unexpected operation")
                    }.to_string()
                ));
            }
        }
        node::Expr::BinExpr(bin)
    }
    fn bin_expr(&mut self, bin: node::BinExpr) -> node::Expr {
        let lhs = self.expr(*bin.lhs);
        let ltype = self.cur_type.clone();
        let mut rhs = self.expr(*bin.rhs);
        if let Some(p) = ltype.pointer() { // Pointer arithmetic - multiply by size of inner type
            rhs = self.calc_bin_expr(node::BinExpr {
                lhs: Box::new(rhs),
                rhs: Box::new(node::Expr::IntLit(self.pos_str(p.size().to_string()))),
                op: self.pos_str("*".to_string())
            });
        }
        if bin.op.str == "[]" { // Array access -> add and dereference
            return node::Expr::UnExpr(node::UnExpr {
                op: self.pos_str("*".to_string()),
                expr: Box::new(self.calc_bin_expr(node::BinExpr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: self.pos_str("+".to_string())
                }))
            });
        }
        self.calc_bin_expr(node::BinExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: bin.op
        })
    }
}
