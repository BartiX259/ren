use crate::ir::{Block, Symbol};
use crate::types::Type;
use crate::node::{self, PosStr, Span};
use std::collections::{HashMap, HashSet};


/// Validate the ast, return a symbol table if successful
pub fn validate(stmts: &mut Vec<node::Stmt>) -> Result<HashMap<String, Symbol>, SemanticError> {
    let mut val = Validate::new();
    for stmt in stmts.iter() {
        val.hoist(stmt)?;
    }
    for stmt in stmts {
        val.stmt(stmt)?;
    }
    Ok(val.symbol_table)
}

pub enum SemanticError {
    InvalidType(PosStr),
    SymbolExists(PosStr),
    UndeclaredSymbol(PosStr),
    TypeMismatch(PosStr, Type, Type),
    InvalidReturn(usize),
    NotInLoop(String, usize),
    InvalidUnaryOperator(PosStr),
    InvalidAdressOf(PosStr),
    InvalidDereference(PosStr, Type),
    FuncInFunc(PosStr),
    InvalidArgCount(PosStr, usize, usize),
    ArgTypeMismatch(PosStr, Type, Type),
    StructInFunc(PosStr),
    InvalidStructKey(PosStr, PosStr),
    MissingStructKey(PosStr, String),
    StructTypeMismatch(PosStr, Type, Type),
    EmptyArray(usize),
    ArrayTypeMismatch(Span, Type, Type),
    MissingLen(PosStr),
}

pub struct Validate {
    pub symbol_table: HashMap<String, Symbol>,
    fn_symbols: Vec<Vec<(String, Symbol)>>,
    symbol_stack: Vec<usize>,
    cur_func: Option<String>,
    cur_ret: Type,
    scope_id: usize,
    loop_count: usize,
}

impl Validate {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::from([
                ("print".to_string(), Symbol::ExternFunc { ty: Type::Void, args: vec![Type::Int] }),
            ]),
            fn_symbols: Vec::new(),
            symbol_stack: Vec::new(),
            cur_func: None,
            cur_ret: Type::Void,
            scope_id: 0,
            loop_count: 0,
        }
    }

    /// Returns mutable reference to the current scope's symbols
    fn scope_symbols(&mut self) -> &mut Vec<(String, Symbol)> {
        if self.cur_func.is_some() {
            let i = self.symbol_stack.last().unwrap().clone();
            self.fn_symbols.get_mut(i).unwrap()
        } else {
            panic!("Pushed symbol outside of function!");
        }
    }
    
    fn push_symbol(&mut self, name: String, sym: Symbol) {
        println!("push {}", name);
        self.symbol_table.insert(name.clone(), sym.clone());
        self.scope_symbols().push((name.clone(), sym.clone()));
    }

    fn hoist(&mut self, stmt: &node::Stmt) -> Result<(), SemanticError> {
        match stmt {
            node::Stmt::Fn(decl) => {
                if self.symbol_table.contains_key(&decl.name.str) {
                    return Err(SemanticError::SymbolExists(decl.name.clone()));
                }
                let ty;
                if let Some(t) = &decl.decl_type {
                    ty = self.r#type(t)?;
                } else {
                    ty = Type::Void;
                }
                let mut arg_symbols: Vec<(String, Symbol)> = Vec::new();
                for arg in decl.arg_names.iter().zip(decl.arg_types.iter()) {
                    for existing in arg_symbols.iter() {
                        if existing.0 == arg.0.str {
                            return Err(SemanticError::SymbolExists(arg.0.clone()));
                        }
                    }
                    let sym = Symbol::Var { ty: self.r#type(arg.1)? };
                    arg_symbols.push((arg.0.str.clone(), sym.clone()));
                }
                self.symbol_table.insert(decl.name.str.clone(), Symbol::Func { 
                    ty,
                    block: Block::new(),
                    symbols: vec![arg_symbols],
                });
                Ok(())
            }
            node::Stmt::StructDecl(decl) => {
                if self.symbol_table.contains_key(&decl.name.str) {
                    return Err(SemanticError::SymbolExists(decl.name.clone()));
                }
                let names: Vec<String> = decl.field_names.iter().map(|pos_str| pos_str.str.clone()).collect();
                let mut types = Vec::new();
                for ty in decl.field_types.iter() {
                    types.push(self.r#type(ty)?);
                }
                let ty = Type::Struct { names, types };
                self.symbol_table.insert(decl.name.str.clone(), Symbol::Struct { ty });
                Ok(())
            }
            _ => Ok(())
        }
    }
    
    fn stmt(&mut self, stmt: &mut node::Stmt) -> Result<(), SemanticError> {
        match stmt {
            node::Stmt::Expr(expr) => self.expr(expr).map(|_| ()),
            node::Stmt::Let(decl) => self.r#let(decl),
            node::Stmt::Decl(decl) => self.r#decl(decl),
            node::Stmt::Fn(decl) => self.r#fn(decl),
            node::Stmt::StructDecl(decl) => self.struct_decl(decl),
            node::Stmt::Ret(ret) => self.ret(ret),
            node::Stmt::If(r#if) => self.r#if(r#if),
            node::Stmt::Loop(r#loop) => self.r#loop(r#loop),
            node::Stmt::While(r#while) => self.r#while(r#while),
            node::Stmt::For(r#for) => self.r#for(r#for),
            node::Stmt::Break(pos_id) => self.check_loop("Break", *pos_id),
            node::Stmt::Continue(pos_id) => self.check_loop("Continue", *pos_id),
        }
    }

    fn expr(&mut self, expr: &mut node::Expr) -> Result<Type, SemanticError> {
        let res = match &mut expr.kind {
            node::ExprKind::IntLit(_) => Ok(Type::Int),
            node::ExprKind::ArrLit(arr_lit) => self.expr_list(&mut arr_lit.exprs, arr_lit.pos_id)
            .map(|res| Type::Array { inner: Box::new(res.0), length: res.1 }),
            node::ExprKind::StructLit(struct_lit) => self.struct_lit(struct_lit),
            node::ExprKind::Variable(pos_str) => self
                .symbol_table
                .get(&pos_str.str)
                .and_then(|symbol| match symbol {
                    Symbol::Var { ty } => Some(ty.clone()),
                    _ => None,
                })
                .ok_or(SemanticError::UndeclaredSymbol(pos_str.clone())),
            node::ExprKind::Call(call) => self.call(call),
            node::ExprKind::Macro(r#macro) => self.r#macro(r#macro),
            node::ExprKind::BinExpr(bin_expr) => self.bin_expr(bin_expr),
            node::ExprKind::UnExpr(un_expr) => self.un_expr(un_expr),
        };
        if let Ok(ty) = &res {
            expr.ty = ty.clone();
        }
        res
    }

    fn r#let(&mut self, decl: &mut node::Let) -> Result<(), SemanticError> {
        if self.symbol_table.contains_key(&decl.name.str) {
            return Err(SemanticError::SymbolExists(decl.name.clone()));
        }
        let ty = self.expr(&mut decl.expr)?;
        self.push_symbol(decl.name.str.clone(), Symbol::Var { ty });
        Ok(())
    }
    fn r#decl(&mut self, decl: &node::Decl) -> Result<(), SemanticError> {
        if self.symbol_table.contains_key(&decl.name.str) {
            return Err(SemanticError::SymbolExists(decl.name.clone()));
        }
        let ty = self.r#type(&decl.r#type)?;
        self.push_symbol(decl.name.str.clone(), Symbol::Var { ty });
        Ok(())
    }

    fn r#fn(&mut self, decl: &mut node::Fn) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::FuncInFunc(decl.name.clone()));
        }
        // Retrieve arguments
        let arg_symbols = self
            .symbol_table
            .get(&decl.name.str)
            .and_then(|sym| match sym {
                Symbol::Func { symbols, .. } => symbols.get(0).cloned(),
                _ => None,
            })
            .expect("Function symbol not found");

        for (name, symbol) in &arg_symbols {
            self.symbol_table.insert(name.clone(), symbol.clone());
        }

        self.scope_id = 0;
        self.symbol_stack.push(self.scope_id);
        self.fn_symbols.push(arg_symbols.clone());
        self.scope_id += 1;

        self.cur_ret = decl
            .decl_type
            .as_ref()
            .map(|t| self.r#type(t))
            .transpose()?
            .unwrap_or(Type::Void);
        
        self.cur_func = Some(decl.name.str.clone());

        self.scope(&mut decl.scope)?;

        for (name, _) in &arg_symbols {
            self.symbol_table.remove(name);
        }

        if let Some(Symbol::Func { symbols, .. }) = self.symbol_table.get_mut(&decl.name.str) {
            *symbols = self.fn_symbols.clone();
        } else {
            unreachable!("Function symbol should exist at this point");
        }

        self.cur_ret = Type::Void;
        self.fn_symbols.clear();
        self.cur_func = None;

        Ok(())
    }

    fn struct_decl(&mut self, decl: &node::StructDecl) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::StructInFunc(decl.name.clone()));
        }
        Ok(())
    }

    fn scope(&mut self, scope: &mut Vec<node::Stmt>) -> Result<(), SemanticError> {
        println!("scope {}, {:?}", self.scope_id, self.fn_symbols);
        self.symbol_stack.push(self.scope_id);
        self.fn_symbols.push(Vec::new());
        self.scope_id += 1;
        for stmt in scope.iter_mut() {
            self.stmt(stmt)?;
        }
        // Remove the local symbols from the symbol table
        println!("popping {:?}", self.symbol_stack);
        let i = self.symbol_stack.pop().unwrap();
        for sym in self.fn_symbols.get(i).unwrap() {
            println!("removing {}", sym.0);
            self.symbol_table.remove(&sym.0);
        }
        Ok(())
    }

    fn ret(&mut self, ret: &mut node::Ret) -> Result<(), SemanticError> {
        if self.cur_func.is_none() {
            return Err(SemanticError::InvalidReturn(ret.pos_id));
        }
        if let Some(expr) = &mut ret.expr {
            let ty = self.expr(expr)?;
            if self.cur_ret != ty {
                return Err(SemanticError::InvalidReturn(ret.pos_id));
            }
        } else if self.cur_ret != Type::Void {
            return Err(SemanticError::InvalidReturn(ret.pos_id));   
        }
        Ok(())
    }

    fn r#if(&mut self, r#if: &mut node::If) -> Result<(), SemanticError> {
        if let Some(e) = &mut r#if.expr {
            self.expr(e)?;
        }
        self.scope(&mut r#if.scope)?;
        if let Some(i) = &mut r#if.els {
            self.r#if(i)?;
        }
        Ok(())
    }

    fn bin_expr(&mut self, bin: &mut node::BinExpr) -> Result<Type, SemanticError> {
        let ty1 = self.expr(&mut bin.lhs)?;
        let ty2 = self.expr(&mut bin.rhs)?;

        match bin.op.str.as_str() {
            "+" | "-" | "*" | "/" => {
                // Arithmetic operators
                match (ty1.clone(), ty2.clone()) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Float) => Ok(Type::Float),
                    (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float),
                    (Type::Pointer(p), Type::Int) => Ok(Type::Pointer(p)),
                    _ => Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2)),
                }
            }
            "&&" | "||" => {
                // Logical operators
                if ty1 == Type::Bool && ty2 == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                // Comparison operators
                if ty1 == ty2 || (ty1 == Type::Int && ty2 == Type::Float) || (ty1 == Type::Float && ty2 == Type::Int) {
                    Ok(Type::Bool)
                } else {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            "=" | "+=" | "-=" | "*=" | "/=" => {
                // Assignment operators
                if ty1 == ty2 {
                    Ok(ty1)
                } else {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            "[]" => {
                if let Some(t) = ty1.pointer() {
                    if ty2 == Type::Int {
                        Ok(t)
                    } else {
                        Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                    }
                } else {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            _ => unreachable!(),
        }
    }

    fn un_expr(&mut self, un: &mut node::UnExpr) -> Result<Type, SemanticError> {
        let ty = self.expr(&mut un.expr)?;
        match un.op.str.as_str() {
            "-" => Ok(ty),
            "&" => {
                if let node::ExprKind::Variable(_) = un.expr.kind {
                    if let Some(p) = ty.stack() {
                        Ok(Type::Pointer(Box::new(p)))
                    } else {
                        Ok(Type::Pointer(Box::new(ty)))
                    }
                } else {
                    Err(SemanticError::InvalidAdressOf(un.op.clone()))
                }
            }
            "*" => {
                if let Type::Pointer(t) = ty {
                    Ok(*t)
                } else {
                    Err(SemanticError::InvalidDereference(un.op.clone(), ty))
                }
            }
            _ => Err(SemanticError::InvalidUnaryOperator(un.op.clone())),
        }
    }

    fn struct_lit(&mut self, lit: &mut node::StructLit) -> Result<Type, SemanticError> {
        let decl_type = self.symbol_table.get(&lit.name.str).cloned();
        let Some(Symbol::Struct { ty }) = decl_type else {
            return Err(SemanticError::UndeclaredSymbol(lit.name.clone()));
        };
        let Type::Struct { names, types } = ty.clone() else {
            unreachable!();
        };
        let map: HashMap<&String, &Type> = names.iter().zip(types.iter()).collect();
        for (name, expr) in lit.field_names.iter().zip(lit.field_exprs.iter_mut()) {
            let field_ty = self.expr(expr)?;
            let m = map.get(&name.str);
            if let Some(t) = m {
                if **t != field_ty {
                    return Err(SemanticError::StructTypeMismatch(name.clone(), (*t).clone(), field_ty))
                }
            } else {
                return Err(SemanticError::InvalidStructKey(lit.name.clone(), name.clone()));
            }
        }
        let set: HashSet<String> = lit.field_names.iter().map(|pos_str| pos_str.str.clone()).collect();
        for name in names {
            if set.get(&name).is_none() {
                return Err(SemanticError::MissingStructKey(lit.name.clone(), name));
            }
        }

        Ok(ty)
    }

    fn expr_list(&mut self, exprs: &mut Vec<node::Expr>, pos_id: usize) -> Result<(Type, usize), SemanticError> {
        let mut cur_type = Type::Void;
        for expr in exprs.iter_mut() {
            let ty = self.expr(expr)?;
            if cur_type == Type::Void {
                cur_type = ty;
            } else if cur_type != ty {
                return Err(SemanticError::ArrayTypeMismatch(expr.span, cur_type, ty));
            }
        }
        if cur_type == Type::Void {
            return Err(SemanticError::EmptyArray(pos_id));
        }
        return Ok((cur_type, exprs.len()));
    }

    fn call(&mut self, call: &mut node::Call) -> Result<Type, SemanticError> {
        let mut arg_types = Vec::new();
        for s in call.args.iter_mut() {
            arg_types.push(self.expr(s)?);
        }
        let ty;
        let expected_types;
        let temp: Vec<_>;
        match self.symbol_table.get(&call.name.str) {
            Some(Symbol::Func { ty: t, block: _, symbols}) => {
                ty = t;
                if symbols.len() == 0 { // Recursive call
                    temp = self.fn_symbols
                    .get(0)
                    .unwrap()
                    .iter()
                    .filter_map(|(_, symbol)| if let Symbol::Var { ty } = symbol { Some(ty) } else { None })
                    .collect();
                } else {
                    temp = symbols
                    .get(0)
                    .unwrap()
                    .iter()
                    .filter_map(|(_, symbol)| if let Symbol::Var { ty } = symbol { Some(ty) } else { None })
                    .collect();
                }
                expected_types = temp;
            }
            Some(Symbol::ExternFunc { ty: t, args }) => {
                ty = t;
                temp = args.iter().collect();
                expected_types = temp;
            }
            None => return Err(SemanticError::UndeclaredSymbol(call.name.clone())),
            _ => return Err(SemanticError::UndeclaredSymbol(call.name.clone())),
        }

        if expected_types.len() != arg_types.len() {
            return Err(SemanticError::InvalidArgCount(call.name.clone(), expected_types.len(), arg_types.len()));
        }
        for types in expected_types.into_iter().zip(arg_types.iter()) {
            if types.0 != types.1 {
                return Err(SemanticError::ArgTypeMismatch(call.name.clone(), types.0.clone(), types.1.clone()));
            }
        }
        Ok(ty.clone())
    }

    fn r#loop(&mut self, r#loop: &mut node::Loop) -> Result<(), SemanticError> {
        self.loop_count += 1;
        self.scope(&mut r#loop.scope)?;
        self.loop_count -= 1;
        Ok(())
    }
    fn r#while(&mut self, r#while: &mut node::While) -> Result<(), SemanticError> {
        self.expr(&mut r#while.expr)?;
        self.loop_count += 1;
        self.scope(&mut r#while.scope)?;
        self.loop_count -= 1;
        Ok(())
    }
    fn r#for(&mut self, r#for: &mut node::For) -> Result<(), SemanticError> {
        match &mut r#for.init {
            node::LetOrExpr::Let(r#let) => self.r#let(r#let)?,
            node::LetOrExpr::Expr(expr) => self.expr(expr).map(|_| ())?
        }
        self.expr(&mut r#for.cond)?;
        self.expr(&mut r#for.incr)?;
        self.loop_count += 1;
        self.scope(&mut r#for.scope)?;
        self.loop_count -= 1;
        Ok(())
    }

    /// Checks if validator is currently in a loop
    fn check_loop(&self, name: &str, pos_id: usize) -> Result<(), SemanticError> {
        if self.loop_count == 0 {
            return Err(SemanticError::NotInLoop(name.to_string(), pos_id));
        }
        Ok(())
    }

    fn r#type(&self, r#type: &node::Type) -> Result<Type, SemanticError> {
        match r#type.str.str.as_str() {
            "int" => self.no_subtype(r#type, Type::Int),
            "float" => self.no_subtype(r#type, Type::Float),
            "bool" => self.no_subtype(r#type, Type::Bool),
            "ref" => Ok(Type::Pointer(self.get_subtype(r#type)?)),
            "arr" => Ok(Type::Array {inner: self.get_subtype(r#type)?, length: self.get_len(r#type)? }),
            _ => Err(SemanticError::InvalidType(r#type.str.clone())),
        }
    }
    fn no_subtype(&self, node_type: &node::Type, res_type: Type) -> Result<Type, SemanticError> {
        if node_type.sub.is_some() {
            return Err(SemanticError::InvalidType(node_type.str.clone()));
        }
        Ok(res_type)
    }
    fn get_subtype(&self, node_type: &node::Type) -> Result<Box<Type>, SemanticError> {
        if let Some(s) = &node_type.sub {
            Ok(Box::new(self.r#type(s)?))
        } else {
            Err(SemanticError::InvalidType(node_type.str.clone()))
        }
    }
    fn get_len(&self, node_type: &node::Type) -> Result<usize, SemanticError> {
        if let Some(l) = &node_type.len {
            Ok(*l)
        } else {
            Err(SemanticError::MissingLen(node_type.str.clone()))
        }
    }

    fn r#macro(&mut self, r#macro: &node::Macro) -> Result<Type, SemanticError> {
        match r#macro {
            node::Macro::Salloc { count: _, ty } => {
                Ok(Type::Pointer(Box::new(self.r#type(ty)?)))
            }
        }
    }
}
