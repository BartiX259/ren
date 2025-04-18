use crate::ir::Symbol;
use crate::types::Type;
use crate::node::{self, ExprKind, PosStr, Span};
use std::collections::HashMap;

mod hoist;

/// Validate the ast, return a symbol table if successful
pub fn validate(module: &mut node::Module, public: Vec<&PublicSymbols>) -> Result<HashMap<String, Symbol>, SemanticError> {
    let mut val = Validate::new();
    for syms in public {
        val.symbol_table.extend(syms.symbol_table.clone());
        val.fn_map.extend(syms.fn_map.clone());
    }
    println!("===========PUBLIC==========\n{:?}\n------------------\n{:?}\n===================", val.symbol_table, val.fn_map);
    for stmt in module.stmts.iter_mut() {
        val.hoist_type(stmt, false)?;
    }
    for stmt in module.stmts.iter_mut() {
        val.hoist_func(stmt, false)?;
    }
    println!("===========HOISTED==========\n{:?}\n------------------\n{:?}\n===================", val.symbol_table, val.fn_map);
    for stmt in module.stmts.iter_mut() {
        val.stmt(stmt)?;
    }
    Ok(val.symbol_table)
}

pub fn hoist_public(module: &mut node::Module, public: &HashMap<String, PublicSymbols>) -> Result<PublicSymbols, SemanticError> {
    let mut val = Validate::new();
    for (_, syms) in public {
        val.fn_map.extend(syms.fn_map.clone());
    }
    for stmt in module.stmts.iter_mut() {
        val.hoist_type(stmt, true)?;
        val.hoist_func(stmt, true)?;
    }
    for (_, syms) in public {
        for (name, sigs) in &syms.fn_map {
            if val.fn_map.get(name).unwrap().len() == sigs.len() {
                val.fn_map.remove(name);
            } else {
                todo!("Function overloading across files");
            }
        }
    }
    Ok(PublicSymbols { symbol_table: val.symbol_table, fn_map: val.fn_map })
}

#[derive(Debug)]
pub struct PublicSymbols {
    symbol_table: HashMap<String, Symbol>,
    fn_map: HashMap<String, Vec<(Vec<Type>, usize)>>
}

pub enum SemanticError {
    InvalidType(Span),
    SymbolExists(PosStr),
    UndeclaredSymbol(PosStr),
    TypeMismatch(PosStr, Type, Type),
    InvalidReturn(usize),
    NotInLoop(String, usize),
    InvalidAssign(Span),
    InvalidUnaryOperator(PosStr),
    InvalidAddressOf(PosStr),
    InvalidDereference(PosStr, Type),
    StructDereference(Span),
    NoIndirection(Span),
    InvalidGlobal(Span),
    FuncInFunc(PosStr),
    InvalidArgCount(Span, usize, usize),
    ArgTypeMismatch(Span, Type, Type),
    NoFnSig(PosStr, Vec<Type>),
    TypeInFunc(PosStr),
    InvalidStructKey(PosStr, PosStr),
    MissingStructKey(PosStr, String),
    StructTypeMismatch(PosStr, Type, Type),
    InvalidMemberAccess(Span),
    EmptyArray(Span),
    ArrayTypeMismatch(Span, Type, Type),
    InvalidCast(Span, Type, Type),
    NoLen(Span, Type)
}

struct Validate {
    symbol_table: HashMap<String, Symbol>,
    fn_map: HashMap<String, Vec<(Vec<Type>, usize)>>,
    cur_module: String,
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
            symbol_table: HashMap::new(),
            fn_map: HashMap::new(),
            cur_module: String::new(),
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
    
    fn stmt(&mut self, stmt: &mut node::Stmt) -> Result<(), SemanticError> {
        match stmt {
            node::Stmt::Expr(expr) => self.expr(expr).map(|_| ()),
            node::Stmt::Let(decl) => self.r#let(decl),
            node::Stmt::Decl(decl) => self.r#decl(decl),
            node::Stmt::Fn(decl) => self.r#fn(decl),
            node::Stmt::TypeDecl(decl) => self.type_decl(decl),
            node::Stmt::Ret(ret) => self.ret(ret),
            node::Stmt::If(r#if) => self.r#if(r#if),
            node::Stmt::Loop(r#loop) => self.r#loop(r#loop),
            node::Stmt::While(r#while) => self.r#while(r#while),
            node::Stmt::For(r#for) => self.r#for(r#for),
            node::Stmt::Break(pos_id) => self.check_loop("Break", *pos_id),
            node::Stmt::Continue(pos_id) => self.check_loop("Continue", *pos_id),
            node::Stmt::Syscall(_) => Ok(()) // Checked while hoisting
        }
    }

    fn expr(&mut self, expr: &mut node::Expr) -> Result<Type, SemanticError> {
        let res = match &mut expr.kind {
            node::ExprKind::IntLit(_) => Ok(Type::Int),
            node::ExprKind::CharLit(_) => Ok(Type::Char),
            node::ExprKind::BoolLit(_) => Ok(Type::Bool),
            node::ExprKind::Null => Ok(Type::Pointer(Box::new(Type::Any))),
            node::ExprKind::ArrLit(arr_lit) => self.expr_list(&mut arr_lit.exprs, arr_lit.pos_id)
            .map(|res| Type::Array { inner: Box::new(res.0), length: res.1 }),
            node::ExprKind::StructLit(struct_lit) => self.struct_lit(struct_lit),
            node::ExprKind::StringLit(lit) => {
                for s in lit {
                    if let node::StringFragment::Expr(e) = s {
                        self.expr(e)?;
                    }
                }
                Ok(Type::String)
            }
            node::ExprKind::TupleLit(exprs) => {
                let types = exprs
                    .iter_mut()
                    .map(|e| self.expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Tuple(types))
            }
            node::ExprKind::Variable(pos_str) => self
                .symbol_table
                .get(&pos_str.str)
                .and_then(|symbol| match symbol {
                    Symbol::Var { ty } => Some(ty.clone()),
                    Symbol::Data { ty, .. } => Some(Type::Pointer(Box::new(ty.clone()))),
                    _ => None,
                })
                .ok_or(SemanticError::UndeclaredSymbol(pos_str.clone())),
            node::ExprKind::Call(call) => self.call(call, expr.span),
            node::ExprKind::BuiltIn(built_in) => self.built_in(built_in, expr.span),
            node::ExprKind::BinExpr(bin_expr) => self.bin_expr(bin_expr),
            node::ExprKind::UnExpr(un_expr) => self.un_expr(un_expr, expr.span),
            node::ExprKind::TypeCast(cast) => {
                let from = self.expr(&mut cast.expr)?;
                let to = self.r#type(&cast.r#type, false)?;
                Self::type_cast(from, to, expr.span)
            }
        };
        if let Ok(ty) = &res {
            expr.ty = ty.clone();
        }
        res
    }

    fn r#let(&mut self, decl: &mut node::Let) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            if self.symbol_table.contains_key(&decl.name.str) {
                return Err(SemanticError::SymbolExists(decl.name.clone()));
            }
            let ty = self.expr(&mut decl.expr)?;
            self.push_symbol(decl.name.str.clone(), Symbol::Var { ty });
        }
        Ok(())
    }
    fn r#decl(&mut self, decl: &mut node::Decl) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            if self.symbol_table.contains_key(&decl.name.str) {
                return Err(SemanticError::SymbolExists(decl.name.clone()));
            }
            let ty = self.r#type(&decl.r#type, false)?;
            decl.ty = ty.clone();
            self.push_symbol(decl.name.str.clone(), Symbol::Var { ty });
        }
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
            .map(|t| self.r#type(t, false))
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

    fn type_decl(&mut self, decl: &node::TypeDecl) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::TypeInFunc(decl.name.clone()));
        }
        Ok(())
    }

    fn node_type(span: Span) -> node::Type {
        node::Type { kind: node::TypeKind::Word("".to_string()), span  }
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
            let span = expr.span;
            let ty = self.expr(expr)?;
            if self.cur_ret != ty {
                Self::type_cast(ty, self.cur_ret.clone(), span)?;
                expr.kind = node::ExprKind::TypeCast(node::TypeCast { r#type: Self::node_type(span), expr: Box::new(expr.clone()) })
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

    fn const_expr(&self, expr: &node::Expr) -> Result<String, SemanticError> {
        let join_exprs = |exprs: &Vec<node::Expr>| {
            let mut str = String::new();
            let mut start = true;
            for e in exprs.iter() {
                if !start {
                    str.push_str(", ");
                } else {
                    start = false;
                }
                str.push_str(&self.const_expr(e)?);
            }
            Ok::<String, SemanticError>(str)
        };
        match &expr.kind {
            ExprKind::IntLit(i) => Ok(i.to_string()),
            ExprKind::CharLit(i) => Ok(i.to_string()),
            ExprKind::ArrLit(arr_lit) => join_exprs(&arr_lit.exprs),
            ExprKind::StructLit(struct_lit) => join_exprs(&struct_lit.field_exprs),
            ExprKind::TupleLit(exprs) => join_exprs(exprs),
            ExprKind::StringLit(string_lit) => {
                if string_lit.len() == 1 {
                    if let Some(node::StringFragment::Lit(lit)) = string_lit.get(0) {
                        return Ok(format!("{lit}"));
                    }
                }
                Err(SemanticError::InvalidGlobal(expr.span))
            }
            _ => Err(SemanticError::InvalidGlobal(expr.span))
        }
    }

    fn is_syntactic_lvalue(&self, expr: &node::Expr) -> bool {
        match &expr.kind {
            ExprKind::Variable(_) => true,
            ExprKind::BinExpr(bin) => {
                if bin.op.str == "." || bin.op.str == "[]" {
                    true
                } else {
                    false
                }
            }
            ExprKind::UnExpr(un) => un.op.str == "*",
            _ => false
        }
    }

    fn bin_expr(&mut self, bin: &mut node::BinExpr) -> Result<Type, SemanticError> {
        let ty1 = self.expr(&mut bin.lhs)?;
        if bin.op.str == "." {
            let ExprKind::Variable(pos_str) = &bin.rhs.kind else {
                return Err(SemanticError::InvalidMemberAccess(bin.rhs.span));
            };
            let map: HashMap<String, (Type, u32)>;
            if let Type::Pointer(p) = ty1 {
                let Type::Struct(m) = *p else {
                    return Err(SemanticError::InvalidMemberAccess(bin.lhs.span));
                };
                map = m;
            } else {
                let Type::Struct(m) = ty1 else {
                    return Err(SemanticError::InvalidMemberAccess(bin.lhs.span));
                };
                map = m
            }
            let Some((ty, _)) = map.get(&pos_str.str) else {
                return Err(SemanticError::InvalidMemberAccess(bin.rhs.span));
            };
            return Ok((*ty).clone());
        }
        let ty2 = self.expr(&mut bin.rhs)?;

        if bin.is_assign() {
            if !self.is_syntactic_lvalue(&*bin.lhs) {
                return Err(SemanticError::InvalidAssign(bin.lhs.span.add(bin.rhs.span)));
            };
        }

        match bin.op.str.as_str() {
            "+" | "-" | "+=" | "-=" => {
                // Arithmetic and pointer arithmetic
                match (&ty1, &ty2) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Float) => Ok(Type::Float),
                    (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float),
                    (Type::Int, Type::Char) => Ok(Type::Int),
                    (Type::Char, Type::Int) => Ok(Type::Char),
                    (Type::Pointer(p), Type::Int) => Ok(Type::Pointer(p.clone())),
                    (Type::Pointer(_), Type::Pointer(_)) => Ok(Type::Int),
                    _ => Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2)),
                }
            }
            "*" | "/" | "*=" | "/=" => {
                // Arithmetic and no pointer arithmetic
                match (&ty1, &ty2) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Float) => Ok(Type::Float),
                    (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float),
                    _ => Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2)),
                }
            }
            "%" | "%=" | "|" | "|=" | "^" | "^=" | "&" | "&=" | "<<" | "<<=" | ">>" | ">>=" => {
                // Integer only operators
                match (&ty1, &ty2) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    _ => Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2)),
                }
            }
            "&&" | "||" => {
                // Logical operators
                Ok(Type::Bool)
            }
            "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                // Comparison operators
                if ty1 == ty2 {
                    Ok(Type::Bool)
                } else {
                    if let Ok(ty) = Self::type_cast(ty1.clone(), ty2.clone(), bin.lhs.span) {
                        bin.lhs.kind = ExprKind::TypeCast(node::TypeCast { r#type: node::Type { 
                            kind: node::TypeKind::Word("void".to_string()), span: bin.lhs.span },
                            expr: bin.lhs.clone()
                        });
                        return Ok(ty);
                    }
                    if let Ok(ty) = Self::type_cast(ty2.clone(), ty1.clone(), bin.rhs.span) {
                        bin.rhs.kind = ExprKind::TypeCast(node::TypeCast { r#type: node::Type { 
                            kind: node::TypeKind::Word("void".to_string()), span: bin.rhs.span },
                            expr: bin.rhs.clone()
                        });
                        return Ok(ty);
                    }
                    // match (&ty1, &ty2) {
                    //     (Type::Int, Type::Char) | (Type::Char, Type::Int) => Ok(Type::Bool),
                    //     _ => Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                    // }
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            "=" => {
                // Assignment operator
                if ty1 == ty2 {
                    Ok(ty1)
                } else {
                    if let Ok(ty) = Self::type_cast(ty2.clone(), ty1.clone(), bin.rhs.span) {
                        bin.rhs.kind = ExprKind::TypeCast(node::TypeCast { r#type: node::Type { 
                            kind: node::TypeKind::Word("void".to_string()), span: bin.rhs.span },
                            expr: bin.rhs.clone()
                        });
                        return Ok(ty);
                    }
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            "[]" => {
                if let Some(t) = ty1.dereference() {
                    if ty2 == Type::Int {
                        Ok(t)
                    } else {
                        Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                    }
                } else if let Type::Tuple(v) = ty1 {
                    if let ExprKind::IntLit(i) = bin.rhs.kind {
                        v.get(i as usize).cloned().ok_or(SemanticError::InvalidMemberAccess(bin.rhs.span))
                    } else {
                        Err(SemanticError::InvalidMemberAccess(bin.rhs.span))
                    }
                } else if let Type::TaggedArray { inner } = &ty1 {
                    if ty2 == Type::Int {
                        Ok(*inner.clone())
                    } else {
                        Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                    }
                } else {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            _ => panic!("Unhandled operator {}", bin.op.str),
        }
    }

    fn un_expr(&mut self, un: &mut node::UnExpr, span: Span) -> Result<Type, SemanticError> {
        let ty = self.expr(&mut un.expr)?;
        match un.op.str.as_str() {
            "-" => Ok(ty),
            "&" => {
                if let node::ExprKind::Variable(_) = un.expr.kind {
                    if let Some(p) = ty.address_of() {
                        Ok(Type::Pointer(Box::new(p)))
                    } else {
                        Ok(Type::Pointer(Box::new(ty)))
                    }
                } else {
                    Err(SemanticError::InvalidAddressOf(un.op.clone()))
                }
            }
            "*" => {
                if let Type::Pointer(t) = ty {
                    if let Type::Struct(_) = *t {
                        Err(SemanticError::StructDereference(span))
                    } else {
                        Ok(*t)
                    }
                } else {
                    Err(SemanticError::InvalidDereference(un.op.clone(), ty))
                }
            }
            "!" => Ok(Type::Bool),
            _ => Err(SemanticError::InvalidUnaryOperator(un.op.clone())),
        }
    }

    fn struct_lit(&mut self, lit: &mut node::StructLit) -> Result<Type, SemanticError> {
        let mut i = 0;
        let mut map: HashMap<String, (Type, u32)> = HashMap::new();
        for (node_name, node_expr) in lit.field_names.iter().zip(lit.field_exprs.iter_mut()) {
            let ty = self.expr(node_expr)?;
            if map.contains_key(&node_name.str) {
                return Err(SemanticError::SymbolExists(node_name.clone()));
            }
            let add = ty.size();
            map.insert(node_name.str.clone(), (ty, i));
            i += add;
        }
        Ok(Type::Struct(map))
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
            return Err(SemanticError::EmptyArray(Span { start: pos_id - 1, end: pos_id}));
        }
        return Ok((cur_type, exprs.len()));
    }

    fn call(&mut self, call: &mut node::Call, span: Span) -> Result<Type, SemanticError> {
        let mut arg_types = Vec::new();
        let mut arg_spans = Vec::new();
        for s in call.args.iter_mut() {
            arg_types.push(self.expr(s)?);
            arg_spans.push(s.span);
        }
        let mut s = call.name.str.clone();
        if let Some(sigs) = self.fn_map.get(&call.name.str) {
            let mut found = false;
            for sig in sigs.iter() {
                if *sig.0 == arg_types {
                    s = format!("{}.{}", s, sig.1);
                    call.name.str = s.clone();
                    found = true;
                    break;
                }
            }
            if !found {
                if sigs.len() != 1 { 
                    return Err(SemanticError::NoFnSig(call.name.clone(), arg_types));
                }
                // Implicit casting
                let tys = sigs.get(0).unwrap().0.iter();
                let id = sigs.get(0).unwrap().1;
                if arg_types.len() != tys.len() {
                    return Err(SemanticError::NoFnSig(call.name.clone(), arg_types));
                }
                for (i, (ty1, ty2)) in arg_types.iter_mut().zip(tys).enumerate() {
                    if ty1 != ty2 {
                        Self::type_cast(ty1.clone(), ty2.clone(), *arg_spans.get(i).unwrap())?;
                        let e = call.args.get(i).unwrap().clone();
                        call.args.get_mut(i).unwrap().kind = ExprKind::TypeCast(node::TypeCast { r#type: node::Type { 
                            kind: node::TypeKind::Word("void".to_string()), span: e.span },
                            expr: Box::new(e)
                        });
                        call.args.get_mut(i).unwrap().ty = ty2.clone();
                        *ty1 = ty2.clone();
                    }
                }
                s = format!("{}.{}", s, id);
                call.name.str = s.clone();
            }
        }
        let ty;
        let expected_types;
        let temp: Vec<_>;
        match self.symbol_table.get(&s) {
            Some(Symbol::Func { ty: t, symbols, .. }) => {
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
            Some(Symbol::Syscall { id: _, ty: t, args }) => {
                ty = t;
                temp = args.iter().collect();
                expected_types = temp;
            }
            None => return Err(SemanticError::UndeclaredSymbol(call.name.clone())),
            _ => return Err(SemanticError::UndeclaredSymbol(call.name.clone())),
        }
        // let mut insert = None;
        if expected_types.len() != arg_types.len() {
            return Err(SemanticError::InvalidArgCount(span, expected_types.len(), arg_types.len()));
        }
        for ((ty1, ty2), span) in expected_types.into_iter().zip(arg_types.iter()).zip(arg_spans.iter()) {
            if ty1 != ty2 {
                return Err(SemanticError::ArgTypeMismatch(*span, ty1.clone(), ty2.clone()));
            }
        }
        let res = ty.clone();
        // if let Some(name) = insert {
        //     let sym = self.symbol_table.get(&s).unwrap().clone();
        //     call.name.str = name.clone();
        //     self.symbol_table.insert(name, sym);
        // }
        Ok(res)
    }

    fn built_in(&mut self, built_in: &mut node::BuiltIn, span: Span) -> Result<Type, SemanticError> {
        match built_in.kind {
            node::BuiltInKind::Len => {
                if built_in.args.len() != 1 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 1));
                }
                let ty = self.expr(built_in.args.get_mut(0).unwrap())?;
                match ty {
                    Type::Array { .. } | Type::TaggedArray { .. } | Type::String => Ok(()),
                    _ => Err(SemanticError::NoLen(span, ty))
                }?;
                Ok(Type::Int)
            }
            node::BuiltInKind::Copy => {
                let [a, b, c] = &mut built_in.args[..] else {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 3));
                };
                let Type::Pointer(_) = self.expr(a)? else {
                    return Err(SemanticError::ArgTypeMismatch(a.span, Type::Pointer(Box::new(Type::Any)), a.ty.clone()));
                };
                let Type::Pointer(_) = self.expr(b)? else {
                    return Err(SemanticError::ArgTypeMismatch(b.span, Type::Pointer(Box::new(Type::Any)), b.ty.clone()));
                };
                let Type::Int = self.expr(c)? else {
                    return Err(SemanticError::ArgTypeMismatch(a.span, Type::Int, c.ty.clone()));
                };
                Ok(Type::Void)
            }
            node::BuiltInKind::StackPointer => {
                if built_in.args.len() != 0 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 0));
                }
                Ok(Type::Pointer(Box::new(Type::Any)))
            }
        }
    }

    fn r#type(&self, r#type: &node::Type, indirection: bool) -> Result<Type, SemanticError> {
        match &r#type.kind {
            node::TypeKind::Word(word) => match word.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "bool" => Ok(Type::Bool),
                "str" => Ok(Type::String),
                "char" => Ok(Type::Char),
                "any" => {
                    if indirection {
                        Ok(Type::Any)
                    } else {
                        Err(SemanticError::NoIndirection(r#type.span))
                    }
                }
                other => {
                    if let Some(t) = self.symbol_table.get(other) {
                        if let Symbol::Type { ty } = t {
                            return Ok(ty.clone());
                        }
                    }
                    Err(SemanticError::InvalidType(r#type.span))
                }
            }
            node::TypeKind::Pointer(inner) => Ok(Type::Pointer(Box::new(self.r#type(&inner, true)?))),
            node::TypeKind::Array(inner, len_opt) => if let Some(len) = len_opt {
                Ok(Type::Array { inner: Box::new(self.r#type(&inner, false)?), length: *len as usize })
            } else {
                Ok(Type::TaggedArray { inner: Box::new(self.r#type(&inner, true)?)})
            }
            node::TypeKind::Tuple(type_list) => {
                let mut types = Vec::new();
                for arg in type_list.iter() {
                    let ty = self.r#type(arg, false)?;
                    types.push(ty.clone());
                }
                Ok(Type::Tuple(types))
            }
            node::TypeKind::Struct(node_names, node_types) => {
                let mut i = 0;
                let mut map: HashMap<String, (Type, u32)> = HashMap::new();
                for (node_name, node_ty) in node_names.iter().zip(node_types.iter()) {
                    let ty = self.r#type(node_ty, false)?;
                    if map.contains_key(&node_name.str) {
                        return Err(SemanticError::SymbolExists(node_name.clone()));
                    }
                    let add = ty.size();
                    map.insert(node_name.str.clone(), (ty, i));
                    i += add;
                }
                Ok(Type::Struct(map))
            }
        }
    }

    fn type_cast(from: Type, to: Type, span: Span) -> Result<Type, SemanticError> {
        match (&from, &to) {
            (Type::String, Type::Pointer(inner)) if matches!(**inner, Type::Char) => (),
            (Type::Int, Type::Char) | (Type::Char, Type::Int) => (),
            (Type::Struct(s1), Type::Struct(s2)) => {
                for (n2, (t2, _)) in s2.iter() {
                    let Some((t1, _)) = s1.get(n2) else {
                        return Err(SemanticError::InvalidCast(span, from, to));
                    };
                    if t1 != t2 {
                        return Err(SemanticError::InvalidCast(span, from, to));
                    }
                }
            }
            (Type::Tuple(tys1), Type::Struct(s)) => {
                let mut tys2: Vec<_> = s.iter().collect();
                tys2.sort_by_key(|(_, (_, offset))| *offset);
                if *tys1 != tys2.iter().map(|(_, (ty, _))| ty.clone()).collect::<Vec<Type>>() {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Struct(s), other) => {
                let first = s.iter().find(|(_, (_, offset))| *offset == 0);
                let mut ok = false;
                if let Some((_, (ty, _))) = first {
                    if ty == other {
                        ok = true;
                    }
                }
                if !ok {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Tuple(tys), other) => {
                let first = tys.get(0);
                let mut ok = false;
                if let Some(ty) = first {
                    if ty == other {
                        ok = true;
                    }
                }
                if !ok {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Pointer(p1), Type::Pointer(p2)) => {
                if **p1 != Type::Any && **p2 != Type::Any  {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::TaggedArray { inner }, Type::Pointer(p)) => {
                if inner != p {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Pointer(_), Type::Int) => (),
            _ => return Err(SemanticError::InvalidCast(span, from, to))
        }
        Ok(to)
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

}
