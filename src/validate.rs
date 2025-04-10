use crate::ir::{Block, Symbol};
use crate::types::Type;
use crate::node::{self, ExprKind, PosStr, Span};
use std::collections::HashMap;


/// Validate the ast, return a symbol table if successful
pub fn validate(root: &mut node::Root) -> Result<HashMap<String, Symbol>, (String, SemanticError)> {
    let mut val = Validate::new();
    for (module, stmt) in root.iter() {
        if module != val.cur_module {
            val.cur_module = module.to_string();
        }
        val.hoist_type(stmt).map_err(|e| (module.to_string(), e))?;
    }
    for (module, stmt) in root.iter_mut() {
        if module != val.cur_module {
            val.cur_module = module.to_string();
        }
        val.hoist_func(stmt).map_err(|e| (module.to_string(), e))?;
    }
    println!("{:?}", val.symbol_table);
    for (module, stmt) in root.iter_mut() {
        if module != val.cur_module {
            val.cur_module = module.to_string();
        }
        val.stmt(stmt).map_err(|e| (module.to_string(), e))?;
    }
    Ok(val.symbol_table)
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
    FuncInFunc(PosStr),
    InvalidArgCount(PosStr, usize, usize),
    ArgTypeMismatch(Span, Type, Type),
    NoFnSig(PosStr, Vec<Type>),
    TypeInFunc(PosStr),
    InvalidStructKey(PosStr, PosStr),
    MissingStructKey(PosStr, String),
    StructTypeMismatch(PosStr, Type, Type),
    InvalidMemberAccess(Span),
    EmptyArray(Span),
    ArrayTypeMismatch(Span, Type, Type),
    InvalidCast(Span, Type, Type)
}

pub struct Validate {
    pub symbol_table: HashMap<String, Symbol>,
    cur_module: String,
    fn_map: HashMap<String, Vec<Vec<Type>>>,
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
            cur_module: String::new(),
            fn_map: HashMap::new(),
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

    fn hoist_type(&mut self, stmt: &node::Stmt) -> Result<(), SemanticError> {
        if let node::Stmt::TypeDecl(decl) = stmt {
            if self.symbol_table.contains_key(&decl.name.str) {
                return Err(SemanticError::SymbolExists(decl.name.clone()));
            }
            let ty = self.r#type(&decl.r#type)?;
            self.symbol_table.insert(decl.name.str.clone(), Symbol::Type { ty });
        }
        Ok(())
    }

    fn hoist_func(&mut self, stmt: &mut node::Stmt) -> Result<(), SemanticError> {
        if let node::Stmt::Fn(decl) = stmt {
            let ty;
            if let Some(t) = &decl.decl_type {
                ty = self.r#type(t)?;
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
                let ty = self.r#type(arg.1)?;
                types.push(ty.clone());
                let sym = Symbol::Var { ty };
                arg_symbols.push((arg.0.str.clone(), sym.clone()));
            }
            let s;
            if decl.name.str == "main" {
                if self.symbol_table.contains_key("main") {
                    return Err(SemanticError::SymbolExists(decl.name.clone()));
                }
                s = "main".to_string();
            } else {
                let len;
                if let Some(sigs) = self.fn_map.get_mut(&decl.name.str) {
                    if sigs.contains(&types) {
                        return Err(SemanticError::SymbolExists(decl.name.clone()));
                    }
                    sigs.push(types);
                    len = sigs.len();
                } else {
                    self.fn_map.insert(decl.name.str.clone(), vec![types]);
                    len = 1;
                }
                s = format!("{}.{}", decl.name.str, len);
                decl.name.str = s.clone();
            }
            self.symbol_table.insert(s, Symbol::Func { 
                ty, module: self.cur_module.clone(),
                block: Block::new(),
                symbols: vec![arg_symbols],
            });
        } else if let node::Stmt::Syscall(decl) = stmt {
            let ty;
            if let Some(t) = &decl.decl_type {
                ty = self.r#type(t)?;
            } else {
                ty = Type::Void;
            }
            let mut types = Vec::new();
            for arg in decl.types.iter() {
                let ty = self.r#type(arg)?;
                types.push(ty.clone());
            }
            let s;
            if decl.name.str == "main" {
                if self.symbol_table.contains_key("main") {
                    return Err(SemanticError::SymbolExists(decl.name.clone()));
                }
                s = "main".to_string();
            } else {
                let len;
                if let Some(sigs) = self.fn_map.get_mut(&decl.name.str) {
                    if sigs.contains(&types) {
                        return Err(SemanticError::SymbolExists(decl.name.clone()));
                    }
                    sigs.push(types.clone());
                    len = sigs.len();
                } else {
                    self.fn_map.insert(decl.name.str.clone(), vec![types.clone()]);
                    len = 1;
                }
                s = format!("{}.{}", decl.name.str, len);
                decl.name.str = s.clone();
            }
            self.symbol_table.insert(s, Symbol::Syscall { id: decl.id, ty, args: types });
        }
        Ok(())
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
            node::ExprKind::ArrLit(arr_lit) => self.expr_list(&mut arr_lit.exprs, arr_lit.pos_id)
            .map(|res| Type::Array { inner: Box::new(res.0), length: res.1 }),
            node::ExprKind::StructLit(struct_lit) => self.struct_lit(struct_lit),
            node::ExprKind::StringLit(_) => Ok(Type::String),
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
                    _ => None,
                })
                .ok_or(SemanticError::UndeclaredSymbol(pos_str.clone())),
            node::ExprKind::Call(call) => self.call(call),
            node::ExprKind::BinExpr(bin_expr) => self.bin_expr(bin_expr),
            node::ExprKind::UnExpr(un_expr) => self.un_expr(un_expr, expr.span),
            node::ExprKind::TypeCast(cast) => self.type_cast(cast, expr.span),
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
    fn r#decl(&mut self, decl: &mut node::Decl) -> Result<(), SemanticError> {
        if self.symbol_table.contains_key(&decl.name.str) {
            return Err(SemanticError::SymbolExists(decl.name.clone()));
        }
        let ty = self.r#type(&decl.r#type)?;
        decl.ty = ty.clone();
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

    fn type_decl(&mut self, decl: &node::TypeDecl) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::TypeInFunc(decl.name.clone()));
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
                // if ty1 == Type::Bool && ty2 == Type::Bool {
                //     Ok(Type::Bool)
                // } else {
                //     Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                // }
            }
            "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                // Comparison operators
                if ty1 == ty2 || (ty1 == Type::Int && ty2 == Type::Float) || (ty1 == Type::Float && ty2 == Type::Int) {
                    Ok(Type::Bool)
                } else {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            "=" => {
                // Assignment operator
                if ty1 == ty2 {
                    Ok(ty1)
                } else {
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

    fn call(&mut self, call: &mut node::Call) -> Result<Type, SemanticError> {
        let mut arg_types = Vec::new();
        let mut arg_spans = Vec::new();
        for s in call.args.iter_mut() {
            arg_types.push(self.expr(s)?);
            arg_spans.push(s.span);
        }
        let mut s = call.name.str.clone();
        if let Some(sigs) = self.fn_map.get(&call.name.str) {
            let mut found = false;
            for (i, sig) in sigs.iter().enumerate() {
                if *sig == arg_types {
                    s = format!("{}.{}", s, i + 1);
                    call.name.str = s.clone();
                    found = true;
                    break;
                }
            }
            if !found {
                return Err(SemanticError::NoFnSig(call.name.clone(), arg_types));
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
            return Err(SemanticError::InvalidArgCount(call.name.clone(), expected_types.len(), arg_types.len()));
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

    fn r#type(&self, r#type: &node::Type) -> Result<Type, SemanticError> {
        match &r#type.kind {
            node::TypeKind::Word(word) => match word.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "bool" => Ok(Type::Bool),
                "str" => Ok(Type::String),
                "char" => Ok(Type::Char),
                other => {
                    if let Some(t) = self.symbol_table.get(other) {
                        if let Symbol::Type { ty } = t {
                            return Ok(ty.clone());
                        }
                    }
                    Err(SemanticError::InvalidType(r#type.span))
                }
            }
            node::TypeKind::Pointer(inner) => Ok(Type::Pointer(Box::new(self.r#type(&inner)?))),
            node::TypeKind::Array(inner, len_opt) => if let Some(len) = len_opt {
                Ok(Type::Array { inner: Box::new(self.r#type(&inner)?), length: *len as usize })
            } else {
                Ok(Type::TaggedArray { inner: Box::new(self.r#type(&inner)?)})
            }
            node::TypeKind::Tuple(type_list) => {
                let mut types = Vec::new();
                for arg in type_list.iter() {
                    let ty = self.r#type(arg)?;
                    types.push(ty.clone());
                }
                Ok(Type::Tuple(types))
            }
            node::TypeKind::Struct(node_names, node_types) => {
                let mut i = 0;
                let mut map: HashMap<String, (Type, u32)> = HashMap::new();
                for (node_name, node_ty) in node_names.iter().zip(node_types.iter()) {
                    let ty = self.r#type(node_ty)?;
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

    fn type_cast(&mut self, cast: &mut node::TypeCast, span: Span) -> Result<Type, SemanticError> {
        let from = self.expr(&mut cast.expr)?;
        let to = self.r#type(&cast.r#type)?;
        match (&from, &to) {
            (Type::String, Type::Pointer(inner)) if matches!(**inner, Type::Char) => (),
            (Type::String, Type::Int) => (),
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
