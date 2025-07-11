use crate::ir::Symbol;
use crate::node::{self, ExprKind, PosStr, Span};
use crate::types::Type;
use std::collections::HashMap;

mod hoist;

/// Validate the ast, return a symbol table if successful
pub fn validate(
    module: &mut node::Module,
    public: Vec<&PublicSymbols>,
    gcalls: &mut Vec<(String, HashMap<String, Type>, usize)>,
    gfns: &mut Vec<(String, usize)>,
) -> Result<PublicSymbols, SemanticError> {
    let mut val = Validate::new();
    for syms in public {
        val.symbol_table.extend(syms.symbol_table.clone());
        val.fn_map.extend(syms.fn_map.clone());
    }
    val.gcalls = gcalls.clone();
    val.gfns = gfns.drain(..).collect();
    val.cur_module = module.path.clone();
    for stmt in module.stmts.iter_mut() {
        val.hoist_type(stmt, false)?;
    }
    for stmt in module.stmts.iter_mut() {
        val.hoist_func(stmt, false)?;
    }
    for stmt in module.stmts.iter_mut() {
        val.stmt(stmt)?;
    }
    gcalls.extend(val.gcalls[gcalls.len()..].to_vec());
    *gfns = val.gfns;
    Ok(PublicSymbols {
        symbol_table: val.symbol_table,
        fn_map: val.fn_map,
    })
}

pub fn resolve(
    module: node::Module,
    symbols: PublicSymbols,
    gcalls: &mut Vec<(String, HashMap<String, Type>, usize)>,
    gfns: &mut Vec<(String, usize)>,
) -> Result<(PublicSymbols, node::Module), SemanticError> {
    let mut val = Validate::new();
    let mut new_mod = node::Module { path: module.path, stmts: Vec::new() };
    let mut to_remove = Vec::new();
    val.symbol_table = symbols.symbol_table;
    val.gfns = gfns.drain(..).collect();
    val.fn_map = symbols.fn_map;
    for stmt in module.stmts.into_iter() {
        let mut stmt_ref = &stmt;
        let mut is_public = false;
        if let node::Stmt::Decorator(dec) = &stmt {
            stmt_ref = dec.inner.as_ref();
            if dec.kinds.contains(&node::DecoratorKind::Pub) {
                is_public = true;
            }
        }
        if let node::Stmt::Fn(decl) = stmt_ref {
            if decl.generics.len() > 0 {
                for (index, (name, map, i)) in gcalls.iter().enumerate() {
                    val.cur_generics = map.keys().cloned().collect();
                    if *name == decl.name.str {
                        let mut ty = Type::Void;
                        let mut types = Vec::new();
                        if let Some(decl_type) = &decl.decl_type {
                            let generic_ty = val.r#type(decl_type, false)?;
                            ty = generic_ty.substitute_generics(map);
                        }
                        for ty_node in decl.arg_types.iter() {
                            let generic_ty = val.r#type(ty_node, false)?;
                            types.push(generic_ty.substitute_generics(map));
                        }
                        let mut new_decl = decl.clone();
                        for (k, v) in map {
                            if val.symbol_table.contains_key(k) {
                                return Err(SemanticError::SymbolExists(PosStr {
                                    str: k.clone(),
                                    pos_id: decl.name.pos_id,
                                }));
                            }
                            val.symbol_table.insert(k.clone(), Symbol::Type { ty: v.clone() });
                        }
                        new_decl.generics.clear();
                        new_decl.name.str += format!(".{i}").as_str();
                        val.hoist_func_with_types(&mut new_decl, ty, types, false, is_public)?;
                        val.resolving.push((decl.name.str.clone(), map.clone(), *i));
                        val.r#fn(&mut new_decl)?;
                        val.resolving.pop();
                        for k in map.keys() {
                            val.symbol_table.remove(k);
                        }
                        new_mod.stmts.push(node::Stmt::Fn(new_decl));
                        to_remove.push(index);
                    }
                }
                val.cur_generics = vec![];
                new_mod.stmts.push(stmt);
            } else {
                new_mod.stmts.push(stmt);
            }
            continue;
        }
        new_mod.stmts.push(stmt);
    }
    to_remove.sort_unstable_by(|a, b| b.cmp(a)); // sort in descending order
    for i in to_remove {
        gcalls.swap_remove(i);
    }
    for g in val.gcalls {
        gcalls.push(g);
    }
    *gfns = val.gfns;
    Ok((
        PublicSymbols {
            symbol_table: val.symbol_table,
            fn_map: val.fn_map,
        },
        new_mod,
    ))
}

pub fn clean_up(module: node::Module, symbols: PublicSymbols) -> (node::Module, HashMap<String, Symbol>) {
    let mut val = Validate::new();
    let mut new_mod = node::Module { path: module.path, stmts: Vec::new() };
    val.symbol_table = symbols.symbol_table;
    val.fn_map = symbols.fn_map;
    for stmt in module.stmts.into_iter() {
        if let node::Stmt::MainFn(decl) = stmt {
            new_mod.stmts.push(node::Stmt::Fn(node::Fn {
                name: PosStr {
                    str: "main".to_string(),
                    pos_id: decl.pos_id,
                },
                arg_names: decl.arg_names,
                arg_types: decl.arg_types,
                decl_type: decl.decl_type,
                generics: vec![],
                scope: decl.scope,
            }));
            continue;
        }
        let mut stmt_ref = &stmt;
        if let node::Stmt::Decorator(dec) = &stmt {
            stmt_ref = dec.inner.as_ref();
        }
        if let node::Stmt::Fn(decl) = stmt_ref {
            if decl.generics.len() > 0 {
                val.symbol_table.remove(&decl.name.str);
                continue;
            }
        }
        new_mod.stmts.push(stmt);
    }
    (new_mod, val.symbol_table)
}

pub fn hoist_public(module: &mut node::Module, public: &HashMap<String, PublicSymbols>, gfns: &mut Vec<(String, usize)>) -> Result<PublicSymbols, SemanticError> {
    let mut val = Validate::new();
    val.gfns = gfns.to_vec();
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
    gfns.extend(val.gfns[gfns.len()..].to_vec());
    Ok(PublicSymbols {
        symbol_table: val.symbol_table,
        fn_map: val.fn_map,
    })
}

#[derive(Debug)]
pub struct PublicSymbols {
    pub symbol_table: HashMap<String, Symbol>,
    fn_map: HashMap<String, Vec<(Vec<Type>, usize)>>,
}

pub enum SemanticError {
    InvalidType(Span),
    SymbolExists(PosStr),
    UndeclaredSymbol(PosStr),
    TypeMismatch(PosStr, Type, Type),
    UnexpectedType(Span, Type, Type),
    NotIterable(Span, Type),
    NotUnwrappable(PosStr),
    NoCapture(Span),
    NoMatch(usize, Type),
    InvalidReturn(usize),
    NotInLoop(String, usize),
    InvalidAssign(Span),
    ConstAssign(Span, Type),
    InvalidUnaryOperator(PosStr),
    UnaryTypeMismatch(Span, String, Type),
    InvalidAddressOf(PosStr),
    InvalidDereference(PosStr, Type),
    StructDereference(Span),
    NoIndirection(Span),
    InvalidGlobal(Span),
    FuncInFunc(PosStr),
    TypeInFunc(PosStr),
    DecoratorInFunc(Span),
    InvalidArgCount(Span, usize, usize),
    InvalidTypeArgCount(Span, usize, usize),
    ArgTypeMismatch(Span, Type, Type),
    NoFnSig(String, Span, Vec<Type>, Option<Type>),
    InvalidStructKey(PosStr, PosStr),
    MissingStructKey(PosStr, String),
    StructTypeMismatch(PosStr, Type, Type),
    InvalidMemberAccess(Span),
    EmptyArray(Span),
    ArrayTypeMismatch(Span, Type, Type),
    GenericTypeMismatch(Span, Type, Type),
    NoSlice(Span, String),
    InvalidCast(Span, Type, Type),
    NoBuiltIn(Span, String, Type),
    PrivateAccess(PosStr),
    MainFnCall(PosStr),
}

struct Validate {
    symbol_table: HashMap<String, Symbol>,
    fn_map: HashMap<String, Vec<(Vec<Type>, usize)>>,
    gfns: Vec<(String, usize)>,
    gcalls: Vec<(String, HashMap<String, Type>, usize)>,
    resolving: Vec<(String, HashMap<String, Type>, usize)>,
    cur_module: String,
    symbol_stack: Vec<Vec<String>>,
    cur_func: Option<String>,
    cur_ret: Type,
    cur_generics: Vec<String>,
    loop_count: usize,
}

impl Validate {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            fn_map: HashMap::new(),
            gfns: Vec::new(),
            gcalls: Vec::new(),
            resolving: Vec::new(),
            cur_module: String::new(),
            symbol_stack: Vec::new(),
            cur_func: None,
            cur_ret: Type::Void,
            cur_generics: Vec::new(),
            loop_count: 0,
        }
    }

    fn push_symbol(&mut self, name: String, sym: Symbol) {
        self.symbol_table.insert(name.clone(), sym.clone());
        self.symbol_stack.last_mut().unwrap().push(name.clone());
    }

    fn stmt(&mut self, stmt: &mut node::Stmt) -> Result<(), SemanticError> {
        match stmt {
            node::Stmt::Expr(expr) => self.expr(expr).map(|_| ()),
            node::Stmt::Let(decl) => self.r#let(decl),
            node::Stmt::Decl(decl) => self.r#decl(decl),
            node::Stmt::LetElseExpr(else_expr) => self.else_expr(else_expr),
            node::Stmt::LetElseScope(else_scope) => self.else_scope(else_scope),
            node::Stmt::Fn(decl) => self.r#fn(decl),
            node::Stmt::MainFn(decl) => self.main_fn(decl),
            node::Stmt::TypeDecl(decl) => self.type_decl(&decl.name),
            node::Stmt::Enum(e) => self.type_decl(&e.name),
            node::Stmt::Decorator(dec) => self.decorator(dec),
            node::Stmt::Ret(ret) => self.ret(ret),
            node::Stmt::If(r#if) => self.r#if(r#if),
            node::Stmt::Loop(r#loop) => self.r#loop(r#loop),
            node::Stmt::While(r#while) => self.r#while(r#while),
            node::Stmt::For(r#for) => self.r#for(r#for),
            node::Stmt::ForIn(r#for) => self.for_in(r#for),
            node::Stmt::Break(pos_id) => self.check_loop("Break", *pos_id),
            node::Stmt::Continue(pos_id) => self.check_loop("Continue", *pos_id),
            node::Stmt::Match(m) => self.r#match(m),
            node::Stmt::MatchType(m) => self.match_type(m),
            node::Stmt::Extern(ext) => self.r#extern(ext),
            node::Stmt::Syscall(_) => Ok(()), // Checked while hoisting
            node::Stmt::Marker => Ok(()),
        }
    }

    fn expr(&mut self, expr: &mut node::Expr) -> Result<Type, SemanticError> {
        let res = match &mut expr.kind {
            node::ExprKind::IntLit(_) => Ok(Type::Int),
            node::ExprKind::CharLit(_) => Ok(Type::Char),
            node::ExprKind::BoolLit(_) => Ok(Type::Bool),
            node::ExprKind::Null => Ok(Type::Pointer(Box::new(Type::Any))),
            node::ExprKind::None => Ok(Type::Option(Box::new(Type::Int))),
            node::ExprKind::ArrLit(arr_lit) => self.expr_list(&mut arr_lit.exprs, arr_lit.pos_id).map(|res| Type::Array {
                inner: Box::new(res.0),
                length: res.1,
            }),
            node::ExprKind::ListLit(..) => unreachable!(),
            node::ExprKind::StructLit(struct_lit) => self.struct_lit(struct_lit),
            node::ExprKind::StringLit(lit, alloc_fn) => {
                if lit.len() == 1 {
                    Ok(Type::Slice { inner: Box::new(Type::Char) })
                } else {
                    *alloc_fn = self.find_fn("alloc", vec![Type::Int], Some(Type::Pointer(Box::new(Type::Any))), &mut vec![], expr.span)?;
                    for s in lit {
                        if let node::StringFragment::Expr { expr, str_fn } = s {
                            let ty = self.expr(expr)?;
                            let span = expr.span;
                            let mut args = vec![expr.clone()];
                            *str_fn = self.find_fn("str", vec![ty], Some(Type::Slice { inner: Box::new(Type::Char) }), &mut args, span)?;
                            if let Some(new_expr) = args.into_iter().next() {
                                *expr = new_expr;
                            }
                        }
                    }
                    Ok(Type::List { inner: Box::new(Type::Char) })
                }
            }
            node::ExprKind::TupleLit(exprs) => {
                let types = exprs.iter_mut().map(|e| self.expr(e)).collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Tuple(types))
            }
            node::ExprKind::MapLit(map) => {
                let key_ty = self.expr_list(&mut map.keys, map.pos_id)?.0;
                let val_ty = self.expr_list(&mut map.values, map.pos_id)?.0;
                let init_fn = self.find_fn(
                    "init_map",
                    vec![Type::Slice {
                        inner: Box::new(Type::Tuple(vec![key_ty.clone(), val_ty.clone()])),
                    }],
                    Some(Type::HashMap {
                        key: Box::new(key_ty.clone()),
                        value: Box::new(val_ty.clone()),
                    }),
                    &mut vec![],
                    Span { start: map.pos_id, end: map.pos_id },
                )?;
                map.init_fn = init_fn;
                Ok(Type::HashMap {
                    key: Box::new(key_ty),
                    value: Box::new(val_ty),
                })
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
            node::ExprKind::BinExpr(bin_expr) => self.bin_expr(bin_expr, expr.span),
            node::ExprKind::UnExpr(un_expr) => self.un_expr(un_expr, expr.span),
            node::ExprKind::PostUnExpr(un_expr) => self.post_un_expr(un_expr, expr.span),
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

    fn unpack(&mut self, unpack: &mut node::Unpack) -> Result<Option<Type>, SemanticError> {
        let mut res = None;
        if let Some(rhs) = &unpack.rhs {
            let Some(s) = self.symbol_table.get(&unpack.lhs.str).cloned() else {
                return Err(SemanticError::UndeclaredSymbol(unpack.lhs.clone()));
            };
            let Symbol::Type { ty } = s else { return Err(SemanticError::NotUnwrappable(unpack.lhs.clone())) };
            let Type::Enum(vars) = ty else { return Err(SemanticError::NotUnwrappable(unpack.lhs.clone())) };
            let Some(pos) = vars.iter().position(|(p, ty)| *p == rhs.str && ty.is_some() == unpack.brackets.is_some()) else {
                return Err(SemanticError::InvalidMemberAccess(Span { start: rhs.pos_id, end: rhs.pos_id }));
            };
            if let Some(s) = &unpack.brackets {
                let ty = vars.get(pos).unwrap().1.clone().unwrap();
                res = Some(ty.clone());
                self.push_symbol(s.str.clone(), Symbol::Var { ty });
            }
            let ty = self.expr(&mut unpack.expr)?;
            let pos_str = PosStr {
                str: "let unpack".to_string(),
                pos_id: unpack.lhs.pos_id - 1,
            };
            let Type::Enum(vars2) = &ty else {
                return Err(SemanticError::TypeMismatch(pos_str, Type::Enum(vars), ty));
            };
            if vars != *vars2 {
                return Err(SemanticError::TypeMismatch(pos_str, Type::Enum(vars), ty));
            }
        } else {
            let ty = self.expr(&mut unpack.expr)?;
            match &ty {
                Type::Result(ok, _) | Type::Option(ok) => {
                    res = Some(*ok.clone());
                    self.push_symbol(unpack.lhs.str.clone(), Symbol::Var { ty: *ok.clone() });
                }
                _ => return Err(SemanticError::NotUnwrappable(unpack.lhs.clone())),
            }
        }
        Ok(res)
    }

    fn r#fn(&mut self, decl: &mut node::Fn) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::FuncInFunc(decl.name.clone()));
        }
        if decl.generics.len() > 0 {
            return Ok(());
        }

        // Retrieve arguments and return type
        let mut arg_types = vec![];
        self.symbol_table
            .get(&decl.name.str)
            .and_then(|sym| match sym {
                Symbol::Func { ty, args, .. } => {
                    self.cur_ret = ty.clone();
                    arg_types = args.clone();
                    Some(())
                }
                _ => None,
            })
            .expect(&format!("Function symbol not found {}", decl.name.str));

        for (name, ty) in decl.arg_names.iter().zip(arg_types) {
            self.symbol_table.insert(name.str.clone(), Symbol::Var { ty: ty.clone() });
        }

        self.cur_func = Some(decl.name.str.clone());

        self.scope(&mut decl.scope)?;

        for name in decl.arg_names.iter() {
            self.symbol_table.remove(&name.str);
        }

        self.cur_ret = Type::Void;
        self.cur_func = None;

        Ok(())
    }

    fn main_fn(&mut self, decl: &mut node::MainFn) -> Result<(), SemanticError> {
        let pos_str = PosStr {
            str: "main".to_string(),
            pos_id: decl.pos_id,
        };
        let span = Span { start: decl.pos_id, end: decl.pos_id };
        if self.cur_func.is_some() {
            return Err(SemanticError::FuncInFunc(pos_str));
        }
        let ty = if let Some(t) = &decl.decl_type { self.r#type(&t, false)? } else { Type::Void };
        let mut arg_types = Vec::new();
        for arg in decl.arg_types.iter() {
            let ty = self.r#type(arg, false)?;
            arg_types.push(ty.clone());
        }
        let init = self.find_fn("init", vec![], Some(Type::Void), &mut vec![], span).unwrap_or("".to_string());
        let arg_parse = self
            .find_fn(
                "arg_parse",
                vec![
                    Type::Slice {
                        inner: Box::new(Type::Pointer(Box::new(Type::Char))),
                    },
                    Type::Slice {
                        inner: Box::new(Type::Slice { inner: Box::new(Type::Char) }),
                    },
                ],
                Some(Type::Result(Box::new(Type::Int), Box::new(Type::Slice { inner: Box::new(Type::Char) }))),
                &mut vec![],
                span,
            )
            .unwrap_or("".to_string());
        let print_help = self
            .find_fn(
                "print_help",
                vec![
                    Type::Pointer(Box::new(Type::Char)),
                    Type::Slice {
                        inner: Box::new(Type::Slice { inner: Box::new(Type::Char) }),
                    },
                ],
                Some(Type::Void),
                &mut vec![],
                span,
            )
            .unwrap_or("".to_string());
        let print = self
            .find_fn("print", vec![Type::Slice { inner: Box::new(Type::Char) }], Some(Type::Void), &mut vec![], span)
            .unwrap_or("".to_string());
        let mut parse_fns = vec![];
        for (name, arg) in decl.arg_names.iter().zip(arg_types.iter()) {
            if let Type::Option(_) = &arg {
                parse_fns.push(self.find_fn(
                    "parse_opt",
                    vec![
                        Type::Pointer(Box::new(Type::Int)),
                        Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                        Type::Slice { inner: Box::new(Type::Char) },
                        Type::Pointer(Box::new(arg.clone())),
                    ],
                    Some(Type::Result(Box::new(Type::Int), Box::new(Type::Slice { inner: Box::new(Type::Char) }))),
                    &mut vec![],
                    Span { start: name.pos_id, end: name.pos_id },
                )?);
            } else {
                parse_fns.push(self.find_fn(
                    "parse",
                    vec![Type::Pointer(Box::new(Type::Char)), Type::Pointer(Box::new(arg.clone()))],
                    Some(Type::Result(Box::new(Type::Int), Box::new(Type::Slice { inner: Box::new(Type::Char) }))),
                    &mut vec![],
                    Span { start: name.pos_id, end: name.pos_id },
                )?);
            }
        }
        self.symbol_table.insert(
            "main".to_string(),
            Symbol::MainFunc {
                ty: ty.clone(),
                block: crate::ir::Block::new(),
                module: self.cur_module.clone(),
                args: arg_types.clone(),
                init,
                arg_parse,
                print,
                print_help,
                arg_names: decl.arg_names.iter().map(|pos_str| pos_str.str.clone()).collect(),
                parse_fns,
                span,
            },
        );
        for (name, ty) in decl.arg_names.iter().zip(arg_types) {
            self.symbol_table.insert(name.str.clone(), Symbol::Var { ty: ty.clone() });
        }

        self.cur_func = Some("main".to_string());

        self.scope(&mut decl.scope)?;

        for name in decl.arg_names.iter() {
            self.symbol_table.remove(&name.str);
        }

        self.cur_ret = Type::Void;
        self.cur_func = None;
        Ok(())
    }

    fn type_decl(&mut self, pos_str: &PosStr) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::TypeInFunc(pos_str.clone()));
        }
        Ok(())
    }

    fn decorator(&mut self, dec: &mut node::Decorator) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::DecoratorInFunc(dec.span));
        }
        self.stmt(dec.inner.as_mut())
    }

    fn node_type(span: Span) -> node::Type {
        node::Type {
            kind: node::TypeKind::Word("".to_string()),
            span,
        }
    }

    fn scope(&mut self, scope: &mut Vec<node::Stmt>) -> Result<(), SemanticError> {
        self.symbol_stack.push(Vec::new());
        for stmt in scope.iter_mut() {
            self.stmt(stmt)?;
        }
        // Remove the local symbols from the symbol table
        for sym in self.symbol_stack.pop().unwrap() {
            self.symbol_table.remove(&sym);
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
                expr.kind = node::ExprKind::TypeCast(node::TypeCast {
                    r#type: Self::node_type(span),
                    expr: Box::new(expr.clone()),
                });
                expr.ty = self.cur_ret.clone();
            }
        } else if self.cur_ret != Type::Void {
            return Err(SemanticError::InvalidReturn(ret.pos_id));
        }
        Ok(())
    }

    fn r#if(&mut self, r#if: &mut node::If) -> Result<(), SemanticError> {
        match &mut r#if.cond {
            node::IfKind::Expr(expr) => self.expr(expr).map(|_| ())?,
            node::IfKind::Unpack(unpack) => self.unpack(unpack).map(|_| ())?,
            node::IfKind::None => (),
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
            ExprKind::StringLit(string_lit, _) => {
                if string_lit.len() == 1 {
                    if let Some(node::StringFragment::Lit(lit)) = string_lit.get(0) {
                        return Ok(format!("{lit}"));
                    }
                }
                Err(SemanticError::InvalidGlobal(expr.span))
            }
            _ => Err(SemanticError::InvalidGlobal(expr.span)),
        }
    }

    fn is_syntactic_lvalue(&self, expr: &node::Expr) -> bool {
        match &expr.kind {
            ExprKind::Variable(_) => true,
            ExprKind::BinExpr(bin) => {
                if bin.op.str == "." || bin.op.str.starts_with("[]") {
                    true
                } else {
                    false
                }
            }
            ExprKind::UnExpr(un) => un.op.str == "*",
            _ => false,
        }
    }

    fn bin_expr(&mut self, bin: &mut node::BinExpr, span: Span) -> Result<Type, SemanticError> {
        if bin.op.str == "." {
            if let ExprKind::Variable(e) = &bin.lhs.kind {
                if let Some(Symbol::Type { ty }) = self.symbol_table.get(&e.str).cloned() {
                    if let Type::Enum(vars) = ty {
                        if let ExprKind::Variable(pos_str) = &bin.rhs.kind {
                            let Some(_) = vars.iter().position(|(p, ty)| *p == pos_str.str && ty.is_none()) else {
                                return Err(SemanticError::InvalidMemberAccess(bin.rhs.span));
                            };
                        } else if let ExprKind::Call(call) = &mut bin.rhs.kind {
                            if call.args.len() != 1 {
                                return Err(SemanticError::InvalidArgCount(bin.rhs.span, call.args.len(), 1));
                            }
                            let mut arg = call.args.get_mut(0).unwrap();
                            let t = Some(self.expr(&mut arg)?);
                            let Some(_) = vars.iter().position(|(p, ty)| *p == call.name.str && *ty == t) else {
                                return Err(SemanticError::InvalidMemberAccess(bin.rhs.span));
                            };
                        }
                        bin.lhs.ty = Type::Enum(vars.to_vec());
                        return Ok(Type::Enum(vars.to_vec()));
                    }
                }
            }
        }
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
                return Err(SemanticError::InvalidAssign(span));
            };
            if let ExprKind::BinExpr(b) = &bin.lhs.kind {
                if b.op.str.starts_with("[]") {
                    if let Type::Slice { .. } = b.lhs.ty {
                        return Err(SemanticError::ConstAssign(bin.lhs.span, b.lhs.ty.clone()));
                    }
                    if let Type::HashMap { key, value } = &b.lhs.ty {
                        if bin.op.str != "=" {
                            return Err(SemanticError::InvalidAssign(Span {
                                start: bin.op.pos_id,
                                end: bin.op.pos_id,
                            }));
                        }
                        let ins_fn = self.find_fn("insert", vec![Type::Pointer(Box::new(b.lhs.ty.clone())), *key.clone(), *value.clone()], None, &mut vec![], bin.rhs.span)?;
                        bin.op.str += &ins_fn;
                        return Ok(Type::Void);
                    }
                }
            }
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
                    (Type::Char, Type::Char) => Ok(Type::Char),
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
                // if ty1.size() > 8 || ty2.size() > 8 {
                //     Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                // } else 
                // if ty1.size() > 8 || ty2.size() > 8 {
                //     panic!("{:?}\n\n{:?}", bin.lhs, bin.rhs);
                // }
                if ty1 == ty2 {
                    Ok(Type::Bool)
                } else {
                    if let Ok(ty) = Self::type_cast(ty1.clone(), ty2.clone(), bin.lhs.span) {
                        bin.lhs.kind = ExprKind::TypeCast(node::TypeCast {
                            r#type: Self::node_type(bin.lhs.span),
                            expr: bin.lhs.clone(),
                        });
                        bin.lhs.ty = ty;
                        return Ok(Type::Bool);
                    }
                    if let Ok(ty) = Self::type_cast(ty2.clone(), ty1.clone(), bin.rhs.span) {
                        bin.rhs.kind = ExprKind::TypeCast(node::TypeCast {
                            r#type: Self::node_type(bin.rhs.span),
                            expr: bin.rhs.clone(),
                        });
                        bin.rhs.ty = ty;
                        return Ok(Type::Bool);
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
                        bin.rhs.kind = ExprKind::TypeCast(node::TypeCast {
                            r#type: Self::node_type(bin.rhs.span),
                            expr: bin.rhs.clone(),
                        });
                        return Ok(ty);
                    }
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2))
                }
            }
            ".." => {
                // Range operator
                if ty1 != Type::Int {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, Type::Int))
                } else if ty2 != Type::Int {
                    Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, Type::Int))
                } else {
                    Ok(Type::Range)
                }
            }
            "[]" => {
                // Helper closure to handle indexed types
                let handle_index = |inner: Type| {
                    if ty2 == Type::Int {
                        Ok(inner)
                    } else if ty2 == Type::Range {
                        match &bin.lhs.kind {
                            node::ExprKind::ArrLit(_) => Err(SemanticError::NoSlice(span, "an array literal".to_string())),
                            node::ExprKind::StringLit(_, _) => Err(SemanticError::NoSlice(span, "a string literal".to_string())),
                            node::ExprKind::UnExpr(_) => Err(SemanticError::NoSlice(span, "an unowned allocation".to_string())),
                            _ => Ok(Type::Slice { inner: Box::new(inner) }),
                        }
                    } else {
                        Err(SemanticError::TypeMismatch(bin.op.clone(), ty1.clone(), ty2.clone()))
                    }
                };

                if let Some(inner) = ty1.dereference() {
                    handle_index(inner)
                } else {
                    match &ty1 {
                        Type::Tuple(v) => {
                            if let ExprKind::IntLit(i) = bin.rhs.kind {
                                v.get(i as usize).cloned().ok_or(SemanticError::InvalidMemberAccess(bin.rhs.span))
                            } else {
                                Err(SemanticError::InvalidMemberAccess(bin.rhs.span))
                            }
                        }
                        Type::Range => {
                            if let ExprKind::IntLit(i) = bin.rhs.kind {
                                if i == 0 || i == 1 {
                                    Ok(Type::Int)
                                } else {
                                    Err(SemanticError::InvalidMemberAccess(bin.rhs.span))
                                }
                            } else {
                                Err(SemanticError::InvalidMemberAccess(bin.rhs.span))
                            }
                        }
                        Type::Array { inner, .. } | Type::Slice { inner } | Type::List { inner } => handle_index(*inner.clone()),
                        Type::HashMap { key, value } => {
                            if bin.rhs.ty == **key {
                                let get_fn = self.find_fn("get", vec![ty1.clone(), *key.clone()], Some(Type::Option(value.clone())), &mut vec![], bin.rhs.span)?;
                                bin.op.str += &get_fn;
                                Ok(Type::Option(value.clone()))
                            } else {
                                Err(SemanticError::InvalidMemberAccess(bin.rhs.span))
                            }
                        }
                        _ => Err(SemanticError::TypeMismatch(bin.op.clone(), ty1, ty2)),
                    }
                }
            }

            _ => panic!("Unhandled operator {}", bin.op.str),
        }
    }

    fn un_expr(&mut self, un: &mut node::UnExpr, span: Span) -> Result<Type, SemanticError> {
        let ty = self.expr(&mut un.expr)?;
        match un.op.str.as_str() {
            "-" => match ty {
                Type::Int | Type::Float => Ok(ty),
                _ => Err(SemanticError::UnaryTypeMismatch(span, un.op.str.clone(), ty)),
            },
            "+" => {
                if let node::ExprKind::ArrLit(_) = un.expr.kind {
                    let Type::Array { inner, .. } = ty else {
                        panic!("not array");
                    };
                    let func = self.find_fn("alloc", vec![Type::Int], Some(Type::Pointer(Box::new(Type::Any))), &mut vec![], span)?;
                    un.op.str += &func;
                    Ok(Type::List { inner })
                } else if let Type::Slice { inner } = &un.expr.ty {
                    let func = self.find_fn("alloc", vec![Type::Int], Some(Type::Pointer(Box::new(Type::Any))), &mut vec![], span)?;
                    un.op.str += &func;
                    Ok(Type::List { inner: inner.clone() })
                } else {
                    Err(SemanticError::UnaryTypeMismatch(span, un.op.str.clone(), ty))
                }
            }
            "&" => {
                if let node::ExprKind::Variable(_) = un.expr.kind {
                    // if let Some(p) = ty.address_of() {
                    //     Ok(Type::Pointer(Box::new(p)))
                    // } else {
                    //     Ok(Type::Pointer(Box::new(ty)))
                    // }
                    Ok(Type::Pointer(Box::new(ty)))
                } else if let node::ExprKind::BinExpr(b) = &un.expr.kind {
                    if b.op.str == "." {
                        if let node::ExprKind::Variable(l) = &b.lhs.kind {
                            if let node::ExprKind::Variable(r) = &b.rhs.kind {
                                let ty;
                                match self.symbol_table.get(&l.str) {
                                    Some(Symbol::Var { ty: var_ty }) => ty = var_ty,
                                    Some(Symbol::Data { ty: data_ty, .. }) => ty = data_ty,
                                    _ => return Err(SemanticError::UndeclaredSymbol(l.clone())),
                                }
                                let Type::Struct(map) = ty else {
                                    return Err(SemanticError::InvalidMemberAccess(un.expr.span));
                                };
                                let Some((ty, _)) = map.get(&r.str) else {
                                    return Err(SemanticError::InvalidMemberAccess(un.expr.span));
                                };
                                return Ok(Type::Pointer(Box::new(ty.clone())));
                            }
                        }
                    }
                    Err(SemanticError::InvalidAddressOf(un.op.clone()))
                } else {
                    Err(SemanticError::InvalidAddressOf(un.op.clone()))
                }
            }
            "*" => {
                if let Type::Pointer(t) = &ty {
                    // if t.size() > 8 {
                    //     Err(SemanticError::InvalidDereference(un.op.clone(), ty))
                    // } else {
                    //     Ok(*t.clone())
                    // }
                    Ok(*t.clone())
                } else {
                    Err(SemanticError::InvalidDereference(un.op.clone(), ty))
                }
            }
            "!" => Ok(Type::Bool),
            "~" => {
                if ty == Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(SemanticError::UnaryTypeMismatch(span, un.op.str.clone(), ty))
                }
            }
            "?" => {
                if let Type::Result(_, err) = &self.cur_ret {
                    if ty != **err {
                        Self::type_cast(ty.clone(), *err.clone(), span)?;
                        un.expr.ty = *err.clone();
                        un.expr.kind = node::ExprKind::TypeCast(node::TypeCast {
                            r#type: Self::node_type(span),
                            expr: un.expr.clone(),
                        })
                    }
                    Ok(self.cur_ret.clone())
                } else {
                    Err(SemanticError::UnaryTypeMismatch(span, un.op.str.clone(), ty))
                }
            }
            _ => Err(SemanticError::InvalidUnaryOperator(un.op.clone())),
        }
    }

    fn post_un_expr(&mut self, un: &mut node::UnExpr, span: Span) -> Result<Type, SemanticError> {
        let ty = self.expr(&mut un.expr)?;
        match un.op.str.as_str() {
            "?" => {
                if let Type::Result(ty, err) = ty {
                    let mut ok = false;
                    if let Type::Result(_, e2) = &self.cur_ret {
                        if err == *e2 {
                            ok = true;
                        }
                    } else if let Type::Option(_) = &self.cur_ret {
                        ok = true
                    }
                    if ok {
                        Ok(*ty)
                    } else {
                        Err(SemanticError::InvalidReturn(un.op.pos_id))
                    }
                } else if let Type::Option(opt) = ty {
                    let mut ok = false;
                    if let Type::Option(_) = &self.cur_ret {
                        ok = true;
                    }
                    if ok {
                        Ok(*opt)
                    } else {
                        Err(SemanticError::InvalidReturn(un.op.pos_id))
                    }
                } else {
                    Err(SemanticError::UnaryTypeMismatch(span, "?".to_string(), ty))
                }
            }
            "!" => {
                if let Type::Result(ty, err) = ty {
                    let call;
                    if let Ok(c) = self.find_fn("panic", vec![*err], None, &mut vec![], span) {
                        call = c;
                    } else {
                        call = self.find_fn("panic", vec![], None, &mut vec![], span)?;
                    }
                    un.op.str += &call;
                    Ok(*ty)
                } else if let Type::Option(ty) = ty {
                    let call = self.find_fn("panic", vec![], None, &mut vec![], span)?;
                    un.op.str += &call;
                    Ok(*ty)
                } else {
                    Err(SemanticError::UnaryTypeMismatch(span, "!".to_string(), ty))
                }
            }
            _ => Err(SemanticError::InvalidUnaryOperator(un.op.clone())),
        }
    }

    fn else_scope(&mut self, r#else: &mut node::ElseScope) -> Result<(), SemanticError> {
        self.unpack(&mut r#else.unpack)?;
        if let Some(c) = &r#else.capture {
            let Type::Result(_, err) = &r#else.unpack.expr.ty else {
                return Err(SemanticError::TypeMismatch(
                    r#else.pos_str.clone(),
                    r#else.unpack.expr.ty.clone(),
                    Type::Result(Box::new(Type::Any), Box::new(Type::Any)),
                ));
            };
            if self.symbol_table.contains_key(&c.str) {
                return Err(SemanticError::SymbolExists(c.clone()));
            }
            self.push_symbol(c.str.clone(), Symbol::Var { ty: *err.clone() });
        }
        self.scope(&mut r#else.scope)?;
        if let Some(c) = &r#else.capture {
            self.symbol_table.remove(&c.str);
        }
        Ok(())
    }

    fn else_expr(&mut self, r#else: &mut node::ElseExpr) -> Result<(), SemanticError> {
        let opt_ty = self.unpack(&mut r#else.unpack)?;
        let Some(ty) = opt_ty else {
            return Err(SemanticError::NoCapture(r#else.else_expr.span));
        };
        let ty2 = self.expr(&mut r#else.else_expr)?;
        if ty != ty2 {
            Self::type_cast(ty2, ty.clone(), r#else.else_expr.span)?;
            r#else.else_expr.kind = node::ExprKind::TypeCast(node::TypeCast {
                r#type: Self::node_type(r#else.else_expr.span),
                expr: r#else.else_expr.clone(),
            })
        }
        Ok(())
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
            return Err(SemanticError::EmptyArray(Span { start: pos_id - 1, end: pos_id }));
        }
        return Ok((cur_type, exprs.len()));
    }

    fn find_fn(
        &mut self,
        func: &str,
        mut expected_sig: Vec<Type>,
        expected_ret: Option<Type>,
        args: &mut Vec<node::Expr>,
        span: Span,
    ) -> Result<String, SemanticError> {
        let mut s = func.to_string();
        let mut call = func.to_string();
        let pos_str = PosStr {
            str: func.to_string(),
            pos_id: span.end,
        };
        if let Some(sigs) = self.fn_map.get(func) {
            let mut found = false;
            for sig in sigs.iter() {
                if *sig.0 == expected_sig {
                    s = format!("{}.{}", s, sig.1);
                    call = s.clone();
                    found = true;
                    break;
                }
            }

            if !found {
                let mut generics = HashMap::new();
                let mut id = -1;
                'sig_loop: for sig in sigs {
                    let tys = &sig.0;
                    if expected_sig.len() != tys.len() {
                        continue;
                    }

                    // --- STAGE 1: Infer ALL possible generics from ALL arguments via structural matching ---
                    let mut inferred_generics = HashMap::new();
                    for (arg_type, param_type) in expected_sig.iter().zip(tys.iter()) {
                        // This is a "soft" match. We ignore the result because failure to match
                        // one argument shouldn't prevent us from gathering info from others.
                        let _ = param_type.match_generics(arg_type, &mut inferred_generics);
                    }

                    // --- STAGE 2: Verify and plan casts using the inferred generics ---
                    let mut final_arg_types = expected_sig.clone();
                    let mut cast_info = Vec::new();

                    for (i, (arg_type, param_type)) in final_arg_types.iter().zip(tys.iter()).enumerate() {
                        let substituted_param_ty = param_type.substitute_generics(&inferred_generics);

                        if *arg_type == substituted_param_ty {
                            // Perfect match, nothing to do for this argument.
                            continue;
                        }

                        if Self::type_cast(arg_type.clone(), substituted_param_ty.clone(), span).is_ok() {
                            // A cast is possible. Plan to insert a cast node.
                            cast_info.push((i, substituted_param_ty));
                        } else {
                            // No direct match and no cast possible. This signature fails.
                            // Move on to the next potential signature.
                            continue 'sig_loop;
                        }
                    }

                    // --- STAGE 3: Success! This signature is a match. Apply changes. ---
                    id = sig.1 as i64;
                    generics = inferred_generics;

                    // Apply the planned casts to the actual AST `args`.
                    for (i, target_ty) in cast_info {
                        let e = args.get(i).unwrap().clone();
                        args.get_mut(i).unwrap().kind = ExprKind::TypeCast(node::TypeCast {
                            r#type: Self::node_type(e.span),
                            expr: Box::new(e),
                        });
                        args.get_mut(i).unwrap().ty = target_ty.clone();
                        // Also update the `expected_sig` we're working with.
                        expected_sig[i] = target_ty;
                    }

                    found = true;
                    break 'sig_loop;
                }

                if id == -1 {
                    return Err(SemanticError::NoFnSig(func.to_string(), span, expected_sig, expected_ret));
                }
                s = format!("{}.{}", s, id);
                if !generics.is_empty() {
                    let mut found = false;
                    for (res_name, res_map, res_id) in self.resolving.iter() {
                        if *res_name == s && generics == *res_map {
                            call = format!("{}.{}", s, res_id);
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        for (name, map, i) in self.gcalls.iter() {
                            if *name == s && generics == *map {
                                call = format!("{}.{}", s, i);
                                found = true;
                                break;
                            }
                        }
                    }
                    if !found {
                        for (name, id) in self.gfns.iter_mut() {
                            if *name == s {
                                self.gcalls.push((s.clone(), generics, *id));
                                call = format!("{}.{}", s, id);
                                *id += 1;
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            panic!("{s} {:?}", self.gfns)
                        }
                    }
                } else {
                    call = s.clone();
                }
            }
        }
        let ty;
        match self.symbol_table.get(&s) {
            Some(Symbol::Func { ty: t, .. }) | Some(Symbol::ExternFunc { ty: t, .. }) | Some(Symbol::Syscall { ty: t, .. }) => {
                ty = t;
            }
            _ => {
                if s.contains(".") {
                    return Err(SemanticError::PrivateAccess(pos_str));
                }
                return Err(SemanticError::UndeclaredSymbol(pos_str));
            }
        }
        let mut generics = HashMap::new();
        let sigs = self.fn_map.get(func).unwrap_or_else(|| panic!("{} {:?}", func, self.fn_map.keys()));
        let sig = sigs.iter().find(|(_, id)| s.ends_with(&format!(".{}", id))).unwrap();
        for (arg_ty, param_ty) in expected_sig.iter().zip(sig.0.iter()) {
            param_ty.match_generics(arg_ty, &mut generics).ok();
        }
        let res = ty.substitute_generics(&generics);
        if let Some(ret) = &expected_ret {
            if res != *ret {
                return Err(SemanticError::NoFnSig(func.to_string(), span, expected_sig, expected_ret));
            }
        }
        if self.symbol_table.get(&call).is_none() {
            self.symbol_table.insert(
                call.clone(),
                Symbol::ExternFunc {
                    ty: res,
                    args: args.iter().map(|a| a.ty.clone()).collect(),
                },
            );
        }
        Ok(call)
    }

    fn call(&mut self, call: &mut node::Call, span: Span) -> Result<Type, SemanticError> {
        if call.name.str == "main" {
            return Err(SemanticError::MainFnCall(call.name.clone()));
        }
        let mut arg_types = Vec::new();
        let mut arg_spans = Vec::new();
        for s in call.args.iter_mut() {
            arg_types.push(self.expr(s)?);
            arg_spans.push(s.span);
        }
        let name = self.find_fn(&call.name.str, arg_types, None, &mut call.args, span)?;
        call.name.str = name;
        let ty;
        match self.symbol_table.get(&call.name.str) {
            Some(Symbol::Func { ty: t, .. }) | Some(Symbol::ExternFunc { ty: t, .. }) | Some(Symbol::Syscall { ty: t, .. }) => {
                ty = t.clone();
            }
            _ => unreachable!(),
        }
        Ok(ty)
    }

    fn built_in(&mut self, built_in: &mut node::BuiltIn, span: Span) -> Result<Type, SemanticError> {
        for expr in built_in.args.iter_mut() {
            self.expr(expr)?;
        }
        for node_ty in built_in.type_args.iter() {
            built_in.tys.push(self.r#type(node_ty, false)?)
        }
        match built_in.kind {
            node::BuiltInKind::Len => {
                if built_in.type_args.len() != 0 {
                    return Err(SemanticError::InvalidTypeArgCount(span, built_in.type_args.len(), 0));
                }
                if built_in.args.len() != 1 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 1));
                }
                let mut ty = built_in.args.get(0).unwrap().ty.clone();
                if let Type::Pointer(p) = &ty {
                    ty = *p.clone();
                }
                match ty {
                    Type::Array { .. } | Type::Slice { .. } | Type::List { .. } => Ok(()),
                    _ => Err(SemanticError::NoBuiltIn(span, "length".to_string(), ty)),
                }?;
                Ok(Type::Int)
            }
            node::BuiltInKind::Copy => {
                if built_in.type_args.len() != 0 {
                    return Err(SemanticError::InvalidTypeArgCount(span, built_in.type_args.len(), 0));
                }
                let [a, b, c] = &mut built_in.args[..] else {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 3));
                };
                let Type::Pointer(_) = a.ty else {
                    return Err(SemanticError::ArgTypeMismatch(a.span, Type::Pointer(Box::new(Type::Any)), a.ty.clone()));
                };
                let Type::Pointer(_) = b.ty else {
                    return Err(SemanticError::ArgTypeMismatch(b.span, Type::Pointer(Box::new(Type::Any)), b.ty.clone()));
                };
                let Type::Int = c.ty else {
                    return Err(SemanticError::ArgTypeMismatch(a.span, Type::Int, c.ty.clone()));
                };
                Ok(Type::Void)
            }
            node::BuiltInKind::StackPointer => {
                if built_in.type_args.len() != 0 {
                    return Err(SemanticError::InvalidTypeArgCount(span, built_in.type_args.len(), 0));
                }
                if built_in.args.len() != 0 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 0));
                }
                Ok(Type::Pointer(Box::new(Type::Any)))
            }
            node::BuiltInKind::Sizeof => {
                if built_in.type_args.len() != 1 {
                    return Err(SemanticError::InvalidTypeArgCount(span, built_in.type_args.len(), 1));
                }
                if built_in.args.len() != 0 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 0));
                }
                Ok(Type::Int)
            }
            node::BuiltInKind::Param => {
                if built_in.type_args.len() != 0 {
                    return Err(SemanticError::InvalidTypeArgCount(span, built_in.type_args.len(), 0));
                }
                if built_in.args.len() != 1 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 1));
                }
                Ok(Type::Void)
            }
            node::BuiltInKind::IsType => {
                if built_in.type_args.len() != 1 {
                    return Err(SemanticError::InvalidTypeArgCount(span, built_in.type_args.len(), 1));
                }
                if built_in.args.len() != 1 {
                    return Err(SemanticError::InvalidArgCount(span, built_in.args.len(), 1));
                }
                Ok(Type::Bool)
            }
        }
    }

    fn r#type(&self, r#type: &node::Type, indirection: bool) -> Result<Type, SemanticError> {
        match &r#type.kind {
            node::TypeKind::Word(word) => match word.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "bool" => Ok(Type::Bool),
                "char" => Ok(Type::Char),
                "range" => Ok(Type::Range),
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
                    if self.cur_generics.contains(&other.to_string()) {
                        return Ok(Type::Generic(other.to_string()));
                    }
                    Err(SemanticError::InvalidType(r#type.span))
                }
            },
            node::TypeKind::Pointer(inner) => Ok(Type::Pointer(Box::new(self.r#type(&inner, true)?))),
            node::TypeKind::Array(inner, len) => Ok(Type::Array {
                inner: Box::new(self.r#type(&inner, false)?),
                length: *len as usize,
            }),
            node::TypeKind::Slice(inner) => Ok(Type::Slice {
                inner: Box::new(self.r#type(inner, true)?),
            }),
            node::TypeKind::List(inner) => Ok(Type::List {
                inner: Box::new(self.r#type(inner, true)?),
            }),
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
                    if let Type::Generic(_) = ty {
                        map.insert(node_name.str.clone(), (ty, i));
                        i += 1;
                    } else {
                        let add = ty.size();
                        map.insert(node_name.str.clone(), (ty, i));
                        i += add;
                    }
                }
                Ok(Type::Struct(map))
            }
            node::TypeKind::Result(ty, err) => Ok(Type::Result(Box::new(self.r#type(ty, false)?), Box::new(self.r#type(err, false)?))),
            node::TypeKind::Option(ty) => Ok(Type::Option(Box::new(self.r#type(ty, false)?))),
            node::TypeKind::Map(k, v) => Ok(Type::HashMap {
                key: Box::new(self.r#type(k, false)?),
                value: Box::new(self.r#type(v, false)?),
            }),
        }
    }

    fn type_cast(from: Type, to: Type, span: Span) -> Result<Type, SemanticError> {
        match (&from, &to) {
            (Type::List { inner }, Type::Pointer(i)) | (Type::List { inner }, Type::Slice { inner: i }) | (Type::Array { inner, .. }, Type::Slice { inner: i }) if inner == i => (),
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
            (Type::Tuple(tys), Type::Slice { inner }) | (Type::Tuple(tys), Type::List { inner }) => {
                let mut ok = false;
                if let Some(Type::Int) = tys.get(0) {
                    if let Some(Type::Pointer(p)) = tys.get(1) {
                        if p == inner || **p == Type::Any {
                            ok = true;
                        }
                    }
                }
                if !ok {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            // (Type::Struct(s), other) => {
            //     let first = s.iter().find(|(_, (_, offset))| *offset == 0);
            //     let mut ok = false;
            //     if let Some((_, (ty, _))) = first {
            //         if ty == other {
            //             ok = true;
            //         }
            //     }
            //     if !ok {
            //         return Err(SemanticError::InvalidCast(span, from, to));
            //     }
            // }
            // (Type::Tuple(tys), other) => {
            //     let first = tys.get(0);
            //     let mut ok = false;
            //     if let Some(ty) = first {
            //         if ty == other {
            //             ok = true;
            //         }
            //     }
            //     if !ok {
            //         return Err(SemanticError::InvalidCast(span, from, to));
            //     }
            // }
            (Type::Pointer(_), Type::Pointer(_)) => (),
            (Type::Slice { inner }, Type::Pointer(p)) => {
                if inner != p {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Pointer(_), Type::Int) => (),
            (ty, Type::Result(ok, _)) => {
                if *ty != **ok {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Pointer(p), Type::HashMap { .. }) => {
                if let Type::HashMap { .. } = **p {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::HashMap { .. }, Type::Pointer(p)) => {
                if let Type::HashMap { .. } = **p {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            (Type::Option(_), Type::Option(_)) => (),
            (ty, Type::Option(opt)) => {
                if *ty != **opt {
                    return Err(SemanticError::InvalidCast(span, from, to));
                }
            }
            _ => return Err(SemanticError::InvalidCast(span, from, to)),
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
            node::LetOrExpr::Expr(expr) => self.expr(expr).map(|_| ())?,
        }
        self.expr(&mut r#for.cond)?;
        self.expr(&mut r#for.incr)?;
        self.loop_count += 1;
        self.scope(&mut r#for.scope)?;
        self.loop_count -= 1;
        Ok(())
    }

    fn for_in(&mut self, r#for: &mut node::ForIn) -> Result<(), SemanticError> {
        let ty = self.expr(&mut r#for.expr)?;
        let capture_ty = match ty {
            Type::Slice { inner } => *inner,
            Type::Range => Type::Int,
            Type::Struct(map) => {
                self.for_in_struct(r#for, map, false)?;
                return Ok(());
            }
            Type::Pointer(inner) => {
                if let Type::Struct(map) = *inner {
                    self.for_in_struct(r#for, map, true)?;
                    return Ok(());
                } else {
                    return Err(SemanticError::NotIterable(r#for.expr.span, Type::Pointer(inner)));
                }
            }
            Type::Tuple(tys) => {
                let mut scope = vec![];
                for ty in tys {
                    self.symbol_table.insert(r#for.capture.str.clone(), Symbol::Var { ty });
                    self.loop_count += 1;
                    let mut new_scope = r#for.scope.clone();
                    self.scope(&mut new_scope)?;
                    scope.extend(new_scope);
                    scope.push(node::Stmt::Marker);
                    self.loop_count -= 1;
                    self.symbol_table.remove(&r#for.capture.str);
                }
                r#for.scope = scope;
                return Ok(());
            }
            other => {
                let inner = other.inner(1).clone();
                let expected = Type::Slice { inner: Box::new(inner.clone()) };
                if Self::type_cast(other.clone(), expected.clone(), r#for.expr.span).is_ok() {
                    r#for.expr.kind = node::ExprKind::TypeCast(node::TypeCast {
                        r#type: Self::node_type(r#for.expr.span),
                        expr: Box::new(r#for.expr.clone()),
                    });
                    r#for.expr.ty = expected.clone();
                    inner
                } else {
                    return Err(SemanticError::NotIterable(r#for.expr.span, other));
                }
            }
        };
        self.symbol_table.insert(r#for.capture.str.clone(), Symbol::Var { ty: capture_ty });
        self.loop_count += 1;
        self.scope(&mut r#for.scope)?;
        self.loop_count -= 1;
        self.symbol_table.remove(&r#for.capture.str);
        Ok(())
    }

    fn for_in_struct(&mut self, r#for: &mut node::ForIn, map: HashMap<String, (Type, u32)>, pointer_mode: bool) -> Result<(), SemanticError> {
        let mut scope = vec![];
        let mut items: Vec<_> = map.iter().collect();
        items.sort_by_key(|(_, (_, offset))| *offset);

        for (_, (ty, _)) in items {
            let value_ty = if pointer_mode { Type::Pointer(Box::new(ty.clone())) } else { ty.clone() };

            self.symbol_table.insert(
                r#for.capture.str.clone(),
                Symbol::Var {
                    ty: Type::Struct(HashMap::from([
                        ("name".to_string(), (Type::Slice { inner: Box::new(Type::Char) }, 0)),
                        ("value".to_string(), (value_ty, 16)),
                    ])),
                },
            );

            self.loop_count += 1;
            let mut new_scope = r#for.scope.clone();
            self.scope(&mut new_scope)?;
            scope.extend(new_scope);
            scope.push(node::Stmt::Marker);
            self.loop_count -= 1;
            self.symbol_table.remove(&r#for.capture.str);
        }

        r#for.scope = scope;
        Ok(())
    }

    fn r#extern(&mut self, ext: &mut node::Extern) -> Result<(), SemanticError> {
        if self.cur_func.is_some() {
            return Err(SemanticError::FuncInFunc(ext.name.clone()));
        }
        let ty;
        if let Some(t) = &ext.decl_type {
            ty = self.r#type(t, false)?;
        } else {
            ty = Type::Void;
        }
        let mut args = Vec::new();
        for arg in ext.types.iter() {
            let ty = self.r#type(arg, false)?;
            args.push(ty.clone());
        }
        self.symbol_table.insert(ext.name.str.clone(), Symbol::ExternFunc { ty, args });
        Ok(())
    }

    fn r#match(&mut self, m: &mut node::Match) -> Result<(), SemanticError> {
        let ty = self.expr(&mut m.match_expr)?;
        for (e, scope) in m.branches.iter_mut() {
            if let ExprKind::Variable(var) = &e.kind {
                self.symbol_table.insert(var.str.clone(), Symbol::Var { ty: ty.clone() });
            } else {
                let match_ty = self.expr(e)?;
                if ty != match_ty {
                    return Err(SemanticError::UnexpectedType(e.span, ty, match_ty));
                }
            }
            self.scope(scope)?;
            if let ExprKind::Variable(var) = &e.kind {
                self.symbol_table.remove(&var.str);
            }
        }
        Ok(())
    }

    fn match_type(&mut self, m: &mut node::MatchType) -> Result<(), SemanticError> {
        let ty = self.r#type(&m.match_type, false)?;
        let mut generics = HashMap::new();
        self.cur_generics.extend(m.generics.clone());
        for (t, scope, active) in m.branches.iter_mut() {
            generics.clear();
            if let node::TypeKind::Word(w) = &t.kind {
                if (w == "struct" && matches!(ty, Type::Struct(_))) || (w == "array" && matches!(ty, Type::Array { .. })) || (w == "tuple" && matches!(ty, Type::Tuple(_))) {
                    *active = true;
                    self.scope(scope)?;
                    break;
                }
                if ["struct", "array", "tuple"].contains(&w.as_str()) {
                    continue;
                }
            }
            let match_ty = self.r#type(t, false)?;
            if ty == match_ty {
                *active = true;
                self.scope(scope)?;
                break;
            } else if match_ty.match_generics(&ty, &mut generics).is_ok() {
                for (k, v) in &generics {
                    self.symbol_table.insert(k.to_string(), Symbol::Type { ty: v.clone() });
                }
                *active = true;
                self.scope(scope)?;
                for k in generics.keys() {
                    self.symbol_table.remove(k);
                }
                break;
            }
        }
        if !m.branches.iter().any(|(_, _, active)| *active) {
            return Err(SemanticError::NoMatch(m.pos_id, ty));
        }
        self.cur_generics.truncate(self.cur_generics.len() - m.generics.len());
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
