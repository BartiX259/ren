#[derive(Debug, Clone)]
pub struct PosStr {
    pub str: String,
    pub pos_id: usize,
}
#[derive(Debug)]
pub struct Let {
    pub name: PosStr,
    pub expr: Expr,
}
#[derive(Debug, Clone)]
pub struct Call {
    pub name: PosStr,
    pub args: Vec<Expr>,
}
#[derive(Debug, Clone)]
pub struct Type {
    pub str: PosStr,
    pub sub: Option<Box<Type>>
}
#[derive(Debug)]
pub struct Fn {
    pub name: PosStr,
    pub arg_names: Vec<PosStr>,
    pub arg_types: Vec<Type>,
    pub scope: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct BinExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: PosStr,
}
#[derive(Debug, Clone)]
pub struct UnExpr {
    pub expr: Box<Expr>,
    pub op: PosStr,
}
#[derive(Debug)]
pub struct Ret {
    pub pos_id: usize,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct If {
    pub pos_id: usize,
    pub expr: Option<Expr>,
    pub scope: Vec<Stmt>,
    pub els: Option<Box<If>>
}

#[derive(Debug)]
pub struct Loop {
    pub pos_id: usize,
    pub scope: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Macro {
    Salloc { count: i64, ty: Type }
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLit(PosStr),
    Variable(PosStr),
    Call(Call),
    Macro(Macro),
    BinExpr(BinExpr),
    UnExpr(UnExpr),
}
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Let(Let),
    Fn(Fn),
    Ret(Ret),
    If(If),
    Loop(Loop),
    Break(usize),
    Continue(usize),
}
