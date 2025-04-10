use crate::types;
use crate::helpers::StringLit;

#[derive(Debug, Clone)]
pub struct PosStr {
    pub str: String,
    pub pos_id: usize,
}
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize
}
impl Span {
    pub fn add(&self, other: Span) -> Span {
        Span { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}
#[derive(Debug)]
pub struct Let {
    pub name: PosStr,
    pub expr: Expr,
}
#[derive(Debug)]
pub struct Decl {
    pub name: PosStr,
    pub r#type: Type,
    pub ty: types::Type
}
#[derive(Debug)]
pub struct StructDecl {
    pub name: PosStr,
    pub field_names: Vec<PosStr>,
    pub field_types: Vec<Type>,
}
#[derive(Debug, Clone)]
pub struct StructLit {
    pub name: PosStr,
    pub field_names: Vec<PosStr>,
    pub field_exprs: Vec<Expr>,
}
#[derive(Debug, Clone)]
pub struct Call {
    pub name: PosStr,
    pub args: Vec<Expr>,
}
#[derive(Debug, Clone)]
pub enum TypeKind {
    Word(String),
    Pointer(Box<Type>),
    Array(Box<Type>, Option<i64>),
    Tuple(Vec<Type>),
}
#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span
}
#[derive(Debug)]
pub struct Fn {
    pub name: PosStr,
    pub arg_names: Vec<PosStr>,
    pub arg_types: Vec<Type>,
    pub decl_type: Option<Type>,
    pub scope: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct BinExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: PosStr,
}
impl BinExpr {
    pub fn is_assign(&self) -> bool {
        return ["=", "+=", "-=", "*=", "/=", "%=", "|=", "^=", "&=", ">>=", "<<="].contains(&self.op.str.as_str());
    }
}
#[derive(Debug, Clone)]
pub struct UnExpr {
    pub expr: Box<Expr>,
    pub op: PosStr,
}
#[derive(Debug)]
pub struct Ret {
    pub pos_id: usize,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub pos_id: usize,
    pub expr: Option<Expr>,
    pub scope: Vec<Stmt>,
    pub els: Option<Box<If>>
}
#[derive(Debug)]
pub enum LetOrExpr {
    Let(Let),
    Expr(Expr)
}
#[derive(Debug)]
pub struct Loop {
    pub pos_id: usize,
    pub scope: Vec<Stmt>,
}
#[derive(Debug)]
pub struct While {
    pub pos_id: usize,
    pub expr: Expr,
    pub scope: Vec<Stmt>,
}
#[derive(Debug)]
pub struct For {
    pub pos_id: usize,
    pub init: LetOrExpr,
    pub cond: Expr,
    pub incr: Expr,
    pub scope: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ArrLit {
    pub exprs: Vec<Expr>,
    pub pos_id: usize
}

#[derive(Debug, Clone)]
pub enum Macro {
    Salloc { count: u32, ty: Type }
}
#[derive(Debug)]
pub struct Syscall {
    pub id: i64,
    pub name: PosStr,
    pub types: Vec<Type>,
    pub decl_type: Option<Type>,
}
#[derive(Debug, Clone)]
pub struct TypeCast {
    pub ty: Type,
    pub expr: Box<Expr>
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: types::Type,
    pub span: Span
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    IntLit(i64),
    ArrLit(ArrLit),
    StructLit(StructLit),
    StringLit(StringLit),
    TupleLit(Vec<Expr>),
    Variable(PosStr),
    Call(Call),
    Macro(Macro),
    BinExpr(BinExpr),
    UnExpr(UnExpr),
    TypeCast(TypeCast)
}
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Let(Let),
    Decl(Decl),
    Fn(Fn),
    StructDecl(StructDecl),
    Ret(Ret),
    If(If),
    Loop(Loop),
    While(While),
    For(For),
    Break(usize),
    Continue(usize),
    Syscall(Syscall)
}
#[derive(Debug)]
pub struct Module {
    pub path: String,
    pub stmts: Vec<Stmt>
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
    pub parent: Option<String>,
    pub pos_id: usize
}

#[derive(Debug)]
pub struct Imports(pub Vec<String>);

#[derive(Debug)]
pub struct Root(pub Vec<Module>);

impl Root {
    pub fn iter(&self) -> impl Iterator<Item = (&str, &Stmt)> {
        self.0.iter().flat_map(|module| {
            let path = module.path.as_str();
            module.stmts.iter().map(move |stmt| (path, stmt))
        })
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut Stmt)> {
        self.0.iter_mut().flat_map(|module| {
            let path = module.path.as_str();
            module.stmts.iter_mut().map(move |stmt| (path, stmt))
        })
    }
}

impl IntoIterator for Root {
    type Item = Stmt;
    type IntoIter = std::vec::IntoIter<Stmt>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().flat_map(|module| module.stmts.into_iter()).collect::<Vec<_>>().into_iter()
    }
}
