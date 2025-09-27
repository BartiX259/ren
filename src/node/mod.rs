use crate::types;
//mod display;

#[derive(Debug, Clone)]
pub struct PosStr {
    pub str: String,
    pub pos_id: usize,
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl Span {
    pub fn add(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub file_path: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub stmts: Vec<Stmt>,
    pub end: usize
}
#[derive(Debug, Clone)]
pub struct Let {
    pub capture: Capture,
    pub expr: Expr,
}
#[derive(Debug, Clone)]
pub struct Decl {
    pub name: PosStr,
    pub r#type: Type,
    pub ty: types::Type,
}
#[derive(Debug, Clone)]
pub struct Unpack {
    pub lhs: PosStr,
    pub rhs: Option<PosStr>,
    pub brackets: Option<PosStr>,
    pub expr: Expr,
}
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: PosStr,
    pub r#type: Type,
}
#[derive(Debug, Clone)]
pub struct Enum {
    pub name: PosStr,
    pub variants: Vec<(PosStr, Option<Type>)>,
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
    Array(Box<Type>, i64),
    Slice(Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Struct(Vec<PosStr>, Vec<Type>),
    Result(Box<Type>, Box<Type>),
    Option(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Fn(Vec<Type>, Option<Box<Type>>)
}
#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub name: PosStr,
    pub arg_types: Vec<Type>,
    pub decl_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: PosStr,
    pub arg_names: Vec<PosStr>,
    pub arg_types: Vec<Type>,
    pub decl_type: Option<Type>,
    pub generics: Vec<String>,
    pub scope: Scope,
}

#[derive(Debug, Clone)]
pub struct MainFn {
    pub pos_id: usize,
    pub arg_names: Vec<PosStr>,
    pub arg_types: Vec<Type>,
    pub decl_type: Option<Type>,
    pub scope: Scope,
}
#[derive(Debug, Clone, PartialEq)]
pub enum DecoratorKind {
    Pub,
}
#[derive(Debug, Clone)]
pub struct Decorator {
    pub kinds: Vec<DecoratorKind>,
    pub pos_ids: Vec<usize>,
    pub span: Span,
    pub inner: Box<Stmt>,
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
    pub fn is_bool(&self) -> bool {
        return ["&&", "||"].contains(&self.op.str.as_str());
    }
    pub fn is_cmp(&self) -> bool {
        return [">", ">=", "<", "<=", "==", "!="].contains(&self.op.str.as_str());
    }
}
#[derive(Debug, Clone)]
pub struct UnExpr {
    pub expr: Box<Expr>,
    pub op: PosStr,
}
#[derive(Debug, Clone)]
pub struct ElseScope {
    pub unpack: Unpack,
    pub capture: Option<PosStr>,
    pub pos_str: PosStr,
    pub scope: Scope,
}
#[derive(Debug, Clone)]
pub struct ElseExpr {
    pub unpack: Unpack,
    pub pos_str: PosStr,
    pub else_expr: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct Ret {
    pub pos_id: usize,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum IfKind {
    Expr(Expr),
    Unpack(Unpack),
    None,
}

#[derive(Debug, Clone)]
pub struct If {
    pub pos_id: usize,
    pub cond: IfKind,
    pub scope: Scope,
    pub els: Option<Box<If>>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub pos_id: usize,
    pub match_expr: Expr,
    pub branches: Vec<(Expr, Scope)>,
}
#[derive(Debug, Clone)]
pub struct MatchType {
    pub pos_id: usize,
    pub generics: Vec<String>,
    pub match_type: Type,
    pub branches: Vec<(Type, Scope, bool)>,
}
#[derive(Debug, Clone)]
pub struct Loop {
    pub pos_id: usize,
    pub scope: Scope,
}
#[derive(Debug, Clone)]
pub struct While {
    pub pos_id: usize,
    pub expr: Expr,
    pub scope: Scope,
}

#[derive(Debug, Clone)]
pub enum Capture {
    Single(PosStr),
    Multiple(Vec<PosStr>, bool)
}
#[derive(Debug, Clone)]
pub struct ForIn {
    pub pos_id: usize,
    pub capture: Capture,
    pub expr: Expr,
    pub scope: Scope,
}

#[derive(Debug, Clone)]
pub struct Extern {
    pub sig: FnSig
}

#[derive(Debug, Clone)]
pub struct ArrLit {
    pub exprs: Vec<Expr>,
    pub pos_id: usize,
}
#[derive(Debug, Clone)]
pub struct MapLit {
    pub keys: Vec<Expr>,
    pub values: Vec<Expr>,
    pub init_fn: String,
    pub pos_id: usize,
}
#[derive(Debug, Clone)]
pub struct StructLit {
    pub field_names: Vec<PosStr>,
    pub field_exprs: Vec<Expr>,
}
#[derive(Debug, Clone)]
pub enum StringFragment {
    Lit(crate::helpers::StringLit),
    Expr { expr: Expr, str_fn: String },
}

#[derive(Debug, Clone)]
pub struct Syscall {
    pub id: i64,
    pub sig: FnSig
}
#[derive(Debug, Clone)]
pub enum BuiltInKind {
    Len,
    Copy,
    StackPointer,
    Sizeof,
    Param,
    IsType,
}
#[derive(Debug, Clone)]
pub struct BuiltIn {
    pub kind: BuiltInKind,
    pub args: Vec<Expr>,
    pub type_args: Vec<Type>,
    pub tys: Vec<types::Type>,
}
#[derive(Debug, Clone)]
pub struct Ternary {
    pub cond: Box<Expr>,
    pub yes: Box<Expr>,
    pub no: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct TypeCast {
    pub r#type: Type,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: types::Type,
    pub span: Span,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    IntLit(i64),
    CharLit(u32),
    BoolLit(bool),
    Null,
    None,
    ArrLit(ArrLit),
    ListLit(ArrLit, String),
    StructLit(StructLit),
    StringLit(Vec<StringFragment>, String),
    TupleLit(Vec<Expr>),
    MapLit(MapLit),
    Variable(PosStr),
    Call(Call),
    BuiltIn(BuiltIn),
    BinExpr(BinExpr),
    UnExpr(UnExpr),
    PostUnExpr(UnExpr),
    TypeCast(TypeCast),
}
#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let(Let),
    Decl(Decl),
    LetElseExpr(ElseExpr),
    LetElseScope(ElseScope),
    Fn(Fn),
    MainFn(MainFn),
    TypeDecl(TypeDecl),
    Decorator(Decorator),
    Ret(Ret),
    If(If),
    Loop(Loop),
    While(While),
    ForIn(ForIn),
    Break(usize),
    Continue(usize),
    Match(Match),
    MatchType(MatchType),
    Syscall(Syscall),
    Enum(Enum),
    Extern(Extern),
    Marker,
}
#[derive(Debug)]
pub struct Module {
    pub path: String,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
    pub parent: Option<String>,
    pub pos_id: usize,
}

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
