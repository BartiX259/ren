use std::fmt;
use crate::types::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    Struct { ty: Type },
    Var { ty: Type },
    Func { ty: Type, block: Block, symbols: Vec<Vec<(String, Symbol)>> },
    ExternFunc { ty: Type, args: Vec<Type> },
    Syscall { id: i64, ty: Type, args: Vec<Type> },
    StringLit { str: String }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Term {
    /// Term contained in a single register
    Temp(usize),
    /// Term contained in two registers
    Double(usize),
    /// Term living on the stack
    Stack(usize),
    /// Pointer living on the stack
    Pointer(usize),
    /// Data from symbol table
    Data(String),
    /// Simple integer
    IntLit(i64),
}

#[derive(Debug, Clone)]
pub struct OpLoc {
    pub start_id: usize,
    pub end_id: usize,
}

#[derive(Clone)]
pub struct Block {
    pub ops: Vec<Op>,
    pub locs: Vec<OpLoc>,
}

#[derive(Clone)]
pub enum Op {
    BinOp { res: Option<Term>, lhs: Term, op: String, rhs: Term },
    UnOp { res: Term, op: String, term: Term },
    Store { res: Option<Term>, ptr: Term, offset: i64, op: String, term: Term },
    Read { res: Term, ptr: Term, offset: i64 },
    Copy { from: Term, to: Term, size: u32 },
    Let { res: Term, term: Term },
    Decl { term: Term, size: u32 },
    Arg { term: Term, double: bool },
    Param { term: Term },
    Call { res: Option<Term>, func: String },
    BeginCall,
    EndCall,
    Label { label: u16 },
    Jump { label: u16 },
    CondJump { label: u16, cond: Term },
    BinJump { label: u16, lhs: Term, op: String, rhs: Term },
    Return { term: Option<Term> },
    NaturalFlow,
    Save,
    Restore,
}

impl Block {
    pub fn new() -> Self {
        Self { ops: Vec::new(), locs: Vec::new() }
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Temp(id) => write!(f, "t{}", id),
            Term::Double(id) => write!(f, "d{}", id),
            Term::Stack(id) => write!(f, "s{}", id),
            Term::Pointer(id) => write!(f, "p{}", id),
            Term::Data(s) => write!(f, "{}", s),
            Term::IntLit(value) => write!(f, "{}", value),
        }
    }
}
impl Term {
    pub fn is_stack(&self) -> bool {
        match self {
            Term::Pointer(_) | Term::Stack(_) => true,
            _ => false
        }
    }
}

fn fmt_opt(res: &Option<Term>) -> String {
    res.clone().map(|r| format!("{:?} = ", r)).unwrap_or("".to_string())
}
fn fmt_ptr(ptr: &Term, offset: &i64) -> String {
    let o = if *offset == 0 { String::new() }
    else if *offset > 0 { format!(" + {}", offset) }
    else { format!(" - {}", -offset)};
    let s = if let Term::Stack(_) = ptr { "&" } else { "" };
    format!("{}{:?}{}", s, ptr, o)
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::BinOp { res, lhs, op, rhs } => write!(f, "{}{:?} {} {:?}", fmt_opt(res), lhs, op, rhs),
            Op::UnOp { res, op, term } => write!(f, "{:?} = {}{:?}", res, op, term),
            Op::Store { res, ptr, offset, op: _, term } => write!(f, "{}*({}) = {:?}", fmt_opt(res), fmt_ptr(ptr, offset), term),
            Op::Read { res, ptr, offset } => write!(f, "{:?} = *({})", res, fmt_ptr(ptr, offset)),
            Op::Copy { from, to, size } => write!(f, "copy {:?} into {:?} (size {})", from, to, size),
            Op::Let { res, term } => write!(f, "let {:?} = {:?}", res, term),
            Op::Decl { term, size } => write!(f, "decl {:?} (size {})", term, size),
            Op::Arg { term, double } => write!(f, "arg {:?}{}", term, if *double { " (double)" } else { "" }),
            Op::Param { term } => write!(f, "param {:?}", term),
            Op::BeginCall => write!(f, "begin call"),
            Op::EndCall => write!(f, "end call"),
            Op::Call { res, func } => write!(f, "{}call {:?}", fmt_opt(res), func),
            Op::Label { label } => write!(f, "L{}", label),
            Op::Jump { label } => write!(f, "jump L{}", label),
            Op::CondJump { label, cond } => write!(f, "if {:?} jump L{}", cond, label),
            Op::BinJump { label, lhs, op, rhs } => write!(f, "if {:?} {} {:?} jump L{}", lhs, op, rhs, label),
            Op::Return { term } => write!(f, "return {}", fmt_opt(term)),
            Op::NaturalFlow => write!(f, "→"),
            Op::Save => write!(f, "save"),
            Op::Restore => write!(f, "restore"),
        }
    }
}


impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "")?;
        writeln!(f, "StartBlock")?;
        //let mut locs = self.locs.iter();
        for op in &self.ops {
            writeln!(f, "  {:?}", op)?;
            //writeln!(f, "  {:?} :: pos {:?}", op, locs.next().unwrap())?;
        }
        writeln!(f, "EndBlock")?;
        Ok(())
    }
}
