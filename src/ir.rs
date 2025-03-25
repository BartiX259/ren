use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Symbol {
    Struct { attrs: HashMap<String, Type> },
    Var { ty: Type },
    Func { ty: Type, block: Block, symbols: Vec<Vec<(String, Symbol)>>, macros: Vec<Macro> },
    ExternFunc { ty: Type, args: Vec<Type> },
}

#[derive(Debug, Clone)]
pub enum Macro {
    Salloc { size: i64, ty: Type }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Bool,
    String,
    Pointer(Box<Type>),
    Array(Box<Type>),
    Type,
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Term {
    Temp(usize),
    IntLit(String),
    Symbol(String),
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
    Tac { lhs: Term, rhs: Option<Term>, op: Option<String>, res: Option<Term> },
    Unary { term: Term, op: String, res: Term },
    DerefAssign { term: Term, op: String, ptr: Term, res: Option<Term> },
    Arg(Term),
    Param(Term),
    Call { func: String, res: Term },
    Macro { id: usize, res: Option<Term> },
    Label(u16),
    Jump(u16),
    CondJump { cond: Term, label: u16 },
    BinJump { lhs: Term, rhs: Term, op: String, label: u16 },
    Return(Term),
    LoadSymbols(usize),
    UnloadSymbols(usize),
    NaturalFlow,
    LoopStart,
    LoopEnd,
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
            Term::IntLit(value) => write!(f, "{}", value),
            Term::Symbol(name) => write!(f, "{}", name),
        }
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Tac { lhs, rhs, op, res } => {
                if let Some(r) = res {
                    write!(f, "{:?} = ", r)?;
                }
                write!(f, "{:?}", lhs)?;
                if let Some(o) = op {
                    write!(f, " {} ", o)?;
                }
                if let Some(r) = rhs {
                    write!(f, "{:?}", r)?;
                }
                Ok(())
            }
            Op::Unary { term, op, res } => write!(f, "{:?} = {}{:?}", res, op, term),
            Op::DerefAssign { term, op, ptr, res } => {
                if let Some(r) = res {
                    write!(f, "{:?} = *{:?} {} {:?}", r, ptr, op, term)
                } else {
                    write!(f, "*{:?} {} {:?}", ptr, op, term)
                }
            }
            Op::Arg(term) => write!(f, "arg {:?}", term),
            Op::Param(term) => write!(f, "param {:?}", term),
            Op::Call { func, res } => write!(f, "{:?} = call {}", res, func),
            Op::Macro { id, res} => {
                if let Some(r) = res {
                    write!(f, "{:?} = macro {}", r, id)
                }else {
                    write!(f, "macro {}", id)
                }
            }
            Op::Label(label) => write!(f, "L{}:", label),
            Op::Jump(label) => write!(f, "jump L{}", label),
            Op::CondJump { cond, label } => write!(f, "if {:?} jump L{}", cond, label),
            Op::BinJump { lhs, rhs, op, label } => write!(f, "if {:?} {} {:?} jump L{}", lhs, op, rhs, label),
            Op::Return(value) => write!(f, "return {:?}", value),
            Op::LoadSymbols(i) => write!(f, "load symbols {}", i),
            Op::UnloadSymbols(i) => write!(f, "unload symbols {}", i),
            Op::NaturalFlow => write!(f, "natural flow"),
            Op::LoopStart => write!(f, "loop start"),
            Op::LoopEnd => write!(f, "loop end"),
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
