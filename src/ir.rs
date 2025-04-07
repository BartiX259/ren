use std::fmt;
use crate::types::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    Struct { ty: Type },
    Var { ty: Type },
    Func { ty: Type, block: Block, symbols: Vec<Vec<(String, Symbol)>> },
    ExternFunc { ty: Type, args: Vec<Type> },
    StringLit { str: String }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Term {
    Temp(usize),
    IntLit(i64),
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
    DerefAssign { term: Term, op: String, ptr: Term, offset: i64, res: Option<Term>, stack: bool },
    DerefRead { ptr: Term, offset: i64, res: Term, stack: bool },
    Let {term: Term, res: Term },
    Decl {term: Term, size: u32 },
    Arg {term: Term, size: u32 },
    Param { term: Term, size: u32, stack_offset: Option<u32> },
    Call { func: String, res: Option<Term> },
    Label(u16),
    Jump(u16),
    CondJump { cond: Term, label: u16 },
    BinJump { lhs: Term, rhs: Term, op: String, label: u16 },
    Return(Term),
    ReturnNone,
    Salloc { size: u32, res: Term },
    TakeSalloc { ptr: Term, res: Term },
    ParamSalloc { ptr: Term, size: u32 },
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
fn opt_res(res: &Option<Term>) -> String {
    res.clone().map(|r| format!("{:?} = ", r)).unwrap_or("".to_string())
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
            Op::DerefAssign { term, op, ptr, offset, res, stack } =>  write!(f, "{}*({}{:?}+{}) {} {:?}",
            opt_res(res), if *stack { "&" } else { "" }, ptr, offset, op, term),
            Op::DerefRead { ptr, offset, res, stack } => write!(f, "{:?} = *({}{:?}+{})",
            res , if *stack { "&" } else { "" }, ptr, offset),
            Op::Let { term, res } => write!(f, "let {:?} = {:?}", res, term),
            Op::Decl { term, size } => write!(f, "decl {:?} (size {})", term, size),
            Op::Arg { term, size } => write!(f, "arg {:?} (size {})", term, size),
            Op::Param { term, size, stack_offset } => {
                if let Some(offset) = stack_offset {
                    write!(f, "stack param &{:?}+{} (size {})", term, offset, size)
                }else {
                    write!(f, "param {:?} (size {})", term, size)
                }
            }
            Op::Call { func, res } => write!(f, "{}call {}", opt_res(res), func),
            Op::Label(label) => write!(f, "L{}:", label),
            Op::Jump(label) => write!(f, "jump L{}", label),
            Op::CondJump { cond, label } => write!(f, "if {:?} jump L{}", cond, label),
            Op::BinJump { lhs, rhs, op, label } => write!(f, "if {:?} {} {:?} jump L{}", lhs, op, rhs, label),
            Op::Return(value) => write!(f, "return {:?}", value),
            Op::ReturnNone => write!(f, "return"),
            Op::Salloc { size, res } => write!(f, "{:?} = salloc {}", res, size),
            Op::TakeSalloc { ptr, res } => write!(f, "{:?} take salloc {:?}", res, ptr),
            Op::ParamSalloc { ptr, size } => write!(f, "param salloc {:?} (size {})", ptr, size),
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
