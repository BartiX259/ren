use crate::helpers::IndentedBuf;
use crate::ir::{self, Block, OpLoc, Symbol, Term};
use std::collections::HashMap;

/// Generate nasm code based on the IR
pub fn gen(ir: &mut HashMap<String, Symbol>) -> Result<String, GenError> {
    let mut gen = Gen::new(ir);
    gen.all()?;
    Ok(gen.buf.get_output())
}

pub enum GenError {
    NoMainFn,
    NoFreeRegisters(OpLoc),
    ReservedSymbol(OpLoc),
    TooManyArguments(OpLoc),
    ExpectedLiteral(OpLoc),
}

#[derive(Debug, Clone, PartialEq)]
struct Reg {
    name: String,
    term: Option<Term>,
    locked: bool,
}
fn reg(name: &str) -> Reg {
    Reg {
        name: name.to_string(),
        term: None,
        locked: false,
    }
}
struct ProgState {
    regs: Vec<Reg>,
    param_index: usize,
    sp: i64
}

struct Gen<'a> {
    buf: IndentedBuf,
    symbol_table: &'a mut HashMap<String, Symbol>,
    fn_symbols: Vec<Vec<(String, Symbol)>>,
    locs: HashMap<Term, i64>,
    regs: Vec<Reg>,
    call_order: Vec<&'a str>,
    sp: i64,
    last_loc: OpLoc,
    reg_states: Vec<Vec<Reg>>,
    saved_states: Vec<ProgState>,
    param_index: usize,
    arg_index: usize,
    calling: bool,
}

impl<'a> Gen<'a> {
    pub fn new(ir: &'a mut HashMap<String, Symbol>) -> Self {
        Self {
            buf: IndentedBuf::new(8, 24),
            symbol_table: ir,
            fn_symbols: Vec::new(),
            locs: HashMap::new(),
            regs: vec![reg("rax"), reg("rbx"), reg("rcx"), reg("rdx"), reg("rdi"), reg("rsi")],
            call_order: vec!["rdi", "rsi", "rdx"],
            sp: 0,
            last_loc: OpLoc { start_id: 0, end_id: 0 },
            reg_states: Vec::new(),
            saved_states: Vec::new(),
            param_index: 0,
            arg_index: 0,
            calling: false,
        }
    }
    fn all(&mut self) -> Result<(), GenError> {
        // Data section
        self.buf.push_line("section .data");
        self.buf.indent();
        self.data();
        self.buf.dedent();
        self.buf.push_line("");
        // Text section
        self.buf.push_line("section .text");
        self.buf.indent();
        // Extern functions
        for (name, sym) in self.symbol_table.iter() {
            if let Symbol::ExternFunc { .. } = sym {
                self.buf.push_line(format!("extern {}", name));
            }
        }
        // _start function
        if let Some(_) = self.symbol_table.get("_start") {
            return Err(GenError::ReservedSymbol(OpLoc { start_id: 0, end_id: 0 }));
        }
        self.buf.push_line("global _start");
        self.buf.dedent();
        self.buf.push_line("");
        self.buf.push_line("_start:");
        self.buf.indent();
        let main = self.symbol_table.get("main");
        if let Some(Symbol::Func {
            ty: _,
            block: _,
            symbols: _,
        }) = main
        {
        } else {
            return Err(GenError::NoMainFn);
        }
        self.buf.push_line("call main");
        self.buf.push_line("mov rdi, rax");
        self.buf.push_line("mov rax, 60");
        self.buf.push_line("syscall");
        self.buf.dedent();
        // Other functions
        self.functions()?;
        Ok(())
    }

    fn data(&mut self) {
        for (name, sym) in self.symbol_table.iter() {
            match sym {
                Symbol::StringLit { str } => {
                    self.buf.push_line(format!("{} db {}", name, str));
                }
                _ => ()
            }
        }
    }

    fn functions(&mut self) -> Result<(), GenError> {
        let binding: Vec<String> = self.symbol_table.keys().cloned().collect();
        for name in binding {
            for r in self.regs.iter_mut() {
                r.term = None;
                r.locked = false;
            }
            self.reg_states = Vec::new();
            self.locs = HashMap::new();
            self.sp = 0;
            self.arg_index = 0;
            let sym = self.symbol_table.get(&name);
            if let Some(Symbol::Func { ty: _, block, symbols }) = sym {
                self.buf.push_line("");
                self.buf.push_line(format!("{}:", name));
                self.buf.indent();
                self.fn_symbols = symbols.clone();
                self.block(block.clone())?;
                if self.buf.last_line.clone() != "ret" {
                    if self.sp > 0 {
                        self.buf.push_line(format!("add rsp, {}", self.sp));
                    }
                    self.buf.push_line("mov rax, 0");
                    self.buf.push_line("ret");
                }
                self.buf.dedent();
            }
        }
        Ok(())
    }

    fn block(&mut self, block: Block) -> Result<(), GenError> {
        let mut locs = block.locs.iter();
        let mut iter = block.ops.into_iter().peekable();
        while let Some(op) = iter.next() {
            self.last_loc = locs.next().unwrap().clone();
            let op_clone = op.clone();
            match op {
                ir::Op::BinOp { lhs, rhs, op, res } => self.binop(lhs, rhs, op, res)?,
                ir::Op::UnOp { term, op, res } => self.unop(term, op, res)?,
                ir::Op::Store { term, op, ptr, offset, res } => self.store(term, op, ptr, offset, res)?,
                ir::Op::Read { ptr, offset, res } => self.read(ptr, offset, res)?,
                ir::Op::Copy { from, to, size } => {
                    if let Term::Stack(_) = from {
                        self.free_reg(&"rsi".to_string())?;
                        self.buf.push_line("mov rsi, rsp");
                        if let Some(x) = self.locs.get(&from) {
                            if *x != self.sp {
                                self.buf.push_line(format!("add rsi, {}", self.sp - x));
                            }
                        }
                    } else {
                        self.force_term_at(&from, &"rsi".to_string())?;
                    }
                    if let Term::Stack(_) = to {
                        self.free_reg(&"rdi".to_string())?;
                        self.buf.push_line("mov rdi, rsp");
                        if let Some(x) = self.locs.get(&to) {
                            if *x != self.sp {
                                self.buf.push_line(format!("add rdi, {}", self.sp - x));
                            }
                        }
                    } else {
                        self.force_term_at(&to, &"rdi".to_string())?;
                    }
                    self.force_term_at(&Term::IntLit((size >> 3) as i64), &"rcx".to_string())?;
                    self.buf.push_line("rep movsq");
                    let rsi = self.get_reg(&"rsi".to_string()).unwrap();
                    rsi.term = None;
                    rsi.locked = false;
                    let rdi = self.get_reg(&"rdi".to_string()).unwrap();
                    rdi.term = None;
                    rdi.locked = false;
                }
                ir::Op::Let { term, res } => {
                    let r = self.eval_term(term, true)?;
                    self.store_term(res, r);
                }
                ir::Op::Decl { term, size } => {
                    self.sp += size as i64;
                    self.buf.push_line(format!("sub rsp, {}", size));
                    self.locs.insert(term, self.sp);
                }
                ir::Op::Arg { term } => {
                    let reg = *self.call_order.get(self.arg_index).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                    self.arg_index += 1;
                    self.save_reg(&reg.to_string(), &term);
                    if term.is_stack() {
                        self.store_term(term, reg.to_string());
                        self.lock_reg(&reg.to_string(), false);
                    } else {
                        self.lock_reg(&reg.to_string(), true);
                    }
                }
                ir::Op::BeginCall => {
                    let mut saved = Vec::new();
                    for r in self.regs.iter_mut() { // Save registers
                        if r.locked {
                            saved.push(r.clone());
                            self.buf.push_line(format!("push {}", r.name));
                            self.buf.comment(format!("save {:?}", r.term.clone().unwrap()));
                            println!("save {:?} ({})",r.term.clone().unwrap(), r.name);
                            self.sp += 8;
                        }
                        r.term = None;
                        r.locked = false;
                    }
                    self.saved_states.push(ProgState { regs: saved, param_index: self.param_index, sp: self.sp });
                    self.param_index = 0;
                }
                ir::Op::Param { term } => {
                    println!("p1");
                    let target = *self.call_order.get(self.param_index).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                    self.param_index += 1;
                    self.force_term_at(&term, &target.to_string())?;
                    self.lock_reg(&target.to_string(), true);
                    self.buf.comment(format!("param {:?}", term));
                }
                ir::Op::Call { func, res } => {
                    self.buf.push_line(format!("call {}", func));
                    self.buf.comment(format!("{:?}", op_clone));
                    if let Some(r) = res {
                        if r.is_stack() {
                            self.store_term(r.clone(), "rax".to_string());
                        } else {
                            self.lock_reg(&"rax".to_string(), true);
                        }
                        self.save_reg(&"rax".to_string(), &r);
                    }
                    for name in self.call_order.clone() {
                        let r = self.get_reg(&name.to_string()).unwrap();
                        r.term = None;
                        r.locked = false;
                    }
                }
                ir::Op::EndCall => {
                    let state = self.saved_states.pop().unwrap();
                    self.param_index = state.param_index;
                    if self.sp != state.sp {
                        self.buf.push_line(format!("add rsp, {}", self.sp - state.sp));
                        self.sp = state.sp;
                    }
                    for r in state.regs.iter().rev() {
                        if r.name == "rax" {
                            let free_name = self.get_free_reg()?;
                            let free_reg = self.get_reg(&free_name).unwrap();
                            *free_reg = r.clone();
                            free_reg.name = free_name.clone();
                            self.buf.push_line(format!("pop {}", free_name));
                        } else {
                            *self.get_reg(&r.name).unwrap() = r.clone();
                            self.buf.push_line(format!("pop {}", r.name));
                        }
                        self.sp -= 8;
                        println!("restore {:?} into {} lock {:?}", r.term.clone().unwrap(), r.name, self.get_reg(&r.name));
                        self.buf.comment(format!("restore {:?}", r.term.clone().unwrap()));
                    }
                    self.calling = false;
                }
                ir::Op::Label { label } => {
                    self.buf.dedent();
                    self.buf.push_line("");
                    let str = format!(".L{}:", label);
                    self.buf.push_line(format!("{:<width$}", str, width = 8));
                    self.buf.indent();
                }
                ir::Op::Jump { label } => self.buf.push_line(format!("jmp .L{}", label)),
                ir::Op::CondJump { cond, label } => {
                    let r = self.eval_term(cond, false)?;
                    self.buf.push_line(format!("test {}, {}", r, r));
                    self.buf.push_line(format!("jnz .L{}", label));
                }
                ir::Op::BinJump { lhs, rhs, op, label } => {
                    let r1 = self.eval_term(lhs, false)?;
                    let r2 = self.eval_term(rhs, true)?;
                    self.buf.push_line(format!("cmp {}, {}", r1, r2));
                    let jmp_instr = match op.as_str() {
                        ">" => "jg",
                        "<" => "jl",
                        ">=" => "jge",
                        "<=" => "jle",
                        "==" => "je",
                        "!=" => "jne",
                        _ => panic!("Unsupported binary operation in BinJump: {}", op),
                    };
                    self.buf.push_line(format!("{} .L{}", jmp_instr, label));
                    self.lock_reg(&r1, false);
                    self.lock_reg(&r2, false);
                }
                ir::Op::Return { term }=> {
                    if let Some(t) = term {
                        self.eval_term_at(&t, &"rax".to_string());
                    } else {
                        self.eval_term_at(&Term::IntLit(0), &"rax".to_string());
                    }
                    if self.sp > 0 {
                        self.buf.push_line(format!("add rsp, {}", self.sp));
                    }
                    self.buf.push_line("ret");
                }
                ir::Op::NaturalFlow => (),
                ir::Op::Save => self.reg_states.push(self.regs.to_vec()),
                ir::Op::Restore => self.restore_regs(),
            }
            match op_clone {
                ir::Op::Save | ir::Op::Restore | ir::Op::Arg { term: _ } | ir::Op::Param { term: _ } | ir::Op::Call { func: _, res: _ } | ir::Op::BeginCall => (),
                _ => self.buf.comment(format!("{:?}", op_clone)),
            }
        }
        Ok(())
    }

    fn get_reg(&mut self, name: &String) -> Option<&mut Reg> {
        for r in self.regs.iter_mut() {
            if r.name == *name {
                return Some(r);
            }
        }
        None
    }

    fn save_reg(&mut self, reg: &String, term: &Term) {
        self.get_reg(reg).map(|r| r.term = Some(term.clone()));
    }

    fn lock_reg(&mut self, reg: &String, lock: bool) {
        self.get_reg(reg).map(|r| r.locked = lock);
    }

    fn restore_regs(&mut self) {
        let state = self.reg_states.pop().unwrap();
        let binding = self.regs.clone();
        for r in binding.iter().zip(state.iter()) {
            if r.0 != r.1 {
                if let Some(t) = &r.1.term {
                    self.eval_term_at(t, &r.1.name.clone());
                    self.save_reg(&r.1.name.clone(), t);
                    self.lock_reg(&r.1.name.clone(), r.1.locked);
                } else {
                    let reg = self.get_reg(&r.1.name).unwrap();
                    reg.term = None;
                    reg.locked = false;
                }
            }
        }
    }
    fn get_free_reg(&self) -> Result<String, GenError> {
        // Find any empty register
        for r in self.regs.iter() {
            if r.term.is_none() {
                return Ok(r.name.clone());
            }
        }
        // Find any register with a literal
        for r in self.regs.iter() {
            if !r.locked {
                if let Some(Term::IntLit(_)) = &r.term {
                    return Ok(r.name.clone());
                }
            }
        }
        // Find any unlocked register
        for r in self.regs.iter() {
            if !r.locked {
                return Ok(r.name.clone());
            }
        }
        Err(GenError::NoFreeRegisters(self.last_loc.clone()))
    }

    /// Stores a term from a register to the stack
    fn store_term(&mut self, term: Term, reg: String) {
        let res_loc: i64;
        if let Some(loc) = self.locs.get(&term) {
            res_loc = *loc;
            if self.sp == res_loc {
                self.buf.push("mov qword [rsp], ");
            } else {
                self.buf.push(format!("mov qword [rsp + {}], ", self.sp - res_loc));
            }
        } else {
            self.sp += 8;
            res_loc = self.sp;
            self.locs.insert(term.clone(), res_loc);
            self.buf.push("push ");
        }
        self.buf.push_line(format!("{}", reg));
        //self.buf.comment(format!("store into {:?}", term));
    }

    fn swap_regs(&mut self, r1: String, r2: String) {
        if r1 == r2 {
            return;
        }
        self.buf.push_line(format!("xchg {}, {}", r1, r2));
        let mut other: Option<&mut Reg> = None;
        for r in self.regs.iter_mut() {
            if r.name == r1 || r.name == r2 {
                if let Some(ref mut t) = other {
                    let temp = (t.term.clone(), t.locked);
                    (t.term, t.locked) = (r.term.clone(), r.locked);
                    (r.term, r.locked) = temp;
                } else {
                    other = Some(r);
                }
            }
        }
        //println!("swap {} {}, {:?}", r1, r2, self.regs);
    }

    /// Find a term and move it to a target register
    fn eval_term_at(&mut self, term: &Term, target: &String) {
        if let Term::IntLit(i) = term {
            self.buf.push_line(format!("mov {}, {}", target, i));
            return;
        }
        for r in self.regs.iter_mut() {
            if r.term == Some(term.clone()) {
                if r.name != *target {
                    self.buf.push_line(format!("mov {}, {}", target, r.name));
                    r.term = None;
                    r.locked = false;
                }
                return;
            }
        }
        if let Some(loc) = self.locs.get(term) {
            self.buf.push(format!("mov {}, [rsp", target));
            if self.sp != *loc {
                self.buf.push(" + ");
                self.buf.push((self.sp - loc).to_string());
            }
            self.buf.push_line("]");
            return;
        }
        if let Term::Data(s) = term {
            if let Some(Symbol::StringLit { str: _ }) = self.symbol_table.get(s) {
                self.buf.push_line(format!("mov {}, {}", target, s));
                return;
            }
        }
        panic!("Couldn't find {:?}", term);
    }

    fn eval_term(&mut self, term: Term, allow_literals: bool) -> Result<String, GenError> {
        if allow_literals {
            if let Term::IntLit(i) = term {
                return Ok(i.to_string());
            }
        }
        for r in self.regs.iter() {
            if r.term == Some(term.clone()) {
                return Ok(r.name.clone());
            }
        }
        let target = self.get_free_reg()?;
        //println!("eval {:?}, {}, {:?}", term, target, self.regs);
        self.eval_term_at(&term, &target);
        self.save_reg(&target, &term);
        Ok(target)
    }

    fn force_term_at(&mut self, term: &Term, target: &String) -> Result<(), GenError> {
        println!("force {:?} at {}", term, target);
        let mut free = None;
        if let Some(r) = self.get_reg(&target.to_string()) {
            if let Some(t) = &r.term {
                if term != t {
                    let binding = t.clone();
                    r.term = None;
                    if r.locked {
                        r.locked = false;
                        free = Some((self.get_free_reg()?, binding));
                    }
                } else {
                    return Ok(());
                }
            }
        }
        if let Some(f) = free {
            println!("mov {}, {}", f.0, target);
            self.buf.push_line(format!("mov {}, {}", f.0, target));
            self.save_reg(&f.0, &f.1);
            self.lock_reg(&f.0, true);
        }
        self.eval_term_at(&term, &target.to_string());
        self.save_reg(target, term);
        Ok(())
    }

    fn free_reg(&mut self, reg: &String) -> Result<(), GenError> {
        println!("free {}", reg);
        let mut free = None;
        if let Some(r) = self.get_reg(&reg.to_string()) {
            if let Some(t) = &r.term {
                let binding = t.clone();
                r.term = None;
                if r.locked {
                    r.locked = false;
                    free = Some((self.get_free_reg()?, binding));
                }
            }
        }
        if let Some(f) = free {
            self.buf.push_line(format!("mov {}, {}", f.0, reg));
            self.save_reg(&f.0, &f.1);
            self.lock_reg(&f.0, true);
        }
        Ok(())
    }

    fn binop(&mut self, lhs: Term, rhs: Term, op: String, res: Option<Term>) -> Result<(), GenError> {
        //println!("tac {:?} {:?} {:?}={:?}", lhs, op_opt, rhs_opt, res);
        let mut r1 = self.eval_term(lhs.clone(), false)?;
        self.lock_reg(&r1, true);
        let mut r2;
        r2 = self.eval_term(rhs.clone(), !["*", "/", "%"].contains(&op.as_str()))?;
        match op.as_str() {
            "+" => self.bin("add", &r1, &r2),
            "-" => self.bin("sub", &r1, &r2),
            "*" => self.bin_rax("mul", &mut r1, &mut r2),
            "/" => self.bin_rax("div", &mut r1, &mut r2),
            "%" => {
                self.bin_rax("div", &mut r1, &mut r2);
                self.buf.push_line("xchg rax, rdx");
            },
            ">" => self.cond("setg", &r1, &r2),
            "<" => self.cond("setl", &r1, &r2),
            ">=" => self.cond("setge", &r1, &r2),
            "<=" => self.cond("setle", &r1, &r2),
            "==" => self.cond("sete", &r1, &r2),
            "!=" => self.cond("setne", &r1, &r2),
            "||" => self.bool_op("or", &r1, &r2),
            "&&" => self.bool_op("and", &r1, &r2),
            "|" => self.bin("or", &r1, &r2),
            "&" => self.bin("and", &r1, &r2),
            "^" => self.bin("xor", &r1, &r2),
            ">>" => self.bin_cl("shr", &mut r1, &mut r2),
            "<<" => self.bin_cl("shl", &mut r1, &mut r2),
            _ => todo!(),
        }
        self.save_reg(&r2, &rhs);
        self.lock_reg(&r2, false);
        if let Some(r) = &res {
            self.save_reg(&r1, r);
            if r.is_stack() {
                // Store variable on the stack, no need to lock the register
                self.lock_reg(&r1, false);
                self.store_term(r.clone(), r1);
            } else {
                // Not storing temp variables on the stack, so lock the register
                self.lock_reg(&r1, true);
            }
        } else {
            self.lock_reg(&r1, false);
        }
        Ok(())
    }

    fn unop(&mut self, term: Term, op: String, res: Term) -> Result<(), GenError> {
        match op.as_str() {
            "&" => {
                let loc = self.locs.get(&term).expect(format!("Couldn't find {:?}", term).as_str());
                let target = self.get_free_reg()?;
                if self.sp == *loc {
                    self.buf.push_line(format!("mov {}, rsp", target));
                } else {
                    self.buf.push_line(format!("mov {}, rsp", target));
                    self.buf.push_line(format!("add {}, {}", target, self.sp - loc));
                }
                self.save_reg(&target, &res);
                self.lock_reg(&target, true);
                return Ok(());
            }
            _ => (),
        }
        let r = self.eval_term(term, false)?;
        match op.as_str() {
            "-" => self.buf.push_line(format!("neg {}", r)),
            "*" => self.buf.push_line(format!("mov {}, [{}]", r, r)),
            _ => unreachable!(),
        }
        self.save_reg(&r, &res);
        self.lock_reg(&r, true);
        Ok(())
    }

    fn store(&mut self, term: Term, op: String, ptr: Term, mut offset: i64, res: Option<Term>) -> Result<(), GenError> {
        let mut t = self.eval_term(term, res.is_none() && op != "*=" && op != "/=" && op != "%=")?;
        let p;
        if let Term::Stack(_) = ptr {
            p = "rsp".to_string();
            offset += self.sp - self.locs.get(&ptr).unwrap();
        } else {
            p = self.eval_term(ptr, false)?;
        }
        let o;
        if offset == 0 {
            o = String::new();
        } else if offset > 0 {
            o = format!("+{}", offset);
        } else {
            o = format!("-{}", -offset);
        }
        match op.as_str() {
            "=" => self.buf.push_line(format!("mov qword [{}{}], {}", p, o, t)),
            "+=" => self.buf.push_line(format!("add qword [{}{}], {}", p, o, t)),
            "-=" => self.buf.push_line(format!("sub qword [{}{}], {}", p, o, t)),
            "|=" => self.buf.push_line(format!("or qword [{}{}], {}", p, o, t)),
            "^=" => self.buf.push_line(format!("xor qword [{}{}], {}", p, o, t)),
            "&=" => self.buf.push_line(format!("and qword [{}{}], {}", p, o, t)),
            "*=" => {
                let mut free = self.get_free_reg()?;
                self.buf.push_line(format!("mov {}, [{}{}]", free, p, o));
                self.bin_rax("mul", &mut free, &mut t);
                self.buf.push_line(format!("mov qword [{}{}], {}", p, o, free));
            }
            "/=" => {
                let mut free = self.get_free_reg()?;
                self.buf.push_line(format!("mov {}, [{}{}]", free, p, o));
                self.bin_rax("div", &mut free, &mut t);
                self.buf.push_line(format!("mov qword [{}{}], {}", p, o, free));
            }
            "%=" => {
                let mut free = self.get_free_reg()?;
                self.buf.push_line(format!("mov {}, [{}{}]", free, p, o));
                self.bin_rax("div", &mut free, &mut t);
                self.buf.push_line("xchg rax, rdx");
                self.buf.push_line(format!("mov qword [{}{}], {}", p, o, free));
            }
            ">>=" => self.bin_cl("shr qword", &mut format!("[{}{}]", p, o), &mut t),
            "<<=" => self.bin_cl("shl qword", &mut format!("[{}{}]", p, o), &mut t),
            _ => unreachable!(),
        }
        if let Some(r) = res {
            self.save_reg(&t, &r);
            self.lock_reg(&t, true);
        } else {
            self.lock_reg(&t, false);
        }
        self.lock_reg(&p, false);
        // Might have modified a symbol on the stack, so forget about symbols in the registers
        for r in self.regs.iter_mut() {
            if let Some(t) = &r.term {
                if t.is_stack() {
                    r.term = None;
                }
            }
        }
        Ok(())
    }

    fn read(&mut self, ptr: Term, mut offset: i64, res: Term) -> Result<(), GenError> {
        let r;
        if let Term::Stack(_) = ptr {
            r = "rsp".to_string();
            println!("sp {} loc {} offset {}", self.sp, self.locs.get(&ptr).unwrap(), offset);
            offset += self.sp - self.locs.get(&ptr).unwrap();
        } else {
            r = self.eval_term(ptr.clone(), false)?
        }
        let o;
        if offset == 0 {
            o = String::new();
        } else if offset > 0 {
            o = format!("+{}", offset);
        } else {
            o = format!("-{}", -offset);
        }
        if let Term::Stack(_) = ptr {
            println!("eval {:?}", res);
            let res_reg = self.get_free_reg()?;
            println!("at {}",  res_reg);
            self.buf.push_line(format!("mov {}, [{}{}]", res_reg, r, o));
            self.save_reg(&res_reg, &res);
            self.lock_reg(&res_reg, true);
        } else {
            println!("r {}", r);
            self.buf.push_line(format!("mov {}, [{}{}]", r, r, o));
            self.save_reg(&r, &res);
            self.lock_reg(&r, true);
        }
        Ok(())
    }

    fn bin(&mut self, op: &str, r1: &String, r2: &String) {
        self.buf.push_line(format!("{} {}, {}", op, r1, r2));
    }

    fn bin_rax(&mut self, op: &str, r1: &mut String, r2: &mut String) {
        self.swap_regs(r1.to_string(), "rax".to_string());
        if r2 == "rax" {
            *r2 = r1.to_string();
        } else if r2 == r1 {
            *r2 = "rax".to_string();
        }
        *r1 = "rax".to_string();
        if op == "div" {
            self.buf.push_line("xor rdx, rdx");
        }
        self.buf.push_line(format!("{} {}", op, r2));
    }
    fn bin_cl(&mut self, op: &str, r1: &mut String, r2: &mut String) {
        if r2.parse::<i64>().is_ok() {
            self.buf.push_line(format!("{} {}, {}", op, r1, r2));
            return;
        }
        self.swap_regs(r2.to_string(), "rcx".to_string());
        if r1 == "rcx" {
            *r1 = r2.to_string();
        } else if r2 == r1 {
            *r1 = "rcx".to_string();
        }
        *r2 = "rcx".to_string();
        self.buf.push_line(format!("{} {}, {}", op, r1, Self::reg_8bit(r2)));
    }

    fn cond(&mut self, cond: &str, r1: &String, r2: &String) {
        if r2 != "0" {
            self.buf.push_line(format!("sub {}, {}", r1, r2));
        }
        self.buf.push(cond);
        self.buf.push_line(format!(" {}", Self::reg_8bit(r1)));
        self.buf.push_line(format!("movzx {}, {}", r1, Self::reg_8bit(r1)));
    }

    fn bool_op(&mut self, op: &str, r1: &String, r2: &String) {
        self.buf.push_line(op);
        self.buf.push_line(format!(" {}, {}", Self::reg_8bit(r2), Self::reg_8bit(r1)));
        self.buf.push_line(format!("movzx {}, {}", r1, Self::reg_8bit(r2)));
    }

    fn assign_bin(&mut self, op: &str, term: Term, r1: &String, r2: &String) {
        self.bin(op, r1, r2);
        self.store_term(term.clone(), r1.to_string());
    }

    fn assign_rax(&mut self, op: &str, term: Term, r1: &mut String, r2: &mut String) {
        self.bin_rax(op, r1, r2);
        self.store_term(term.clone(), r1.to_string());
    }

    fn reg_8bit(reg: &String) -> String {
        match reg.as_str() {
            "rax" => "al".to_string(),
            "rbx" => "bl".to_string(),
            "rcx" => "cl".to_string(),
            "rdx" => "dl".to_string(),
            "rsi" => "sil".to_string(),
            "rdi" => "dil".to_string(),
            _ => panic!("No 8 bit counterpart"),
        }
    }
}
