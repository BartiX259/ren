use crate::helpers::IndentedBuf;
use crate::ir::{self, Block, Macro, OpLoc, Symbol, Term, Type};
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
struct Gen<'a> {
    buf: IndentedBuf,
    symbol_table: &'a mut HashMap<String, Symbol>,
    fn_symbols: Vec<Vec<(String, Symbol)>>,
    temp_table: Vec<Type>,
    macro_list: Vec<Macro>,
    locs: HashMap<Term, i64>,
    regs: Vec<Reg>,
    sp: i64,
    arg_ptr: i64,
    last_loc: OpLoc,
    reg_states: Vec<Vec<Reg>>,
    saved_regs: Vec<Reg>,
    calling: bool,
    param_size: i64,
}

impl<'a> Gen<'a> {
    pub fn new(ir: &'a mut HashMap<String, Symbol>) -> Self {
        Self {
            buf: IndentedBuf::new(8, 16),
            symbol_table: ir,
            fn_symbols: Vec::new(),
            temp_table: Vec::new(),
            macro_list: Vec::new(),
            locs: HashMap::new(),
            // Can't use rdx because it has to be 0 to divide
            regs: vec![reg("rax"), reg("rbx"), reg("rcx"), reg("rdi"), reg("rsi")],
            sp: 0,
            arg_ptr: 0,
            last_loc: OpLoc { start_id: 0, end_id: 0 },
            reg_states: Vec::new(),
            saved_regs: Vec::new(),
            calling: false,
            param_size: 0,
        }
    }
    fn all(&mut self) -> Result<(), GenError> {
        if let Some(_) = self.symbol_table.get("_start") {
            return Err(GenError::ReservedSymbol(OpLoc { start_id: 0, end_id: 0 }));
        }
        self.buf.push_line("global _start");
        self.buf.push_line("_start:");
        self.buf.indent();
        let main = self.symbol_table.get("main");
        if let Some(Symbol::Func {
            ty: _,
            block: _,
            symbols: _,
            macros: _,
        }) = main
        {
        } else {
            return Err(GenError::NoMainFn);
        }
        self.buf.push_line("call main");
        self.buf.push_line("mov rdi, rax");
        self.buf.push_line("mov rax, 60");
        //self.buf.push_line("mov rdi, 0");
        self.buf.push_line("syscall");
        self.buf.dedent();
        self.functions()?;
        Ok(())
    }

    fn functions(&mut self) -> Result<(), GenError> {
        let binding = self.symbol_table.clone();
        let names = binding.keys();
        for name in names {
            for r in self.regs.iter_mut() {
                r.term = None;
                r.locked = false;
            }
            self.reg_states = Vec::new();
            self.locs = HashMap::new();
            self.sp = 0;
            self.arg_ptr = 0;
            let sym = self.symbol_table.get(name);
            if let Some(Symbol::Func { ty: _, block, symbols, macros }) = sym {
                self.buf.push_line("");
                self.buf.push_line(format!("{}:", name));
                self.buf.indent();
                self.fn_symbols = symbols.clone();
                self.macro_list = macros.clone();
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
            //for op in block.ops {
            self.last_loc = locs.next().unwrap().clone();
            let op_clone = op.clone();
            match op {
                ir::Op::Tac { lhs, rhs, op, res } => self.tac(lhs, rhs, op, res)?,
                ir::Op::Unary { term, op, res } => self.unary(term, op, res)?,
                ir::Op::DerefAssign { term, op, ptr, res } => self.deref_assign(term, op, ptr, res)?,
                ir::Op::Arg(term) => {
                    self.arg_ptr -= 8;
                    self.locs.insert(term, self.arg_ptr);
                }
                ir::Op::Param(term) => {
                    let mut params = vec![term];
                    while let Some(ir::Op::Param(t)) = iter.peek() {
                        params.push(t.clone());
                        iter.next();
                    }
                    for r in self.regs.iter() {
                        if r.locked && !params.contains(&r.term.clone().unwrap()) {
                            self.saved_regs.push(r.clone());
                            self.buf.push_line(format!("push {}", r.name));
                            //self.buf.comment(format!("save {:?}", r.term.clone().unwrap()));
                        }
                    }
                    for p in params {
                        let r = self.eval_term(p.clone(), true)?;
                        self.sp += 8;
                        self.param_size += 8;
                        self.buf.push_line(format!("push {}", r));
                        //self.buf.comment(format!("param {:?}", p));
                        self.lock_reg(&r, false);
                    }
                }
                ir::Op::Call { func, res } => {
                    for r in self.regs.iter_mut() {
                        r.term = None;
                        r.locked = false;
                    }
                    self.buf.push_line(format!("call {}", func));
                    if let Term::Symbol(_) = res {
                        self.store_term(res.clone(), "rax".to_string());
                    } else {
                        self.lock_reg(&"rax".to_string(), true);
                    }
                    self.save_reg(&"rax".to_string(), &res);
                    if self.param_size != 0 {
                        self.sp -= self.param_size;
                        self.buf.push_line(format!("add rsp, {}", self.param_size));
                    }
                    for r in self.saved_regs.clone().iter().rev() {
                        if r.name == "rax" {
                            let free_name = self.get_free_reg()?;
                            let free_reg = self.get_reg(&free_name).unwrap();
                            *free_reg = r.clone();
                            free_reg.name = free_name.clone();
                            self.buf.push_line(format!("pop {}", free_name));
                            //self.buf.comment(format!("restore {:?} into {}", r.term.clone().unwrap(), free_name));
                            println!("restore {:?}", self.get_reg(&free_name));
                        } else {
                            *self.get_reg(&r.name).unwrap() = r.clone();
                            self.buf.push_line(format!("pop {}", r.name));
                            //self.buf.comment(format!("restore {:?}", r.term.clone().unwrap()));
                            println!("restore {:?}", self.regs);
                        }
                    }
                    self.calling = false;
                    self.param_size = 0;
                    self.saved_regs = Vec::new();
                }
                ir::Op::Macro { id, res } => match self.macro_list.get(id).unwrap().clone() {
                    Macro::Salloc { size, ty } => {
                        self.sp += size;
                        self.buf.push_line(format!("sub rsp, {}", size));
                        if let Some(r) = res {
                            let reg = self.get_free_reg()?;
                            self.buf.push_line(format!("mov {}, rsp", reg));
                            self.save_reg(&reg, &r);
                            if let Term::Symbol(_) = r {
                                self.store_term(r.clone(), reg);
                            } else {
                                self.lock_reg(&reg, true);
                            }
                        }
                        //self.buf.comment(format!("{:?} = salloc {}",, size));
                    }
                },
                ir::Op::Label(label) => {
                    self.buf.dedent();
                    self.buf.push_line("");
                    let str = format!(".L{}:", label);
                    self.buf.push(format!("{:<width$}", str, width = 8));
                    self.buf.indent();
                }
                ir::Op::Jump(label) => self.buf.push_line(format!("jmp .L{}", label)),
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
                ir::Op::Return(term) => {
                    //self.eval_term(term.clone(), true)?;
                    self.eval_term_at(&term, &"rax".to_string());
                    //self.buf.comment("|");
                    if self.sp > 0 {
                        self.buf.push_line(format!("add rsp, {}", self.sp));
                        //self.buf.comment("|");
                    }
                    self.buf.push_line("ret");
                    //self.buf.comment(format!("return {:?}", term));
                }
                ir::Op::LoadSymbols(i) => {
                    for s in self.fn_symbols.get(i).unwrap().iter() {
                        self.symbol_table.insert(s.0.clone(), s.1.clone());
                    }
                }
                ir::Op::UnloadSymbols(i) => {
                    for s in self.fn_symbols.get(i).unwrap().iter() {
                        self.symbol_table.remove(&s.0);
                    }
                }
                ir::Op::LoopStart => self.reg_states.push(self.regs.to_vec()),
                ir::Op::LoopEnd => self.restore_regs(),
            }
            match op_clone {
                ir::Op::LoadSymbols(_) | ir::Op::UnloadSymbols(_) | ir::Op::LoopStart | ir::Op::LoopEnd | ir::Op::Arg(_) => (),
                _ => self.buf.comment(format!("{:?}", op_clone)),
            }
        }
        Ok(())
    }

    fn get_type(&self, term: &Term) -> Option<&Type> {
        match term {
            Term::Symbol(str) => match self.symbol_table.get(str) {
                _ => None,
            },
            Term::Temp(id) => self.temp_table.get(id - 1),
            _ => None,
        }
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

    fn store_term(&mut self, term: Term, reg: String) {
        let res_loc: i64;
        if let Some(loc) = self.locs.get(&term) {
            res_loc = *loc;
            if self.sp == res_loc {
                self.buf.push("mov [rsp], ");
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
        for r in self.regs.iter() {
            if r.term == Some(term.clone()) {
                if r.name != *target {
                    self.buf.push_line(format!("mov {}, {}", target, r.name));
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
        } else {
            match &term {
                Term::IntLit(val) => {
                    self.buf.push_line(format!("mov {}, {}", target, val));
                }
                _ => {
                    println!("Couldn't find {:?}", term);
                    unreachable!()
                }
            }
        }
    }

    fn eval_term(&mut self, term: Term, allow_literals: bool) -> Result<String, GenError> {
        if allow_literals {
            if let Term::IntLit(i) = term {
                return Ok(i);
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

    fn tac(&mut self, lhs: Term, rhs_opt: Option<Term>, op_opt: Option<String>, res: Option<Term>) -> Result<(), GenError> {
        //println!("tac {:?} {:?} {:?}={:?}", lhs, op_opt, rhs_opt, res);
        let mut r1 = "".to_string();
        if op_opt != Some("=".to_string()) {
            r1 = self.eval_term(lhs.clone(), rhs_opt.is_none())?;
            self.lock_reg(&r1, true);
        }
        let mut r2;
        if let Some(rhs) = rhs_opt {
            if let Some(op) = op_opt {
                r2 = self.eval_term(rhs.clone(), !(op.contains("*") || op.contains("/")))?;
                match op.as_str() {
                    "+" => self.bin("add", &r1, &r2),
                    "-" => self.bin("sub", &r1, &r2),
                    "*" => self.bin_rax("mul", &mut r1, &mut r2),
                    "/" => self.bin_rax("div", &mut r1, &mut r2),
                    ">" => self.cond("setg", &r1, &r2),
                    "<" => self.cond("setl", &r1, &r2),
                    ">=" => self.cond("setge", &r1, &r2),
                    "<=" => self.cond("setle", &r1, &r2),
                    "==" => self.cond("sete", &r1, &r2),
                    "!=" => self.cond("setne", &r1, &r2),
                    "||" => self.bool_op("or", &r1, &r2),
                    "&&" => self.bool_op("and", &r1, &r2),
                    "=" => {
                        for r in self.regs.iter_mut() {
                            if let Some(s) = &r.term {
                                if lhs == *s {
                                    r.term = None;
                                }
                            }
                        }
                        r1 = r2.clone();
                        self.store_term(lhs.clone(), r2.to_string())
                    }
                    "+=" => self.assign_bin("add", lhs.clone(), &r1, &r2),
                    "-=" => self.assign_bin("sub", lhs.clone(), &r1, &r2),
                    "*=" => self.assign_rax("mul", lhs.clone(), &mut r1, &mut r2),
                    "/=" => self.assign_rax("div", lhs.clone(), &mut r1, &mut r2),
                    _ => todo!(),
                }
                self.save_reg(&r2, &rhs);
                self.lock_reg(&r2, false);
            }
        }
        if let Some(r) = &res {
            self.save_reg(&r1, r);
            if let Term::Symbol(_) = r {
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

    fn unary(&mut self, term: Term, op: String, res: Term) -> Result<(), GenError> {
        match op.as_str() {
            "&" => {
                let loc = self.locs.get(&term).unwrap();
                let target = self.get_free_reg()?;
                if self.sp == *loc {
                    self.buf.push_line(format!("mov {}, rsp", target));
                    //self.buf.comment(format!("address of {:?}", term));
                } else {
                    self.buf.push_line(format!("mov {}, rsp", target));
                    //self.buf.comment("|");
                    self.buf.push_line(format!("add {}, {}", target, self.sp - loc));
                    //self.buf.comment(format!("address of {:?}", term));
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
            "*" => {
                self.buf.push_line(format!("mov {}, [{}]", r, r));
            }
            _ => unreachable!(),
        }
        self.save_reg(&r, &res);
        self.lock_reg(&r, true);
        Ok(())
    }

    fn deref_assign(&mut self, term: Term, op: String, ptr: Term, res: Option<Term>) -> Result<(), GenError> {
        let mut t = self.eval_term(term, res.is_none() && op != "*=" && op != "/=")?;
        let p = self.eval_term(ptr, false)?;
        match op.as_str() {
            "=" => self.buf.push_line(format!("mov qword [{}], {}", p, t)),
            "+=" => self.buf.push_line(format!("add qword [{}], {}", p, t)),
            "-=" => self.buf.push_line(format!("sub qword [{}], {}", p, t)),
            "*=" => {
                let mut free = self.get_free_reg()?;
                self.buf.push_line(format!("mov {}, [{}]", free, p));
                self.bin_rax("mul", &mut free, &mut t);
                self.buf.push_line(format!("mov qword [{}], {}", p, free));
            }
            "/=" => {
                let mut free = self.get_free_reg()?;
                self.buf.push_line(format!("mov {}, [{}]", free, p));
                self.bin_rax("div", &mut free, &mut t);
                self.buf.push_line(format!("mov qword [{}], {}", p, free));
            }
            _ => unreachable!(),
        }
        if let Some(r) = res {
            self.save_reg(&t, &r);
            self.lock_reg(&t, true);
        }
        // Might have modified a symbol on the stack, so forget about symbols in the registers
        for r in self.regs.iter_mut() {
            if let Some(Term::Symbol(_)) = r.term {
                r.term = None;
            }
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
