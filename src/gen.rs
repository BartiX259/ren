use crate::helpers::IndentedBuf;
use crate::ir::{self, Block, OpLoc, Symbol, Term};
use std::collections::HashMap;

/// Generate nasm code based on the IR
pub fn gen(ir: &mut HashMap<String, Symbol>) -> Result<String, (String, GenError)> {
    let mut gen = Gen::new(ir);
    gen.all().map_err(|e| (gen.cur_module, e))?;
    Ok(gen.buf.get_output())
}

pub enum GenError {
    NoMainFn,
    NoFreeRegisters(OpLoc),
    ReservedSymbol(OpLoc),
    TooManyArguments(OpLoc),
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
    sp: i64,
}

struct Gen<'a> {
    buf: IndentedBuf,
    cur_module: String,
    cur_block: Block,
    cur_fn: String,
    symbol_table: &'a mut HashMap<String, Symbol>,
    locs: HashMap<Term, i64>,
    doubles: HashMap<Term, (String, String)>,
    regs: Vec<Reg>,
    call_order: Vec<&'a str>,
    sp: i64,
    last_loc: OpLoc,
    reg_states: Vec<Vec<Reg>>,
    saved_states: Vec<ProgState>,
    saved_sps: Vec<i64>,
    params: Vec<Vec<Term>>,
    param_index: usize,
    arg_index: usize,
    calling: bool,
}

impl<'a> Gen<'a> {
    pub fn new(ir: &'a mut HashMap<String, Symbol>) -> Self {
        Self {
            buf: IndentedBuf::new(8, 24),
            cur_module: String::new(),
            cur_block: Block::new(),
            cur_fn: String::new(),
            symbol_table: ir,
            locs: HashMap::new(),
            doubles: HashMap::new(),
            regs: vec![reg("rax"), reg("rbx"), reg("rcx"), reg("rdx"), reg("rdi"), reg("rsi"), reg("r8"), reg("r9")],
            call_order: vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            sp: 0,
            last_loc: OpLoc { start_id: 0, end_id: 0 },
            reg_states: Vec::new(),
            saved_states: Vec::new(),
            saved_sps: Vec::new(),
            params: Vec::new(),
            param_index: 0,
            arg_index: 0,
            calling: false,
        }
    }
    fn all(&mut self) -> Result<(), GenError> {
        // Data section
        self.buf.push_line("section .rodata");
        self.buf.indent();
        self.rodata();
        self.buf.dedent();
        self.buf.push_line("");
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
            } else if let Symbol::Func { public, .. } = sym {
                if *public {
                    self.buf.push_line(format!("global {}", name));
                }
            }
        }
        // _start function
        if let Some(_) = self.symbol_table.get("_start") {
            return Err(GenError::ReservedSymbol(OpLoc { start_id: 0, end_id: 0 }));
        }
        if let Some(Symbol::MainFunc {
            init,
            arg_parse,
            arg_names,
            args,
            parse_fns,
            print,
            print_help,
            module,
            span,
            ..
        }) = self.symbol_table.get("main")
        {
            self.cur_module = module.to_string();
            self.buf.push_line("global _start");
            self.buf.dedent();
            self.buf.push_line("");

            // The `args.fail` block is called on any parsing error.
            if !init.is_empty() {
                self.buf.push_line("args.fail:");
                self.buf.indent();
                self.buf.push_line("mov rsi, [rax+16]"); // Error message
                self.buf.push_line("mov rdi, [rax+8]"); // Error code
                self.buf.push_line(format!("call {print}"));
                self.buf.push_line("mov rdi, [rbp+8]"); // Program name
                self.buf.push_line(format!("mov rsi, {}", arg_names.len()));
                self.buf.push_line("mov rdx, args."); // Argument metadata
                self.buf.push_line(format!("call {print_help}"));
                self.buf.push_line("mov rdi, 1"); // Exit with error code 1
                self.buf.push_line("mov rax, 60");
                self.buf.push_line("syscall");
                self.buf.dedent();
                self.buf.push_line("");
            }

            self.buf.push_line("_start:");
            self.buf.indent();

            const ARG_PARSE_RESULT_SIZE: u32 = 24;
            let mut args_size = 0;

            // Separate arguments into flags and positionals to handle them in two stages.
            let mut flags = Vec::new();
            let mut positionals = Vec::new();
            let mut reg_info = vec![0; args.len()];
            let mut current_offset = ARG_PARSE_RESULT_SIZE;

            for (i, (ty, parse_fn)) in args.iter().zip(parse_fns.iter()).enumerate() {
                if parse_fn.is_empty() {
                    continue;
                }
                let size = ty.aligned_size();
                let item = (ty, parse_fn, i, current_offset);
                args_size += size;
                if parse_fn.contains("opt") || parse_fn.contains("flag") {
                    flags.push(item);
                } else {
                    positionals.push(item);
                }
                reg_info[i] = current_offset;
                current_offset += size;
            }

            if !init.is_empty() {
                self.buf.push_line(format!("call {init}"));
            }

            self.buf.push_line("mov rbp, rsp");
            self.buf.push_line(format!("sub rsp, {}", args_size + ARG_PARSE_RESULT_SIZE));
            self.buf.push_line("");

            if !flags.is_empty() {
                self.buf.push_line(";; Flag Parsing");
                for (_, parse_fn, arg_idx, stack_offset) in &flags {
                    self.buf.push_line("mov rdi, rsp"); // rdi: error struct
                    self.buf.push_line("mov rsi, rbp"); // rsi: &argc
                    self.buf.push_line("mov rdx, rbp"); // rdx: argv
                    self.buf.push_line("add rdx, 8");
                    self.buf.push_line(format!("mov rcx, [args.+{}]", arg_idx * 16)); // rcx: start index for search
                    self.buf.push_line(format!("mov r8, [args.+{}]", arg_idx * 16 + 8)); // r8: end index for search
                    self.buf.push_line(format!("lea r9, [rsp+{}]", stack_offset)); // r9: pointer to final value destination
                    self.buf.push_line(format!("call {}", parse_fn));
                    self.buf.push_line("mov rbx, [rax]"); // check error pointer
                    self.buf.push_line("test rbx, rbx");
                    self.buf.push_line("jnz args.fail");
                    self.buf.push_line("");
                }
            }

            if !arg_parse.is_empty() {
                self.buf.push_line("; Positional Argument Parsing");
                self.buf.push_line("mov rdi, rsp"); // rdi: error struct
                self.buf.push_line("mov rsi, [rbp]"); // rsi: argc
                self.buf.push_line("mov rdx, rbp"); // rdx: argv
                self.buf.push_line("add rdx, 8");
                self.buf.push_line(format!("mov rcx, {}", positionals.len())); // rcx: Number of positionals to find
                self.buf.push_line("mov r8, args."); // r8: Pointer to all argument metadata
                self.buf.push_line(format!("call {}", arg_parse));
                self.buf.push_line("mov rbx, [rax]");
                self.buf.push_line("test rbx, rbx");
                self.buf.push_line("jnz args.fail");
                self.buf.push_line("");
            }

            if !positionals.is_empty() {
                self.buf.push_line(";; Positional Value Conversion");
                for (i, (_, parse_fn, _, stack_offset)) in positionals.iter().enumerate() {
                    // The value parser function signature is: (err_ptr, string_to_parse_ptr, value_dest_ptr)
                    // It uses registers: rdi, rsi, rdx
                    self.buf.push_line(format!(";; Convert positional argument {}", i));
                    self.buf.push_line("mov rdi, rsp"); // rdi: Use top of stack for temporary error struct
                    self.buf.push_line(format!("mov rsi, [rbp+{}]", i * 8 + 16)); // rsi: Pointer to the string to parse
                    self.buf.push_line(format!("lea rdx, [rsp+{}]", stack_offset)); // rdx: Pointer to final value destination
                    self.buf.push_line(format!("call {}", parse_fn));
                    self.buf.push_line("mov rbx, [rax]");
                    self.buf.push_line("test rbx, rbx");
                    self.buf.push_line("jnz args.fail");
                    self.buf.push_line("");
                }
            }

            self.buf.push_line("call main");
            self.buf.push_line("mov rdi, rax"); // Exit with main's return code
            self.buf.push_line("mov rax, 60");
            self.buf.push_line("syscall");
        }
        self.buf.dedent();
        // Other functions
        self.functions()?;
        Ok(())
    }

    fn rodata(&mut self) {
        for (name, sym) in self.symbol_table.iter() {
            match sym {
                Symbol::Data { ty, str } => {
                    if let crate::types::Type::Slice { .. } = ty {
                        self.buf.push_line(format!("{} db {}", name, str));
                    }
                }
                Symbol::MainFunc { arg_names, .. } => {
                    for (i, name) in arg_names.iter().enumerate() {
                        self.buf.push_line(format!("arg{}.name db \"{}\"", i, name));
                    }
                    self.buf.push("args.:");
                    for (i, name) in arg_names.iter().enumerate() {
                        if i == 0 {
                            self.buf.push_line(format!("    dq {}", name.len()));
                        } else {
                            self.buf.push_line(format!("          dq {}", name.len()));
                        }
                        self.buf.push_line(format!("          dq arg{}.name", i));
                    }
                    if arg_names.is_empty() {
                        self.buf.push_line("");
                    }
                }
                _ => (),
            }
        }
    }

    fn data(&mut self) {
        for (name, sym) in self.symbol_table.iter() {
            match sym {
                Symbol::Data { ty, str } => {
                    if !matches!(ty, crate::types::Type::Slice { .. }) {
                        self.buf.push_line(format!("{} dq {}", name, str));
                    }
                }
                _ => (),
            }
        }
    }

    fn functions(&mut self) -> Result<(), GenError> {
        let mut binding: Vec<String> = self.symbol_table.keys().cloned().collect();
        binding.sort();
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
            match sym {
                Some(Symbol::MainFunc { block, module, .. }) | Some(Symbol::Func { block, module, .. }) => {
                    self.cur_module = module.to_string();
                    self.cur_fn = name.clone();
                    self.buf.push_line("");
                    self.buf.push_line(format!("{}:", name));
                    self.buf.indent();
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
                _ => (),
            }
        }
        Ok(())
    }

    fn block(&mut self, block: Block) -> Result<(), GenError> {
        self.cur_block = block.clone();
        let mut locs = block.locs.iter();
        let mut iter = block.ops.into_iter().peekable();
        while let Some(op) = iter.next() {
            self.last_loc = locs.next().unwrap().clone();
            let op_clone = op.clone();
            match op {
                ir::Op::BinOp { lhs, rhs, op, res, size } => self.binop(lhs, rhs, op, res, size)?,
                ir::Op::UnOp { term, op, res, size } => self.unop(term, op, res, size)?,
                ir::Op::Store { term, op, ptr, offset, res, size } => self.store(term, op, ptr, offset, res, size)?,
                ir::Op::Read { ptr, offset, res, size } => self.read(ptr, offset, res, size)?,
                ir::Op::Copy { from, to, size } => {
                    if size == Term::IntLit(0) {
                        continue;
                    }
                    if let Term::IntLit(_) = from {
                        self.store(from, "=".to_string(), to, 0, None, 8)?;
                        self.buf.comment(format!("{op_clone:?}"));
                        continue;
                    }
                    if let Term::Double(_) = from {
                        self.store(from, "=".to_string(), to, 0, None, 8)?;
                        self.buf.comment(format!("{op_clone:?}"));
                        continue;
                    }
                    if let Some(t) = from.stack_arithmetic() {
                        self.free_reg(&"rsi".to_string())?;
                        self.buf.push_line("mov rsi, rsp");
                        if let Some(x) = self.locs.get(&t) {
                            if *x != self.sp {
                                self.buf.push_line(format!("add rsi, {}", self.sp - x));
                            }
                        }
                    } else {
                        self.force_term_at(&from, &"rsi".to_string())?;
                    }
                    if let Some(t) = to.stack_arithmetic() {
                        self.free_reg(&"rdi".to_string())?;
                        self.buf.push_line("mov rdi, rsp");
                        if let Some(x) = self.locs.get(&t) {
                            if *x != self.sp {
                                self.buf.push_line(format!("add rdi, {}", self.sp - x));
                            }
                        }
                    } else {
                        self.force_term_at(&to, &"rdi".to_string())?;
                    }
                    if let Term::IntLit(s) = size {
                        if s < 8 {
                            self.force_term_at(&Term::IntLit(s), &"rcx".to_string())?;
                            self.buf.push_line("rep movsb");
                        } else {
                            self.force_term_at(&Term::IntLit(s >> 3), &"rcx".to_string())?;
                            self.buf.push_line("rep movsq");
                        }
                    } else {
                        let mut r1 = self.eval_term(size.clone(), None, false)?;
                        let mut r2 = "3".to_string();
                        self.bin("add", &r1, &"7".to_string(), 8);
                        self.bin_cl("shr", &mut r1, &mut r2, 8);
                        self.force_term_at(&size, &"rcx".to_string())?;
                        self.buf.push_line("rep movsq");
                    }
                    self.clear_reg(&"rsi".to_string());
                    self.clear_reg(&"rdi".to_string());
                    self.clear_reg(&"rcx".to_string());
                }
                ir::Op::Let { term, res } => {
                    let r = self.eval_term(term, Some(res.clone()), true)?;
                    self.save_reg(&r, &res);
                    self.lock_reg(&r, false);
                    self.store_term(res, r);
                }
                ir::Op::Decl { term, size } => {
                    self.sp += size as i64;
                    self.locs.insert(term, self.sp);
                    for _ in (0..size).step_by(8) {
                        // self.buf.push_line(format!("sub rsp, {}", size));
                        self.buf.push_line("push 0");
                    }
                }
                ir::Op::Own { res, term, offset } => {
                    let loc = self.locs.get(&term).unwrap();
                    self.locs.insert(res, loc - offset);
                }
                ir::Op::Arg { term, size } => {
                    if self.cur_fn == "main" {
                        if let Term::Pointer(_) = term {
                            self.buf.push_line(format!("push rsp"));
                            self.buf.push_line(format!("add qword [rsp], {}", self.sp + self.arg_index as i64 + 32));
                            self.sp += 8;
                            self.locs.insert(term, self.sp);
                        } else {
                            self.locs.insert(term, -(self.arg_index as i64) - 32);
                        }
                        self.arg_index += crate::types::Type::align(size) as usize;
                        continue;
                    }
                    let reg = *self.call_order.get(self.arg_index).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                    self.arg_index += 1;
                    if term.is_stack() {
                        if size > 8 && matches!(term, Term::Stack(_)) {
                            self.sp += 16;
                            self.buf.push_line(format!("sub rsp, 16"));
                            self.locs.insert(term, self.sp);
                            self.buf.push_line(format!("mov [rsp], {}", reg));
                            let reg = *self.call_order.get(self.arg_index).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                            self.arg_index += 1;
                            self.buf.push_line(format!("mov [rsp+8], {}", reg));
                        } else {
                            self.save_reg(&reg.to_string(), &term);
                            self.store_term(term, reg.to_string());
                            self.lock_reg(&reg.to_string(), false);
                        }
                    } else {
                        self.save_reg(&reg.to_string(), &term);
                        self.lock_reg(&reg.to_string(), true);
                    }
                }
                ir::Op::BeginCall { params: args } => {
                    let mut saved = Vec::new();
                    for r in self.regs.iter_mut() {
                        // Save registers
                        if r.locked {
                            saved.push(r.clone());
                            self.buf.push_line(format!("push {}", r.name));
                            self.buf.comment(format!("save {:?}", r.term.clone().unwrap()));
                            // println!("save {:?} ({})", r.term.clone().unwrap(), r.name);
                            self.sp += 8;
                        }
                        r.term = None;
                        r.locked = false;
                    }
                    self.saved_states.push(ProgState {
                        regs: saved,
                        param_index: self.param_index,
                        sp: self.sp,
                    });
                    self.params.push(args);
                    self.param_index = 0;
                }
                ir::Op::Param { term } => {
                    let target = *self.call_order.get(self.param_index).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                    if let Some((t1, t2)) = self.doubles.get(&term).cloned() {
                        self.param_index += 1;
                        self.swap_regs(t1, target.to_string());
                        self.lock_reg(&target.to_string(), true);
                        let target = *self.call_order.get(self.param_index).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                        self.param_index += 1;
                        self.swap_regs(t2, target.to_string());
                        self.lock_reg(&target.to_string(), true);
                    } else {
                        self.param_index += 1;
                        self.force_term_at(&term, &target.to_string())?;
                        self.lock_reg(&target.to_string(), true);
                    }
                    self.buf.comment(format!("param {:?}", term));
                }
                ir::Op::Call { func, res } => {
                    if let Some(Symbol::Syscall { id, .. }) = self.symbol_table.get(&func) {
                        self.force_term_at(&Term::IntLit(*id), &"rax".to_string())?;
                        if self.get_reg(&"rcx".to_string()).unwrap().locked {
                            self.swap_regs("rcx".to_string(), "r10".to_string());
                        }
                        self.buf.push_line("syscall");
                    } else {
                        self.buf.push_line(format!("call {}", func));
                    }
                    self.buf.comment(format!("{:?}", op_clone));
                    if let Some(r) = res {
                        if r.is_stack() {
                            self.store_term(r.clone(), "rax".to_string());
                        } else {
                            self.lock_reg(&"rax".to_string(), true);
                        }
                        self.save_reg(&"rax".to_string(), &r);
                        if let Term::Double(_) = r {
                            self.doubles.insert(r.clone(), ("rax".to_string(), "rdx".to_string()));
                            self.save_reg(&"rdx".to_string(), &r);
                            self.lock_reg(&"rdx".to_string(), true);
                        }
                    }
                    for name in self.call_order.clone() {
                        self.clear_reg(&name.to_string());
                    }
                    // Restore
                    let state = self.saved_states.pop().unwrap();
                    self.params.pop();
                    self.param_index = state.param_index;
                    if self.sp != state.sp {
                        self.buf.push_line(format!("add rsp, {}", self.sp - state.sp));
                        self.buf.comment(format!("restore sp (call)"));
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
                        // println!("restore {:?} into {} lock {:?}", r.term.clone().unwrap(), r.name, self.get_reg(&r.name));
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
                ir::Op::Jump { label } => {
                    self.buf.push_line(format!("jmp .L{}", label));
                }
                ir::Op::CondJump { cond, label } => {
                    let r = self.eval_term(cond, None, false)?;
                    self.restore_sp(0);
                    self.buf.push_line(format!("test {}, {}", r, r));
                    self.clear_reg(&r);
                    self.buf.push_line(format!("jnz .L{}", label));
                }
                ir::Op::BinJump { lhs, rhs, op, label } => {
                    let r1 = self.eval_term(lhs, None, false)?;
                    let r2 = self.eval_term(rhs, None, true)?;
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
                    self.restore_sp(0);
                    self.buf.push_line(format!("{} .L{}", jmp_instr, label));
                    self.clear_reg(&r1);
                    self.clear_reg(&r2);
                }
                ir::Op::Return { term } => {
                    if let Some(t) = term {
                        if let Some((t1, t2)) = self.doubles.get(&t).cloned() {
                            self.swap_regs("rax".to_string(), t1);
                            self.swap_regs("rdx".to_string(), t2);
                        } else {
                            self.force_term_at(&t, &"rax".to_string())?;
                        }
                    } else {
                        self.force_term_at(&Term::IntLit(0), &"rax".to_string())?;
                    }
                    if self.sp > 0 {
                        self.buf.push_line(format!("add rsp, {}", self.sp));
                    }
                    self.buf.push_line("ret");
                    self.clear_reg(&"rax".to_string());
                    self.clear_reg(&"rdx".to_string());
                }
                ir::Op::StackPointer { res } => {
                    let t = self.get_free_reg()?;
                    self.buf.push_line(format!("mov {t}, rsp"));
                    self.save_reg(&t, &res);
                    self.lock_reg(&t, true);
                }
                ir::Op::NaturalFlow => (),
                ir::Op::BeginLoop => {
                    for r in self.regs.iter_mut() {
                        if !r.locked {
                            r.term = None;
                        }
                    }
                    self.reg_states.push(self.regs.to_vec());
                }
                ir::Op::EndLoop => self.restore_regs(),
                ir::Op::BeginScope => self.saved_sps.push(self.sp),
                ir::Op::BeginScopeOffset(i) => {
                    self.saved_sps.push(self.sp);
                    self.sp += i;
                }
                ir::Op::EndScope => self.restore_sp(0),
                ir::Op::BreakScope { depth } => self.restore_sp(depth),
            }
            match op_clone {
                ir::Op::BeginLoop | ir::Op::EndLoop | ir::Op::Param { .. } | ir::Op::Call { .. } | ir::Op::BeginCall { .. } | ir::Op::BeginScope | ir::Op::EndScope | ir::Op::NaturalFlow => (),
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

    fn clear_reg(&mut self, reg: &String) {
        self.get_reg(reg).map(|r| {
            r.term = None;
            r.locked = false;
        });
    }

    fn restore_regs(&mut self) {
        let state = self.reg_states.pop().unwrap();
        let binding = self.regs.clone();
        for r in binding.iter().zip(state.iter()) {
            if r.0 != r.1 {
                if let Some(t) = &r.1.term {
                    if let Some(Term::Temp(_)) = r.1.term {
                        continue;
                    }
                    self.eval_term_at(t, &r.1.name.clone());
                    self.save_reg(&r.1.name.clone(), t);
                    self.lock_reg(&r.1.name.clone(), r.1.locked);
                } else {
                    self.clear_reg(&r.1.name);
                }
            }
            self.clear_reg(&r.1.name);
        }
    }
    fn restore_sp(&mut self, depth: usize) {
        let opt_sp;
        if depth == 0 {
            opt_sp = self.saved_sps.pop()
        } else {
            let i = self.saved_sps.len() - depth;
            let diff = self.sp - self.saved_sps.get(i).cloned().unwrap();
            // if diff == 24 {
            //     panic!("{:?}, {}, {}", self.saved_sps, i, diff);
            // }
            opt_sp = self.saved_sps.get(i).cloned()
        }
        if let Some(sp) = opt_sp {
            if self.sp != sp {
                if self.buf.last_line != "ret" {
                    self.buf.push_line(format!("add rsp, {}", self.sp - sp));
                    self.buf.comment(format!("restore sp"));
                }
                if depth == 0 {
                    self.sp = sp;
                }
            }
        } else {
            panic!("no sp {depth}");
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
                self.buf.push(format!("mov qword [rsp+{}], ", self.sp - res_loc));
            }
        } else {
            self.sp += 8;
            res_loc = self.sp;
            self.locs.insert(term.clone(), res_loc);
            self.buf.push("push ");
        }
        if let Ok(val) = reg.parse::<i64>() {
            if val < i32::MIN as i64 || val > i32::MAX as i64 {
                let bytes = (val as u64).to_le_bytes();
                let low = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
                let high = u32::from_le_bytes(bytes[4..8].try_into().unwrap());
                self.buf.push_line(format!("{}", low as i32));
                self.buf.push_line(format!("mov dword [rsp+4], {}", high as i32));
                return;
            }
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
        let t;
        if let Term::PointerArithmetic(p) = term {
            t = Term::Pointer(*p);
        } else {
            t = term.clone();
        }
        for r in self.regs.iter_mut() {
            if r.term == Some(t.clone()) {
                if r.name != *target {
                    self.buf.push_line(format!("mov {}, {}", target, r.name));
                    r.term = None;
                    r.locked = false;
                }
                return;
            }
        }
        if let Some(loc) = self.locs.get(&t) {
            self.buf.push(format!("mov {}, [rsp", target));
            if self.sp != *loc {
                self.buf.push("+");
                self.buf.push((self.sp - loc).to_string());
            }
            self.buf.push_line("]");
            return;
        }
        if let Term::Data(s) = t {
            if let Some(Symbol::Data { .. }) = self.symbol_table.get(&s) {
                self.buf.push_line(format!("mov {}, {}", target, s));
                return;
            }
        }
        panic!("Couldn't find {:?}", term);
    }

    fn eval_term(&mut self, term: Term, res: Option<Term>, allow_literals: bool) -> Result<String, GenError> {
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
        if let Some(r) = res {
            if let Some(p) = self.params.last().cloned() {
                for (i, t) in p.iter().enumerate() {
                    if *t == r {
                        let t = *self.call_order.get(i).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                        let target = t.to_string();
                        self.force_term_at(&term, &target)?;
                        self.save_reg(&target, &term);
                        return Ok(target);
                    }
                }
            }
        }
        let target = self.get_free_reg()?;
        //println!("eval {:?}, {}, {:?}", term, target, self.regs);
        self.eval_term_at(&term, &target);
        self.save_reg(&target, &term);
        Ok(target)
    }

    fn force_term_at(&mut self, term: &Term, target: &String) -> Result<(), GenError> {
        // println!("force {:?} at {}", term, target);
        let mut free = None;
        if let Some(r) = self.get_reg(&target.to_string()) {
            if let Some(t) = &r.term {
                if term != t {
                    if r.locked {
                        free = Some((t.clone(), self.get_free_reg()?));
                    }
                } else {
                    return Ok(());
                }
            }
        }
        self.clear_reg(target);
        if let Some(f) = free {
            // println!("move {}, {}", f.1, target);
            self.buf.push_line(format!("mov {}, {}", f.1, target));
            self.save_reg(&f.1, &f.0);
            self.lock_reg(&f.1, true);
        }
        self.eval_term_at(&term, &target.to_string());
        self.save_reg(target, term);
        Ok(())
    }

    fn free_reg(&mut self, reg: &String) -> Result<String, GenError> {
        // println!("free {}", reg);
        let mut free = None;
        if let Some(r) = self.get_reg(&reg.to_string()) {
            if let Some(t) = &r.term {
                let binding = t.clone();
                if r.locked {
                    free = Some((self.get_free_reg()?, binding));
                }
            }
        }
        self.clear_reg(&reg);
        if let Some(f) = free {
            self.buf.push_line(format!("mov {}, {}", f.0, reg));
            self.save_reg(&f.0, &f.1);
            self.lock_reg(&f.0, true);
            return Ok(f.0);
        }
        Ok("".to_string())
    }

    fn binop(&mut self, lhs: Term, rhs: Term, op: String, res: Option<Term>, size: u32) -> Result<(), GenError> {
        //println!("tac {:?} {:?} {:?}={:?}", lhs, op_opt, rhs_opt, res);
        let mut r1;
        let mut r2;
        if ["*", "/", "%"].contains(&op.as_str()) {
            r1 = "rax".to_string();
            self.force_term_at(&lhs, &"rax".to_string())?;
            r2 = self.eval_term(rhs.clone(), None, false)?;
        } else {
            r1 = self.eval_term(lhs.clone(), res.clone(), false)?;
            r2 = self.eval_term(rhs.clone(), None, true)?;
        }
        self.lock_reg(&r1, true);
        self.lock_reg(&r2, true);
        match op.as_str() {
            "+" => self.bin("add", &r1, &r2, size),
            "-" => self.bin("sub", &r1, &r2, size),
            "*" => self.bin_rax("mul", &mut r1, &mut r2, size)?,
            "/" => self.bin_rax("div", &mut r1, &mut r2, size)?,
            "%" => {
                self.bin_rax("div", &mut r1, &mut r2, size)?;
                r1 = "rdx".to_string();
            }
            ">" => self.cond("setg", &r1, &r2, size),
            "<" => self.cond("setl", &r1, &r2, size),
            ">=" => self.cond("setge", &r1, &r2, size),
            "<=" => self.cond("setle", &r1, &r2, size),
            "==" => self.cond("sete", &r1, &r2, size),
            "!=" => self.cond("setne", &r1, &r2, size),
            "|" => self.bin("or", &r1, &r2, size),
            "&" => self.bin("and", &r1, &r2, size),
            "^" => self.bin("xor", &r1, &r2, size),
            ">>" => self.bin_cl("shr", &mut r1, &mut r2, size),
            "<<" => self.bin_cl("shl", &mut r1, &mut r2, size),
            _ => panic!("Unsupported operator {op}"),
        }
        self.save_reg(&r1, &lhs);
        self.clear_reg(&r2);
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

    fn unop(&mut self, term: Term, op: String, res: Term, size: u32) -> Result<(), GenError> {
        match op.as_str() {
            "&" => {
                let loc = self.locs.get(&term).expect(format!("{:?}\n{:?}\nCouldn't find {:?}", self.cur_fn, self.cur_block, term).as_str());
                let target = self.get_free_reg()?;
                self.buf.push_line(format!("mov {}, rsp", target));
                if self.sp != *loc {
                    self.buf.push_line(format!("add {}, {}", target, self.sp - loc));
                }
                self.save_reg(&target, &res);
                self.lock_reg(&target, true);
                return Ok(());
            }
            _ => (),
        }
        let r = self.eval_term(term, Some(res.clone()), false)?;
        let d = Self::reg_name(&r, size);
        match op.as_str() {
            "-" => self.buf.push_line(format!("neg {}", d)),
            "*" => {
                self.buf.push_line(format!("mov {}, [{}]", d, r));
                if d != r {
                    self.buf.push_line(format!("movzx {}, {}", r, d));
                }
            }
            "!" => {
                self.buf.push_line(format!("test {}, {}", d, d));
                let res = Self::reg_name(&r, 1);
                self.buf.push_line(format!("sete {}", res));
                if d != res {
                    self.buf.push_line(format!("movzx {}, {}", d, res));
                }
            }
            "~" => self.buf.push_line(format!("not {}", d)),
            "0extend" => self.buf.push_line(format!("movzx {}, {}", r, d)),
            _ => unreachable!("Unknown unary op '{}'.", op),
        }
        self.save_reg(&r, &res);
        self.lock_reg(&r, true);
        // println!("SAVE {res:?} {r}");
        Ok(())
    }

    fn fmt_offset(&self, offset: i64) -> String {
        if offset == 0 {
            String::new()
        } else if offset > 0 {
            format!("+{}", offset)
        } else {
            format!("-{}", -offset)
        }
    }

    fn store(&mut self, term: Term, op: String, ptr: Term, mut offset: i64, res: Option<Term>, size: u32) -> Result<(), GenError> {
        let p;
        if let Some(t) = ptr.stack_arithmetic() {
            p = "rsp".to_string();
            offset += self.sp - self.locs.get(&t).unwrap();
        } else {
            p = self.eval_term(ptr.clone(), res.clone(), false)?;
        }
        let s = Self::word_size_name(size);
        if let Some((t1, t2)) = self.doubles.get(&term).cloned() {
            self.buf.push_line(format!("mov {s} [{}{}], {}", p, self.fmt_offset(offset), t1));
            self.buf.push_line(format!("mov {s} [{}{}], {}", p, self.fmt_offset(offset + 8), t2));
            self.clear_reg(&t1);
            self.clear_reg(&t2);
            return Ok(());
        }
        let mut t;
        if ["*=", "/=", "%="].contains(&op.as_str()) {
            t = self.eval_term(term.clone(), None, false)?;
        } else {
            t = self.eval_term(term.clone(), None, res.is_none())?;
        }
        self.save_reg(&t, &term);
        self.lock_reg(&t, true);
        let display_term = Self::reg_name(&t, size);
        let o = self.fmt_offset(offset);
        match op.as_str() {
            "=" => self.buf.push_line(format!("mov {s} [{}{}], {}", p, o, display_term)),
            "+=" => self.buf.push_line(format!("add {s} [{}{}], {}", p, o, display_term)),
            "-=" => self.buf.push_line(format!("sub {s} [{}{}], {}", p, o, display_term)),
            "|=" => self.buf.push_line(format!("or {s} [{}{}], {}", p, o, display_term)),
            "^=" => self.buf.push_line(format!("xor {s} [{}{}], {}", p, o, display_term)),
            "&=" => self.buf.push_line(format!("and {s} [{}{}], {}", p, o, display_term)),
            "*=" => {
                let mut free = "rax".to_string();
                if p != "rax" {
                    self.free_reg(&free)?;
                }
                if t == "rax" {
                    t = self.eval_term(term.clone(), None, false)?;
                }
                self.buf.push_line(format!("mov {}, [{}{}]", free, p, o));
                self.save_reg(&free, &term);
                self.lock_reg(&free, true);
                self.bin_rax("mul", &mut free, &mut t, size)?;
                self.clear_reg(&free);
                self.buf.push_line(format!("mov {s} [{}{}], {}", p, o, free));
            }
            "/=" => {
                let mut free = "rax".to_string();
                if p != "rax" {
                    self.free_reg(&free)?;
                }
                if t == "rax" {
                    t = self.eval_term(term.clone(), None, false)?;
                }
                self.buf.push_line(format!("mov {}, [{}{}]", free, p, o));
                self.save_reg(&free, &term);
                self.lock_reg(&free, true);
                self.bin_rax("div", &mut free, &mut t, size)?;
                self.clear_reg(&free);
                self.buf.push_line(format!("mov {s} [{}{}], {}", p, o, free));
            }
            "%=" => {
                let mut free = "rax".to_string();
                if p != "rax" {
                    self.free_reg(&free)?;
                }
                if t == "rax" {
                    t = self.eval_term(term.clone(), None, false)?;
                }
                self.buf.push_line(format!("mov {}, [{}{}]", free, p, o));
                self.save_reg(&free, &term);
                self.lock_reg(&free, true);
                self.bin_rax("div", &mut free, &mut t, size)?;
                self.buf.push("mov rax, rdx");
                self.clear_reg(&free);
                self.buf.push_line(format!("mov {s} [{}{}], {}", p, o, free));
            }
            ">>=" => self.bin_cl(format!("shr {s}").as_str(), &mut format!("[{}{}]", p, o), &mut t, size),
            "<<=" => self.bin_cl(format!("shl {s}").as_str(), &mut format!("[{}{}]", p, o), &mut t, size),
            _ => unreachable!(),
        }
        if let Some(r) = res {
            self.save_reg(&t, &r);
            self.lock_reg(&t, true);
        } else {
            self.clear_reg(&t);
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

    fn read(&mut self, ptr: Term, mut offset: i64, res: Term, size: u32) -> Result<(), GenError> {
        let r;
        if let Some(t) = ptr.stack_arithmetic() {
            r = "rsp".to_string();
            offset += self.sp - self.locs.get(&t).unwrap();
        } else if let Term::Double(_) = ptr {
            let (t1, t2) = self.doubles.get(&ptr).unwrap();
            if offset < 8 {
                r = t1.to_string();
            } else {
                r = t2.to_string();
            }
            if let Term::Temp(_) = res {
                self.save_reg(&r, &res);
                self.lock_reg(&r, true);
                return Ok(());
            }
        } else {
            r = self.eval_term(ptr.clone(), Some(res.clone()), false)?
        }
        if let Term::Double(_) = res {
            let t1 = self.get_free_reg()?;
            self.buf.push_line(format!("mov {}, [{}{}]", t1, r, self.fmt_offset(offset)));
            self.save_reg(&t1, &res);
            self.lock_reg(&t1, true);
            let t2 = self.get_free_reg()?;
            self.buf.push_line(format!("mov {}, [{}{}]", t2, r, self.fmt_offset(offset + 8)));
            self.save_reg(&t2, &res);
            self.lock_reg(&t2, true);
            self.doubles.insert(res, (t1, t2));
            self.clear_reg(&r);
            return Ok(());
        }
        let o = self.fmt_offset(offset);
        if let Some(_) = ptr.stack_arithmetic() {
            let mut res_reg = self.get_free_reg()?;
            if let Some(p) = self.params.last().cloned() {
                for (i, t) in p.iter().enumerate() {
                    if *t == res {
                        let t = *self.call_order.get(i).ok_or(GenError::TooManyArguments(self.last_loc.clone()))?;
                        res_reg = t.to_string();
                        self.free_reg(&res_reg)?;
                        break;
                    }
                }
            }
            let d = Self::reg_name(&res_reg, size);
            self.buf.push_line(format!("mov {}, [{}{}]", d, r, o));
            if d != res_reg {
                self.buf.push_line(format!("movzx {}, {}", res_reg, d));
            }
            self.save_reg(&res_reg, &res);
            self.lock_reg(&res_reg, true);
        } else {
            let d = Self::reg_name(&r, size);
            self.buf.push_line(format!("mov {}, [{}{}]", d, r, o));
            if d != r {
                self.buf.push_line(format!("movzx {}, {}", r, d));
            }
            self.save_reg(&r, &res);
            self.lock_reg(&r, true);
        }
        Ok(())
    }

    fn bin(&mut self, op: &str, r1: &String, r2: &String, size: u32) {
        self.buf.push_line(format!("{} {}, {}", op, Self::reg_name(r1, size), Self::reg_name(r2, size)));
    }

    fn bin_rax(&mut self, op: &str, r1: &mut String, r2: &mut String, size: u32) -> Result<(), GenError> {
        self.swap_regs(r1.to_string(), "rax".to_string());
        if r2 == "rax" {
            *r2 = r1.to_string();
        } else if r2 == r1 {
            *r2 = "rax".to_string();
        }
        *r1 = "rax".to_string();
        let moved = self.free_reg(&"rdx".to_string())?;
        if r2 == "rdx" {
            *r2 = moved;
        }
        if op == "div" {
            self.buf.push_line("xor rdx, rdx");
        }
        self.buf.push_line(format!("{} {}", op, Self::reg_name(r2, size)));
        Ok(())
    }
    fn bin_cl(&mut self, op: &str, r1: &mut String, r2: &mut String, size: u32) {
        if r2.parse::<i64>().is_ok() {
            self.buf.push_line(format!("{} {}, {}", op, Self::reg_name(r1, size), r2));
            return;
        }
        self.swap_regs(r2.to_string(), "rcx".to_string());
        if r1 == "rcx" {
            *r1 = r2.to_string();
        } else if r2 == r1 {
            *r1 = "rcx".to_string();
        }
        *r2 = "rcx".to_string();
        self.buf.push_line(format!("{} {}, {}", op, Self::reg_name(r1, size), Self::reg_name(r2, 1)));
    }

    fn cond(&mut self, cond: &str, r1: &String, r2: &String, size: u32) {
        // if r2 != "0" {
        //     self.buf.push_line(format!("sub {}, {}", Self::reg_name(r1, size), Self::reg_name(r2, size)));
        // }
        self.buf.push_line(format!("cmp {}, {}", Self::reg_name(r1, size), Self::reg_name(r2, size)));
        self.buf.push_line(format!("{} {}", cond, Self::reg_name(r1, 1)));
        self.buf.push_line(format!("movzx {}, {}", Self::reg_name(r1, 8), Self::reg_name(r1, 1)));
    }

    fn word_size_name(size: u32) -> &'static str {
        match size {
            1 => "byte",
            2 => "word",
            3 | 4 => "dword",
            _ => "qword",
        }
    }
    fn reg_name(base: &str, size: u32) -> String {
        let r;
        match size {
            1 => match base {
                "rax" => "al",
                "rbx" => "bl",
                "rcx" => "cl",
                "rdx" => "dl",
                "rsi" => "sil",
                "rdi" => "dil",
                "rbp" => "bpl",
                "rsp" => "spl",
                _ if base.starts_with("r") => {
                    r = format!("{}b", base);
                    r.as_str()
                }
                _ => base,
            }
            .to_string(),
            2 => match base {
                "rax" => "ax",
                "rbx" => "bx",
                "rcx" => "cx",
                "rdx" => "dx",
                "rsi" => "si",
                "rdi" => "di",
                "rbp" => "bp",
                "rsp" => "sp",
                _ if base.starts_with("r") => {
                    r = format!("{}w", base);
                    r.as_str()
                }
                _ => base,
            }
            .to_string(),
            3 | 4 => match base {
                "rax" => "eax",
                "rbx" => "ebx",
                "rcx" => "ecx",
                "rdx" => "edx",
                "rsi" => "esi",
                "rdi" => "edi",
                "rbp" => "ebp",
                "rsp" => "esp",
                _ if base.starts_with("r") => {
                    r = format!("{}d", base);
                    r.as_str()
                }
                _ => base,
            }
            .to_string(),
            _ => base.to_string(),
        }
    }
}
