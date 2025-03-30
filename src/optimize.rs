use crate::ir::{Block, Op, Symbol, Term};
use std::{
    collections::{HashMap, HashSet},
    usize,
};

/// Optimize the IR
pub fn optimize(ir: &mut HashMap<String, Symbol>) {
    let mut opt = Opt::new(ir);
    opt.all();
}

struct Opt<'a> {
    symbol_table: &'a mut HashMap<String, Symbol>,
}

impl<'a> Opt<'a> {
    pub fn new(ir: &'a mut HashMap<String, Symbol>) -> Self {
        Self { symbol_table: ir }
    }
    fn all(&mut self) {
        for sym in self.symbol_table.values_mut() {
            if let Symbol::Func { ty: _, block, symbols: _ } = sym {
                *block = Self::block(block.clone());
            }
        }
    }

    fn block(block: Block) -> Block {
        // First pass - optimize data flow, build the unused table and label_no_replace table
        let mut replace_table: HashMap<Term, Term> = HashMap::new();
        let mut unused_table: HashSet<Term> = HashSet::new();
        let mut used_labels: HashSet<u16> = HashSet::new();
        let mut label_no_replace: HashSet<u16> = HashSet::new();
        let mut new_block = Block::new();
        let mut iter = block.ops.into_iter().peekable();
        let mut loc_iter = block.locs.into_iter();
        let mut expr_start = usize::MAX;

        while let Some(op) = iter.next() {
            let mut last_loc = loc_iter.next().unwrap();
            match op {
                Op::Tac { lhs, rhs, op, res } => {
                    if let Some(Term::Symbol(_)) = res {
                        expr_start = last_loc.start_id;
                    }
                    if expr_start == usize::MAX {
                        expr_start = last_loc.start_id;
                    }
                    //last_loc.start_id = expr_start;
                    let mut new_lhs = lhs.clone();
                    let mut new_rhs = rhs.clone();
                    let mut new_res = res.clone();
                    // Apply replace table
                    if let Some(t) = replace_table.get(&lhs) {
                        new_lhs = t.clone();
                    }
                    if let Some(r) = rhs.clone() {
                        if let Some(t) = replace_table.get(&r) {
                            new_rhs = Some(t.clone());
                        }
                    }

                    // Optimize temp variables
                    if let Some(Term::Temp(_)) = res {
                        let r = res.unwrap();
                        // Inline temp = single_term expressions
                        if rhs.is_none() {
                            replace_table.insert(r, lhs);
                            continue;
                        }
                        unused_table.insert(r.clone());
                        // Inline temp = expr, term = temp expressions
                        if let Some(next_op) = iter.peek() {
                            if let Op::Tac {
                                lhs: n_lhs,
                                rhs: n_rhs,
                                op: _,
                                res: n_res,
                            } = next_op
                            {
                                if *n_lhs == r && n_rhs.is_none() {
                                    unused_table.remove(&new_lhs);
                                    if let Some(r) = &new_rhs {
                                        unused_table.remove(r);
                                    }
                                    new_block.ops.push(Op::Tac {
                                        lhs: new_lhs,
                                        rhs: new_rhs,
                                        op,
                                        res: n_res.clone(),
                                    });
                                    if let Some(Term::Symbol(_)) = n_res {
                                        expr_start = usize::MAX;
                                    }
                                    iter.next();
                                    last_loc = loc_iter.next().unwrap();
                                    new_block.locs.push(last_loc);
                                    continue;
                                }
                            } else if let Op::CondJump { cond, label } = next_op {
                                if *cond == r {
                                    new_block.ops.push(Op::BinJump {
                                        lhs: new_lhs.clone(),
                                        rhs: new_rhs.clone().unwrap(),
                                        op: op.clone().unwrap(),
                                        label: *label,
                                    });
                                    used_labels.insert(*label);
                                    label_no_replace.insert(*label);
                                    iter.next();
                                    last_loc = loc_iter.next().unwrap();
                                    new_block.locs.push(last_loc);
                                    continue;
                                }
                            }
                        }
                        // Reuse temp variables
                        if let Term::Temp(_) = new_lhs {
                            new_res = Some(new_lhs.clone());
                            replace_table.insert(r, new_lhs.clone());
                        }
                    }
                    unused_table.remove(&new_lhs);
                    if let Some(r) = &new_rhs {
                        unused_table.remove(r);
                    }
                    new_block.ops.push(Op::Tac {
                        lhs: new_lhs,
                        rhs: new_rhs,
                        op,
                        res: new_res,
                    });
                    new_block.locs.push(last_loc);
                }
                Op::Unary { term, op, res } => {
                    unused_table.remove(&term);
                    unused_table.insert(res.clone());
                    new_block.locs.push(last_loc);
                    if let Some(new_term) = replace_table.get(&term) {
                        new_block.ops.push(Op::Unary { term: new_term.clone(), op, res });
                    } else {
                        new_block.ops.push(Op::Unary { term, op, res });
                    }
                }
                Op::DerefAssign { term, op, ptr, offset, res } => {
                    unused_table.remove(&term);
                    unused_table.remove(&ptr);
                    unused_table.insert(res.clone().unwrap());
                    new_block.locs.push(last_loc);
                    let mut new_term = term.clone();
                    let mut new_ptr = ptr.clone();
                    if let Some(n) = replace_table.get(&term) {
                        new_term = n.clone();
                    }
                    if let Some(n) = replace_table.get(&ptr) {
                        new_ptr = n.clone();
                    }
                    new_block.ops.push(Op::DerefAssign { term: new_term, op, ptr: new_ptr, offset, res });
                }
                Op::Return(term) => {
                    expr_start = usize::MAX;
                    unused_table.remove(&term);
                    new_block.locs.push(last_loc);
                    if let Some(new_term) = replace_table.get(&term) {
                        new_block.ops.push(Op::Return(new_term.clone()));
                    } else {
                        new_block.ops.push(Op::Return(term));
                    }
                    while let Some(Op::Jump(_)) = iter.peek() {
                        iter.next();
                        loc_iter.next();
                    }
                }
                Op::Jump(label) => {
                    expr_start = usize::MAX;
                    new_block.ops.push(op);
                    new_block.locs.push(last_loc);
                    used_labels.insert(label);
                }
                Op::CondJump { cond, label } => {
                    expr_start = usize::MAX;
                    unused_table.remove(&cond);
                    used_labels.insert(label);
                    label_no_replace.insert(label);
                    new_block.locs.push(last_loc);
                    if let Some(new_term) = replace_table.get(&cond) {
                        new_block.ops.push(Op::CondJump { cond: new_term.clone(), label });
                    } else {
                        new_block.ops.push(Op::CondJump { cond, label });
                    }
                }
                Op::BinJump { lhs, rhs, op, label } => {
                    expr_start = usize::MAX;
                    unused_table.remove(&lhs);
                    unused_table.remove(&rhs);
                    used_labels.insert(label);
                    label_no_replace.insert(label);
                    new_block.locs.push(last_loc);
                    let mut new_lhs = lhs.clone();
                    let mut new_rhs = rhs.clone();
                    if let Some(l) = replace_table.get(&lhs) {
                        new_lhs = l.clone();
                    }
                    if let Some(r) = replace_table.get(&rhs) {
                        new_rhs = r.clone();
                    }
                    new_block.ops.push(Op::BinJump {
                        lhs: new_lhs,
                        rhs: new_rhs,
                        op,
                        label,
                    });
                }
                Op::Param(term) => {
                    expr_start = usize::MAX;
                    unused_table.remove(&term);
                    new_block.locs.push(last_loc);
                    if let Some(new_term) = replace_table.get(&term) {
                        new_block.ops.push(Op::Param(new_term.clone()));
                    } else {
                        new_block.ops.push(Op::Param(term));
                    }
                }
                Op::TakeSalloc { ptr, res } => {
                    expr_start = usize::MAX;
                    if let Some(next_op) = iter.peek() {
                        if let Op::Tac {
                            lhs: n_lhs,
                            rhs: n_rhs,
                            op: _,
                            res: n_res,
                        } = next_op {
                            if *n_lhs == res && n_res.is_some() && n_rhs.is_none() {
                                new_block.ops.push(Op::TakeSalloc { ptr: ptr, res: n_res.clone().unwrap() });
                                new_block.locs.push(last_loc);
                                iter.next();
                                loc_iter.next();
                                continue;
                            }
                        }
                    }
                    new_block.ops.push(Op::TakeSalloc { ptr, res });
                    new_block.locs.push(last_loc);
                }
                _ => {
                    expr_start = usize::MAX;
                    new_block.ops.push(op);
                    new_block.locs.push(last_loc);
                }
            }
        }
        println!("unused {:?}", unused_table);

        // Second pass - apply unused table
        let mut new_block_2 = Block::new();
        let mut iter_2 = new_block.ops.into_iter().peekable();
        let mut loc_iter_2 = new_block.locs.into_iter().peekable();

        while let Some(op) = iter_2.next() {
            match op {
                Op::Tac { lhs, rhs, op, res } => {
                    let mut r = res.clone();
                    if unused_table.contains(&res.clone().unwrap()) {
                        r = None;
                    }
                    if let Some(o) = &op {
                        if r == Some(lhs.clone()) && o.contains("=") {
                            r = None;
                        }
                    }
                    new_block_2.ops.push(Op::Tac { lhs, rhs, op, res: r });
                    new_block_2.locs.push(loc_iter_2.next().unwrap());
                }
                Op::DerefAssign { term, op, ptr, offset, res } => {
                    let mut r = res.clone();
                    if unused_table.contains(&res.clone().unwrap()) {
                        r = None;
                    }
                    new_block_2.ops.push(Op::DerefAssign { term, op, ptr, offset, res: r }) ;
                    new_block_2.locs.push(loc_iter_2.next().unwrap());
                }
                _ => {
                    new_block_2.ops.push(op);
                    new_block_2.locs.push(loc_iter_2.next().unwrap());
                }
            }
        }

        // Third pass - optimize pointer instructions
        let mut new_block_3 = Block::new();
        let mut iter_3 = new_block_2.ops.into_iter().peekable();
        let mut loc_iter_3 = new_block_2.locs.into_iter().peekable();

        while let Some(op) = iter_3.next() {
            match op {
                Op::Unary { term: ptr, op, res } => {
                    if op == "&" {
                        let mut iter_clone = iter_3.clone();
                        if let Some(Op::Tac { lhs, rhs, op, res }) = iter_clone.next() {
                            if let Some(Term::IntLit(offset)) = rhs {
                                let next_op = iter_clone.next();
                                if let Some(Op::DerefAssign { term, op, ptr: _, offset: _, res }) = next_op {
                                    new_block_3.ops.push(Op::StackAssign { term, op, ptr, offset: offset.parse::<i64>().unwrap(), res });
                                    new_block_3.locs.push(loc_iter_3.next().unwrap());
                                    for _ in 0..2 {
                                        iter_3.next();
                                        loc_iter_3.next();
                                    }
                                    continue;
                                }
                                if let Some(Op::Unary { term, op, res }) = next_op {
                                    if op == "*" {
                                        new_block_3.ops.push(Op::StackRead { ptr, offset: offset.parse::<i64>().unwrap(), res });
                                        new_block_3.locs.push(loc_iter_3.next().unwrap());
                                        for _ in 0..2 {
                                            iter_3.next();
                                            loc_iter_3.next();
                                        }
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                    new_block_3.ops.push(Op::Unary { term: ptr, op, res });
                    new_block_3.locs.push(loc_iter_3.next().unwrap());
                }
                _ => {
                    new_block_3.ops.push(op);
                    new_block_3.locs.push(loc_iter_3.next().unwrap());
                }
            }
        }

        // Fourth pass - build the label replace table
        let mut label_replace: HashMap<u16, (u16, Vec<Op>)> = HashMap::new();
        let mut new_block_4 = Block::new();
        let mut iter_4 = new_block_3.ops.into_iter().peekable();
        let mut loc_iter_4 = new_block_3.locs.into_iter().peekable();

        while let Some(op) = iter_4.next() {
            let last_loc = loc_iter_4.next().unwrap();
            match op {
                Op::Label(label) => {
                    if label_no_replace.contains(&label) || !used_labels.contains(&label) {
                        new_block_4.ops.push(op);
                        new_block_4.locs.push(last_loc);
                        continue;
                    }
                    if let Some(next_op) = iter_4.peek() {
                        if let Op::Jump(rep_label) = next_op {
                            label_replace.insert(label, (*rep_label, Vec::new()));
                            label_no_replace.insert(*rep_label);
                            iter_4.next();
                            loc_iter_4.next();
                            continue;
                        } else if let Op::Label(rep_label) = next_op {
                            label_replace.insert(label, (*rep_label, Vec::new()));
                            label_no_replace.insert(*rep_label);
                            continue;
                        } else if let Op::LoopStart = next_op {
                        } else {
                            let mut vec = vec![next_op.clone()];
                            let mut rep_label = 0;
                            let mut n_iter = iter_4.clone();
                            n_iter.next();
                            while let Some(n_op) = n_iter.next() {
                                if let Op::Jump(r) = n_op {
                                    rep_label = r;
                                    break;
                                } else if let Op::Label(_) = n_op {
                                    break;
                                }
                                vec.push(n_op);
                            }
                            if rep_label != 0 {
                                for _ in 0..vec.len() {
                                    iter_4.next();
                                    loc_iter_4.next();
                                }
                                label_replace.insert(label, (rep_label, vec));
                                label_no_replace.insert(rep_label);
                                continue;
                            }
                        }
                    }
                    new_block_4.ops.push(op);
                    new_block_4.locs.push(last_loc);
                }
                _ => {
                    new_block_4.ops.push(op);
                    new_block_4.locs.push(last_loc);
                }
            }
        }
        //label_replace.retain(|key, _| !label_no_replace.contains(key));
        //Self::remove_circular_references(&mut label_replace);
        println!("repl {:?}", label_replace);
        println!("used {:?}", used_labels);

        // Fifth pass - apply label replace table
        let mut new_block_5 = Block::new();
        let mut iter_5 = new_block_4.ops.into_iter().peekable();
        let mut loc_iter_5 = new_block_4.locs.into_iter().peekable();

        while let Some(op) = iter_5.next() {
            match op {
                Op::Jump(label) => {
                    let mut l = label;
                    while label_replace.contains_key(&l) {
                        let repl = label_replace.get(&l).unwrap();
                        for o in repl.1.iter() {
                            new_block_5.ops.push(o.clone());
                            new_block_5.locs.push(loc_iter_5.peek().clone().unwrap().clone());
                        }
                        l = repl.0;
                    }
                    new_block_5.ops.push(Op::Jump(l));
                    new_block_5.locs.push(loc_iter_5.next().unwrap());
                }
                Op::Label(label) => {
                    if !used_labels.contains(&label) {
                        println!("removing L{}", label);
                        while let Some(o) = iter_5.next() {
                            loc_iter_5.next();
                            match o {
                                Op::NaturalFlow => break,
                                Op::Label(_) => break,
                                Op::Jump(_) => break,
                                Op::Return(_) => break,
                                _ => ()
                            }
                        }
                    } else {
                        new_block_5.ops.push(op);
                        new_block_5.locs.push(loc_iter_5.next().unwrap());
                    }
                }
                Op::Return(_) => {
                    new_block_5.ops.push(op);
                    new_block_5.locs.push(loc_iter_5.next().unwrap());
                    while let Some(Op::Jump(_)) = iter_5.peek() {
                        iter_5.next();
                        loc_iter_5.next();
                    }
                }
                _ => {
                    new_block_5.ops.push(op);
                    new_block_5.locs.push(loc_iter_5.next().unwrap());
                }
            }
        }
        // Fix label and temp ordering, optional
        //Self::fix_order(&mut new_block_4);
        new_block_5
    }

    fn fix_order(block: &mut Block) {
        let mut replace_table: HashMap<usize, usize> = HashMap::new();
        let mut label_replace: HashMap<u16, u16> = HashMap::new();
        let mut temp_count = 0;
        let mut label_count = 0;

        for op in block.ops.iter() {
            match op {
                Op::Label(i) => {
                    label_count += 1;
                    if *i != label_count {
                        label_replace.insert(*i, label_count);
                    }
                }
                Op::Tac { lhs: _, rhs: _, op: _, res } | Op::DerefAssign { term: _, op: _, ptr: _, offset: _, res } => {
                    if let Some(Term::Temp(t)) = res {
                        temp_count += 1;
                        if *t != temp_count {
                            replace_table.insert(*t, temp_count);
                        }
                    }
                }
                Op::Unary { term: _, op: _, res } | Op::Call { func: _, res }  => {
                    if let Term::Temp(t) = res {
                        temp_count += 1;
                        if *t != temp_count {
                            replace_table.insert(*t, temp_count);
                        }
                    }
                }
                _ => {}
            }
        }

        for op in block.ops.iter_mut() {
            match op {
                Op::Tac { lhs, rhs, op: _, res } => {
                    if let Term::Temp(t) = lhs {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Some(Term::Temp(t)) = rhs {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Some(Term::Temp(t)) = res {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                }
                Op::Unary { term, op: _, res } => {
                    if let Term::Temp(t) = term {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Term::Temp(t) = res {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                }
                Op::DerefAssign { term, op: _, ptr, offset: _, res } => {
                    if let Term::Temp(t) = term {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Term::Temp(t) = ptr {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Some(Term::Temp(t)) = res {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                }
                Op::Arg(term) | Op::Param(term) | Op::Return(term) | Op::Call { func: _, res: term } => {
                    if let Term::Temp(t) = term {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                }
                Op::Label(l) | Op::Jump(l) => {
                    if let Some(i) = label_replace.get(l) {
                        *l = *i;
                    }
                }
                Op::CondJump { cond, label } => {
                    if let Some(i) = label_replace.get(label) {
                        *label = *i;
                    }
                    if let Term::Temp(t) = cond {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                }
                Op::BinJump { lhs, rhs, op: _, label } => {
                    if let Term::Temp(t) = lhs {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Term::Temp(t) = rhs {
                        if let Some(r) = replace_table.get(&t) {
                            *t = *r;
                        }
                    }
                    if let Some(i) = label_replace.get(label) {
                        *label = *i;
                    }
                }
                _ => (),
            }
        }
    }
}
