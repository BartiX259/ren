use std::fs;
use std::io;
use std::process::Command;

mod error;
mod gen;
mod helpers;
mod ir;
mod lower;
mod node;
mod opt;
mod parse;
mod token;
mod validate;

fn main() -> Result<(), io::Error> {
    let content: String = fs::read_to_string("./test.re")?;
    println!("{}", content);
    let token_res;
    match token::tokenize(&content) {
        Ok(res) => token_res = res,
        Err(e) => {
            error::token_err(&content, e);
            return Ok(());
        }
    }
    println!("Tokens:\n{:?}\n", token_res.0);
    println!("Locs:\n{:?}\n", token_res.1);

    let stmts;
    match parse::parse(token_res.0) {
        Ok(res) => stmts = res,
        Err(e) => {
            error::parse_err(&content, e, token_res.1);
            return Ok(());
        }
    }
    println!("Statements:\n{:?}\n", stmts);

    let mut ir;
    match validate::validate(&stmts) {
        Ok(res) => ir = res,
        Err(e) => {
            error::sematic_err(&content, e, token_res.1);
            return Ok(());
        }
    }

    lower::lower(&stmts, &mut ir);

    println!("IR:\n{:?}\n", ir);

    opt::optimize(&mut ir);

    println!("Optimized IR:\n{:?}\n", ir);

    let result;
    match gen::gen(&mut ir) {
        Ok(res) => result = res,
        Err(e) => {
            error::gen_err(&content, e, token_res.1);
            return Ok(());
        }
    }
    fs::write("out.S", result).expect("Unable to write file");

    println!("nasm: {:?}", Command::new("nasm").args(["-felf64", "out.S", "-o", "out.o"]).output());
    println!(
        "ld:   {:?}",
        Command::new("ld")
            .args([
                "-o", "out",
                "out.o" //, "-lc", "--dynamic-linker", "/lib64/ld-linux-x86-64.so.2"]).output());
            ])
            .output()
    );
    Ok(())
}
