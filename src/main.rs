use std::fs;
use std::env;
use std::io;
use std::process::Command;

mod error;
mod gen;
mod helpers;
mod ir;
mod lower;
mod node;
mod optimize;
mod parse;
mod process;
mod tokenize;
mod types;
mod validate;

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];
    let content = fs::read_to_string(file_path)?;
    println!("{}", content);
    let token_res;
    match tokenize::tokenize(&content) {
        Ok(res) => token_res = res,
        Err(e) => {
            error::token_err(&content, e);
            return Ok(());
        }
    }
    println!("Tokens:\n{:?}\n", token_res.0);
    println!("Locs:\n{:?}\n", token_res.1);

    let mut stmts;
    match parse::parse(token_res.0) {
        Ok(res) => stmts = res,
        Err(e) => {
            error::parse_err(&content, e, token_res.1);
            return Ok(());
        }
    }
    println!("Statements:\n{:?}\n", stmts);

    let mut ir;
    match validate::validate(&mut stmts) {
        Ok(res) => ir = res,
        Err(e) => {
            error::sematic_err(&content, e, token_res.1);
            return Ok(());
        }
    }

    let processed_stmts = process::process(stmts);

    println!("Processed statements:\n{:?}\n", processed_stmts);

    lower::lower(processed_stmts, &mut ir);

    println!("IR:\n{:?}\n", ir);

    optimize::optimize(&mut ir);

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

    println!("nasm: {:?}", Command::new("nasm").args(["-felf64", "-g", "out.S", "-o", "out.o"]).output());
    println!("nasm std: {:?}", Command::new("nasm").args(["-felf64", "-g", "lib/std.S", "-o", "std.o"]).output());
    println!(
        "ld:   {:?}",
        Command::new("ld")
            .args([
                "-o", "out",
                "out.o", "std.o" //, "-lc", "--dynamic-linker", "/lib64/ld-linux-x86-64.so.2"
            ])
            .output()
    );
    Ok(())
}
