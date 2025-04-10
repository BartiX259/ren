use std::collections::HashSet;
use std::fs;
use std::env;
use std::path::Path;
use std::process::Command;
use std::process::ExitCode;

use node::Imports;
use node::Module;
use node::Root;

mod error;
mod gen;
mod helpers;
mod ir;
mod lower;
mod node;
// mod optimize;
mod parse;
mod process;
mod tokenize;
mod types;
mod validate;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];
    
    let mut import = node::Import { path: file_path.clone(), parent: None, pos_id: usize::MAX  };
    let mut modules = Vec::new();
    let mut cur_imports = Vec::new();
    let mut handled_imports = HashSet::new();

    loop {
        let (module, imports) = match parse_module(&import) {
            Ok(res) => res,
            Err(e) => return e
        };
        modules.push(node::Module { stmts: module, path: import.path });
        for i in imports {
            if !handled_imports.contains(&i.path) {
                handled_imports.insert(i.path.clone());
                cur_imports.push(i);
            }
        }
        if let Some(mut i) = cur_imports.pop() {
            let cur = i.parent.clone().unwrap();
            let parent = Path::new(&cur)
                .parent()
                .unwrap_or_else(|| Path::new("."));
            i.path = parent.join(i.path).with_extension("re").display().to_string().replace('\\', "/");
            import = i;
        } else {
            break;
        }
    }
    println!("{:?}", modules);
    
    let mut root = Root(modules);

    let mut ir = match validate::validate(&mut root) {
        Ok(res) => res,
        Err((module, e)) => {
            error::sematic_err(&module, e);
            return ExitCode::from(1);
        }
    };

    let processed_stmts = process::process(root);

    // println!("Processed statements:\n{:?}\n", processed_stmts);

    lower::lower(processed_stmts, &mut ir);

    println!("IR:\n{:?}\n", ir);

    // // optimize::optimize(&mut ir);

    // // println!("Optimized IR:\n{:?}\n", ir);

    let result = match gen::gen(&mut ir) {
        Ok(res) => res,
        Err((module, e)) => {
            error::gen_err(&module, e);
            return ExitCode::from(1);
        }
    };
    fs::write("out.S", result).expect("Unable to write file");

    println!("nasm: {:?}", Command::new("nasm").args(["-felf64", "out.S", "-o", "out.o"]).output());
    println!("nasm std: {:?}", Command::new("nasm").args(["-felf64", "lib/std.S", "-o", "std.o"]).output());
    println!(
        "ld:   {:?}",
        Command::new("ld")
            .args([
                "-o", "out",
                "out.o", "std.o" //, "-lc", "--dynamic-linker", "/lib64/ld-linux-x86-64.so.2"
            ])
            .output()
    );
    ExitCode::from(0)
}

fn parse_module(import: &node::Import) -> Result<(Vec<node::Stmt>, Vec<node::Import>), ExitCode> {
    let content = match fs::read_to_string(&import.path) {
        Ok(c) => c,
        Err(err) => {
            error::import_error(err, import);
            return Err(ExitCode::from(1));
        }
    };

    let (tokens, locs) = match tokenize::tokenize(&content) {
        Ok(res) => res,
        Err(e) => {
            error::token_err(&content, e);
            return Err(ExitCode::from(1));
        }
    };

    match parse::parse(tokens, Some(import.path.clone())) {
        Ok(res) => Ok(res),
        Err(e) => {
            error::parse_err(&import.path, e);
            Err(ExitCode::from(1))
        }
    }
}
