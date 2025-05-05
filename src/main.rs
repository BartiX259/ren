use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::env;
use std::path::Path;
use std::process::Command;
use std::process::ExitCode;

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
    let mut import_names: Vec<Vec<String>> = Vec::new();
    let mut cur_imports = Vec::new();
    let mut handled_imports = HashSet::new();

    loop {
        let (module, imports) = match parse_module(&import) {
            Ok(res) => res,
            Err(e) => return e
        };
        import_names.push(imports.iter().map(|i| get_path(&import.path, &i.path)).collect());
        modules.push(node::Module { stmts: module, path: import.path });
        for i in imports {
            if !handled_imports.contains(&i.path) {
                handled_imports.insert(i.path.clone());
                cur_imports.push(i);
            }
        }
        if let Some(mut i) = cur_imports.pop() {
            let cur = i.parent.clone().unwrap();
            i.path = get_path(&cur, &i.path);
            import = i;
        } else {
            break;
        }
    }

    let mut public_map = HashMap::new();
    for module in modules.iter_mut() {
        match validate::hoist_public(module, &public_map) {
            Ok(res) => { 
                let binding = module.path.clone();
                public_map.insert(binding, res);
            }
            Err(e) => {
                error::sematic_err(&module.path, e);
                return ExitCode::from(1);
            }
        }
    }

    let mut files = Vec::new();
    let mut generic_calls = Vec::new();
    let mut unresolved = Vec::new();

    for (mut module, imports) in modules.into_iter().zip(import_names.into_iter()) {
        let mut pub_symbols = Vec::new();
        for path in imports {
            if let Some(symbols) = public_map.get(&path) {
                pub_symbols.push(symbols);
            }
        }
        let (syms, gfns) = match validate::validate(&mut module, pub_symbols, &mut generic_calls) {
            Ok(res) => res,
            Err(e) => {
                error::sematic_err(&module.path, e);
                return ExitCode::from(1);
            }
        };
        if gfns > 0 {
            unresolved.push((module, syms));
            continue;
        }

        let (m, ir) = validate::clean_up(module, syms);
        match gen_module(m, ir) {
            Ok(file) => files.push(file),
            Err(e) => return ExitCode::from(e),
        }
    }

    println!("UNRESOLVED {}", unresolved.len());
    println!("CALLS {:?}", generic_calls);

    let mut resolved_modules = Vec::new();

    loop {
        println!("Resolving with {} generic calls...", generic_calls.len());

        let drained: Vec<_> = unresolved.drain(..).collect();
        let before = generic_calls.clone();

        for (module, syms) in drained {
            let path = module.path.clone();
            match validate::resolve(module, syms, &mut generic_calls) {
                Ok((ir, resolved_module)) => {
                    // println!("{generic_calls:?}");
                    resolved_modules.push((resolved_module, ir));
                }
                Err(e) => {
                    error::sematic_err(&path, e);
                    return ExitCode::from(1);
                }
            }
        }

        if generic_calls.is_empty() {
            break;
        } else if before == generic_calls {
            panic!("No progress {generic_calls:?}");
        } else {
            unresolved = resolved_modules
                .drain(..)
                .collect();
        }
    }

    for (module, syms) in resolved_modules {
        let (m, ir) = validate::clean_up(module, syms);
        match gen_module(m, ir) {
            Ok(file) => files.push(file),
            Err(e) => return ExitCode::from(e),
        }
    }


    println!("CALLS {:?}", generic_calls);

    let mut obj_files = Vec::new();
    for file in files {
        let path = Path::new(&file);
        let stem = path.file_stem().unwrap(); // gets the name without extension
        let obj = format!("{}.o", stem.to_string_lossy());
        println!("nasm {}: {:?}", file, Command::new("nasm").args(["-felf64", &file, "-o", &obj]).output());
        obj_files.push(obj);
    }
    println!("ld: {:?}", Command::new("ld").args(obj_files).args(["-o", "out"]).output());

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

    let (tokens, _) = match tokenize::tokenize(&content) {
        Ok(res) => res,
        Err(e) => {
            error::token_err(&import.path, e);
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

fn gen_module(module: node::Module, mut ir: HashMap<String, ir::Symbol>) -> Result<String, u8> {
    let path = Path::new(&module.path);
    let stem = path.file_stem().unwrap(); // gets the name without extension
    let res_file = format!("{}.S", stem.to_string_lossy());

    let processed_stmts = process::process(module);

    // println!("Processed statements:\n{:?}\n", processed_stmts);

    lower::lower(processed_stmts, &mut ir);

    // println!("IR:\n{:?}\n", ir);

    // optimize::optimize(&mut ir);

    // println!("Optimized IR:\n{:?}\n", ir);

    let result = match gen::gen(&mut ir) {
        Ok(res) => res,
        Err((module, e)) => {
            error::gen_err(&module, e);
            return Err(1);
        }
    };
    fs::write(&res_file, result).expect("Unable to write file");
    Ok(res_file)
}

fn get_path(parent: &String, s: &String) -> String {
    let parent = Path::new(&parent)
                .parent()
                .unwrap_or_else(|| Path::new("."));
    parent.join(s).with_extension("re").display().to_string().replace('\\', "/")
}
