use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs;
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

use error::{ErrorInfo, Location};

/// Holds the compiler's configuration, parsed from command-line arguments.
struct Config {
    input_file: String,
    output_file: String,
    verbose: bool,
    assembly_only: bool,
    compile_only: bool,
    diagnostics_mode: bool,
}

/// Parses command-line arguments into a Config struct.
fn parse_config(args: &[String]) -> Result<Config, String> {
    // Default configuration
    let mut config = Config {
        input_file: String::new(),
        output_file: "out".to_string(),
        verbose: false,
        assembly_only: false,
        compile_only: false,
        diagnostics_mode: false,
    };

    // Check for a help flag first
    if args.iter().any(|arg| arg == "-h" || arg == "--help") {
        let help_message = format!(
            "Usage: {} <file> [options]\n\n\
             Options:\n\
             -o, --output <file>    Specify the output file name (default: \"out\").\n\
             -S                     Compile to assembly only; do not assemble or link.\n\
             -c                     Compile and assemble; do not link.\n\
             -v, --verbose          Enable verbose output for debugging.\n\
             --diagnostics          Output errors in a machine-readable format for IDEs.\n\
             -h, --help             Display this help message.",
            args.get(0).unwrap_or(&"compiler".to_string())
        );
        println!("{}", help_message);
        std::process::exit(0);
    }

    let mut args_iter = args.iter().skip(1).peekable(); // Skip program name

    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            "-o" | "--output" => {
                if let Some(val) = args_iter.next() {
                    config.output_file = val.clone();
                } else {
                    return Err("Expected a filename after '-o' or '--output' flag.".to_string());
                }
            }
            "-v" | "--verbose" => {
                config.verbose = true;
            }
            "-S" => {
                config.assembly_only = true;
            }
            "-c" => {
                config.compile_only = true;
            }
            "--diagnostics" => {
                config.diagnostics_mode = true;
            }
            _ => {
                if arg.starts_with('-') {
                    return Err(format!("Unknown flag: {}", arg));
                }
                if config.input_file.is_empty() {
                    config.input_file = arg.clone();
                } else {
                    return Err(format!("Unexpected argument '{}'. Input file already set to '{}'.", arg, config.input_file));
                }
            }
        }
    }

    if config.input_file.is_empty() {
        return Err("No input file specified. Use -h or --help for usage information.".to_string());
    }

    Ok(config)
}


fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    
    let config = match parse_config(&args) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {}", e);
            return ExitCode::from(1);
        }
    };

    let mut import = node::Import {
        path: config.input_file.clone(),
        parent: None,
        pos_id: usize::MAX,
    };

    let mut handled_imports = HashSet::new();
    let mut import_names = vec![];
    let mut modules = vec![];
    let mut cur_imports = vec![];

    loop {
        let (module, imports) = match parse_module(&import, &config) {
            Ok(res) => res,
            Err(e) => return e,
        };

        import_names.push(imports.iter().map(|i| get_path(&import.path, &i.path)).collect::<Vec<_>>());

        modules.push(node::Module {
            stmts: module,
            path: import.path.clone(),
        });

        for mut i in imports {
            let cur = i.parent.clone().unwrap();
            i.path = get_path(&cur, &i.path);

            if config.verbose {
                println!("Found import: {:?}", i);
            }

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
    let mut generic_fns = Vec::new();
    let mut is_generic_vec = Vec::new();

    for module in modules.iter_mut() {
        let before = generic_fns.len();
        match validate::hoist_public(module, &public_map, &mut generic_fns) {
            Ok(res) => {
                let binding = module.path.clone();
                public_map.insert(binding, res);
            }
            Err(e) => {
                let info = error::semantic_err(e);
                error::print_error(&module.path, &info, &config);
                return ExitCode::from(1);
            }
        }
        is_generic_vec.push(before != generic_fns.len());
    }

    let mut files = Vec::new();
    let mut generic_calls = Vec::new();
    let mut unresolved = Vec::new();

    for (mut module, (imports, is_generic)) in modules.into_iter().zip(import_names.into_iter().zip(is_generic_vec.into_iter())) {
        let mut pub_symbols = Vec::new();
        for path in imports {
            if let Some(symbols) = public_map.get(&path) {
                pub_symbols.push(symbols);
            }
        }
        let syms = match validate::validate(&mut module, pub_symbols, &mut generic_calls, &mut generic_fns) {
            Ok(res) => res,
            Err(e) => {
                let info = error::semantic_err(e);
                error::print_error(&module.path, &info, &config);
                return ExitCode::from(1);
            }
        };

        if is_generic {
            unresolved.push((module, syms));
            continue;
        }

        let (m, ir) = validate::clean_up(module, syms);
        match gen_module(m, ir, &config) {
            Ok(file) => files.push(file),
            Err(e) => return e,
        }
    }
    
    if config.verbose {
        println!("UNRESOLVED MODULES: {}", unresolved.len());
        println!("INITIAL GENERIC CALLS: {:?}", generic_calls);
    }
    
    let mut resolved_modules = Vec::new();

    loop {
        if config.verbose {
            println!("Resolving generics with {} calls...", generic_calls.len());
        }

        let drained: Vec<_> = unresolved.drain(..).collect();
        let before = generic_calls.clone();

        for (module, syms) in drained {
            let path = module.path.clone();
            match validate::resolve(module, syms, &mut generic_calls, &mut generic_fns) {
                Ok((ir, resolved_module)) => {
                    if config.verbose {
                        println!("Updated generic calls: {:?}", generic_calls);
                    }
                    resolved_modules.push((resolved_module, ir));
                }
                Err(e) => {
                    let info = error::semantic_err(e);
                    error::print_error(&path, &info, &config);
                    return ExitCode::from(1);
                }
            }
        }

        if generic_calls.is_empty() {
            break;
        } else if before == generic_calls {
            panic!("Compiler Error: No progress made on resolving generic calls. Aborting. {:?}", generic_calls);
        } else {
            unresolved = resolved_modules.drain(..).collect();
        }
    }

    
    for (module, syms) in resolved_modules {
        let (m, ir) = validate::clean_up(module, syms);
        match gen_module(m, ir, &config) {
            Ok(file) => files.push(file),
            Err(e) => return e,
        }
    }

    
    if config.verbose {
        println!("FINAL GENERIC CALLS: {:?}", generic_calls);
        println!("Generated Assembly files: {:?}", files);
    }

    if config.diagnostics_mode {
        return ExitCode::from(0);
    }
    
    let mut obj_files = Vec::new();
    for file in files {
        let path = Path::new(&file);

        if !config.assembly_only { 
            let stem = path.file_stem().unwrap();
            let obj = format!("{}.o", stem.to_string_lossy());
            
            let nasm_output = Command::new("nasm")
                .args(["-felf64", &file, "-o", &obj])
                .output()
                .expect("Failed to execute nasm assembler.");
            
            if config.verbose || !nasm_output.status.success() {
                println!("nasm {}:", file);
                if !nasm_output.stdout.is_empty() {
                    println!("{}", String::from_utf8_lossy(&nasm_output.stdout));
                }
                if !nasm_output.stderr.is_empty() {
                    eprintln!("{}", String::from_utf8_lossy(&nasm_output.stderr));
                }
            }
            
            if !nasm_output.status.success() {
                eprintln!("nasm failed for file {}. Aborting.", file);
                return ExitCode::from(1);
            }

            obj_files.push(obj);
        }
    }

    if config.assembly_only {
        println!("Compilation finished. Assembly files generated.");
        return ExitCode::from(0);
    }

    if config.compile_only {
        println!("Compilation finished. Object files generated.");
        return ExitCode::from(0);
    }

    let ld_output = Command::new("ld")
        .args(&obj_files)
        .args(["-o", &config.output_file])
        .output()
        .expect("Failed to execute ld linker.");

    if config.verbose || !ld_output.status.success() {
        println!("ld -o {}:", config.output_file);
        if !ld_output.stdout.is_empty() {
            println!("{}", String::from_utf8_lossy(&ld_output.stdout));
        }
        if !ld_output.stderr.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&ld_output.stderr));
        }
    }

    if !ld_output.status.success() {
        eprintln!("ld failed. Aborting.");
        return ExitCode::from(1);
    }

    if config.verbose {
        println!("Successfully created executable '{}'", config.output_file);
    }

    for obj in obj_files {
        let _ = fs::remove_file(obj);
    }

    ExitCode::from(0)
}

fn parse_module(import: &node::Import, config: &Config) -> Result<(Vec<node::Stmt>, Vec<node::Import>), ExitCode> {
    let content = match fs::read_to_string(&import.path) {
        Ok(c) => c,
        Err(err) => {
            let info = ErrorInfo {
                message: format!("For path '{}': {}", import.path, err),
                location: Location::PosId(import.pos_id),
                level: "error",
            };
            error::print_error(&import.path, &info, config);
            return Err(ExitCode::from(1));
        }
    };

    let (tokens, _) = match tokenize::tokenize(&content) {
        Ok(res) => res,
        Err(e) => {
            let info = error::token_err(e);
            error::print_error(&import.path, &info, config);
            return Err(ExitCode::from(1));
        }
    };

    match parse::parse(tokens, Some(import.path.clone())) {
        Ok(res) => Ok(res),
        Err(e) => {
            let info = error::parse_err(e);
            error::print_error(&import.path, &info, config);
            Err(ExitCode::from(1))
        }
    }
}

fn gen_module(module: node::Module, mut ir: HashMap<String, ir::Symbol>, config: &Config) -> Result<String, ExitCode> {
    let path = Path::new(&module.path);
    let stem = path.file_stem().unwrap();
    let res_file = format!("{}.S", stem.to_string_lossy());

    let processed_stmts = process::process(module);

    lower::lower(processed_stmts, &mut ir);

    let result = match gen::gen(&mut ir) {
        Ok(res) => res,
        Err((module_path, e)) => {
            let info = error::gen_err(e);
            error::print_error(&module_path, &info, config);
            return Err(ExitCode::from(1));
        }
    };
    fs::write(&res_file, result).expect("Unable to write file");
    Ok(res_file)
}

fn get_path(parent: &String, s: &String) -> String {
    let parent = Path::new(&parent).parent().unwrap_or_else(|| Path::new("."));
    parent.join(s).with_extension("re").display().to_string().replace('\\', "/")
}
