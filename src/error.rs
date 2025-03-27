use crate::gen::GenError;
use crate::parse::ParseError;
use crate::token::TokenizeError;
use crate::validate::SemanticError;

#[derive(Debug)]
pub struct FilePos {
    pub start: usize,
    pub end: usize,
}

pub fn token_err(text: &String, e: TokenizeError) {
    print_err();
    match e {
        TokenizeError::InvalidCharacter(c) => {
            println!("Invalid character '{}'", c.ch);
            print_file_err(text, &c.pos);
        }
        TokenizeError::InvalidNumberCharacter(c) => {
            println!("Invalid character '{}' in number", c.ch);
            print_file_err(text, &c.pos);
        }
    }
}

pub fn parse_err(text: &String, e: ParseError, info: Vec<FilePos>) {
    print_err();
    match e {
        ParseError::UnexpectedToken(u) => {
            println!("Unexpected token '{}', expected {}.", u.token.to_string(), u.expected);
            print_file_err(text, info.get(u.info_id).unwrap());
        }
        ParseError::UnexpectedEndOfInput(expected) => {
            println!("Unexpected end of input, expected {}.", expected);
            let mut end = text.len();
            for c in text.chars().rev() {
                if !c.is_whitespace() {
                    break;
                }
                end -= 1;
            }
            print_file_err(
                text,
                &FilePos {
                    start: end,
                    end
                },
            )
        }
        ParseError::InvalidMacro(pos_str) => {
            println!("Invalid macro '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
    }
}

pub fn sematic_err(text: &String, e: SemanticError, info: Vec<FilePos>) {
    print_err();
    match e {
        SemanticError::InvalidType(pos_str) => {
            println!("Invalid type '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::SymbolExists(pos_str) => {
            println!("Symbol '{}' exists.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::UndeclaredSymbol(pos_str) => {
            println!("Undeclared symbol '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::TypeMismatch(pos_str, ty1, ty2) => {
            println!("Type mismatch: can't use '{}' with {:?} and {:?}", pos_str.str, ty1, ty2);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidReturn(pos_id) => {
            println!("Invalid return statement.");
            print_file_err(text, info.get(pos_id).unwrap());
        }
        SemanticError::NotInLoop(name, pos_id) => {
            println!("{} statement not in a loop.", name);
            print_file_err(text, info.get(pos_id).unwrap());
        }
        SemanticError::InvalidUnaryOperator(pos_str) => {
            println!("Invalid unary operator '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidAdressOf(pos_str) => {
            println!("Invalid '&' use. Expected &symbol.");
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidDereference(pos_str, ty) => {
            println!("Invalid dereference: can't dereference {:?}", ty);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::FuncInFunc(pos_str) => {
            println!("Function inside another function.");
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidArgCount(pos_str, exp, got) => {
            println!("Invalid argument count, expected {} arguments but got {}", exp, got);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::ArgTypeMismatch(pos_str, ty1, ty2) => {
            println!("Argument type mismatch: expected {:?} but got {:?}", ty1, ty2);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
    }
}

pub fn gen_err(text: &String, e: GenError, info: Vec<FilePos>) {
    print_err();
    match e {
        GenError::NoMainFn => {
            println!("Main function was not found.");
        }
        GenError::ReservedSymbol(loc) => {
            println!("Symbol is reserved");
            print_file_err(
                text,
                &FilePos {
                    start: info.get(loc.start_id).unwrap().start,
                    end: info.get(loc.end_id).unwrap().end,
                },
            );
        }
        GenError::NoFreeRegisters(loc) => {
            println!("Ran out of registers. Consider splitting up this expression.");
            print_file_err(
                text,
                &FilePos {
                    start: info.get(loc.start_id).unwrap().start,
                    end: info.get(loc.end_id).unwrap().end,
                },
            );
        }
        GenError::ExpectedLiteral(loc) => {
            println!("Expected a literal.");
            print_file_err(
                text,
                &FilePos {
                    start: info.get(loc.start_id).unwrap().start,
                    end: info.get(loc.end_id).unwrap().end,
                },
            );
        }
    }
}

fn print_err() {
    print!("\x1b[91mError: \x1b[0m");
}

fn print_file_err(text: &String, pos: &FilePos) {
    println!("\x1b[94m  |\x1b[0m");
    let mut line: String = String::new();
    let mut cur_pos: usize = 0;
    let mut line_pos: usize = 0;
    let mut line_start: usize = 0;
    let mut line_end: usize = 0;
    let mut line_nr: usize = 1;
    for c in text.chars() {
        cur_pos += 1;
        if cur_pos == pos.start {
            line_start = line_pos + 1;
        }
        if cur_pos == pos.end {
            line_end = line_pos + 2;
        }
        if c == '\n' {
            if cur_pos > pos.start {
                println!("\x1b[94m{} |\x1b[0m {}", line_nr, line);
                print!("\x1b[94m  |\x1b[0m");
                for _ in 0..line_start {
                    print!(" ");
                }
                print!("\x1b[91m");
                let end;
                if cur_pos > pos.end {
                    end = line_end;
                } else {
                    end = line.len() + 1;
                }
                for _ in 0..end - line_start {
                    print!("^")
                }
                println!("\x1b[0m");
                line_start = 0;
            }
            if cur_pos > pos.end {
                break;
            }
            line.clear();
            line_pos = 0;
            line_nr += 1;
        } else {
            line.push(c);
            line_pos += 1;
        }
    }
}
