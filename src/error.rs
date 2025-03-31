use crate::gen::GenError;
use crate::node::Span;
use crate::parse::ParseError;
use crate::tokenize::TokenizeError;
use crate::validate::SemanticError;

#[derive(Debug, Clone)]
pub struct FilePos {
    pub start: usize,
    pub end: usize,
}
impl FilePos {
    pub fn pos_id(info: Vec<FilePos>, pos_id: usize) -> Self {
        info.get(pos_id).unwrap().clone()
    }
    pub fn span(info: Vec<FilePos>, span: Span) -> Self {
        Self {
            start: info.get(span.start).unwrap().start,
            end: info.get(span.end).unwrap().end
        }
    }
}

pub fn token_err(text: &String, e: TokenizeError) {
    print_err();
    match e {
        TokenizeError::InvalidCharacter(c) => {
            eprintln!("Invalid character '{}'", c.ch);
            print_file_err(text, &c.pos);
        }
        TokenizeError::InvalidNumberCharacter(c) => {
            eprintln!("Invalid character '{}' in number", c.ch);
            print_file_err(text, &c.pos);
        }
    }
}

pub fn parse_err(text: &String, e: ParseError, info: Vec<FilePos>) {
    print_err();
    match e {
        ParseError::UnexpectedToken(u) => {
            eprintln!("Unexpected token '{}', expected {}.", u.token.to_string(), u.expected);
            print_file_err(text, info.get(u.info_id).unwrap());
        }
        ParseError::UnexpectedEndOfInput(expected) => {
            eprintln!("Unexpected end of input, expected {}.", expected);
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
            eprintln!("Invalid macro '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
    }
}

pub fn sematic_err(text: &String, e: SemanticError, info: Vec<FilePos>) {
    print_err();
    match e {
        SemanticError::InvalidType(pos_str) => {
            eprintln!("Invalid type '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::SymbolExists(pos_str) => {
            eprintln!("Symbol '{}' exists.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::UndeclaredSymbol(pos_str) => {
            eprintln!("Undeclared symbol '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::TypeMismatch(pos_str, ty1, ty2) => {
            eprintln!("Type mismatch: can't use '{}' with {:?} and {:?}", pos_str.str, ty1, ty2);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidReturn(pos_id) => {
            eprintln!("Invalid return statement.");
            print_file_err(text, info.get(pos_id).unwrap());
        }
        SemanticError::NotInLoop(name, pos_id) => {
                eprintln!("{} statement not in a loop.", name);
                print_file_err(text, info.get(pos_id).unwrap());
            }
        SemanticError::InvalidUnaryOperator(pos_str) => {
            eprintln!("Invalid unary operator '{}'.", pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidAddressOf(pos_str) => {
            eprintln!("Invalid '&' use. Expected &symbol.");
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidDereference(pos_str, ty) => {
            eprintln!("Invalid dereference: can't dereference {:?}", ty);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::FuncInFunc(pos_str) => {
            eprintln!("Function inside another function.");
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::StructInFunc(pos_str) => {
            eprintln!("Struct declaration inside a function.");
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::InvalidArgCount(pos_str, exp, got) => {
            eprintln!("Invalid argument count, expected {} arguments but got {}", exp, got);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::ArgTypeMismatch(span, ty1, ty2) => {
            eprintln!("Argument type mismatch: expected {:?} but got {:?}", ty1, ty2);
            print_file_err(text, &FilePos::span(info, span));
        }
        SemanticError::InvalidStructKey(pos_str1, pos_str2) => {
            eprintln!("Struct '{}' doesn't have key '{}'.", pos_str1.str, pos_str2.str);
            print_file_err(text, info.get(pos_str2.pos_id).unwrap());
        }
        SemanticError::MissingStructKey(pos_str, key) => {
            eprintln!("Missing key '{}' for struct '{}'.", key, pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::StructTypeMismatch(pos_str, ty1, ty2) => {
            eprintln!("Struct type mismatch: expected {:?} but got {:?}", ty1, ty2);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
        SemanticError::EmptyArray(span) => {
            eprintln!("Empty arrays not allowed. Use 'decl' or initialize with values.");
            print_file_err(text, &FilePos::span(info, span));
        }
        SemanticError::ArrayTypeMismatch(span, ty1, ty2) => {
            eprintln!("Array type mismatch: expected {:?} but got {:?}", ty1, ty2);
            print_file_err(text, &FilePos::span(info, span));
        }
        SemanticError::MissingLen(pos_str) => {
            eprintln!("Missing length for '{}'. For example, '{}[int, 4]'.", pos_str.str, pos_str.str);
            print_file_err(text, info.get(pos_str.pos_id).unwrap());
        }
    }
}

pub fn gen_err(text: &String, e: GenError, info: Vec<FilePos>) {
    print_err();
    match e {
        GenError::NoMainFn => {
            eprintln!("Main function was not found.");
        }
        GenError::ReservedSymbol(loc) => {
            eprintln!("Symbol is reserved");
            print_file_err(
                text,
                &FilePos {
                    start: info.get(loc.start_id).unwrap().start,
                    end: info.get(loc.end_id).unwrap().end,
                },
            );
        }
        GenError::NoFreeRegisters(loc) => {
            eprintln!("Ran out of registers. Consider splitting up this expression.");
            print_file_err(
                text,
                &FilePos {
                    start: info.get(loc.start_id).unwrap().start,
                    end: info.get(loc.end_id).unwrap().end,
                },
            );
        }
        GenError::ExpectedLiteral(loc) => {
            eprintln!("Expected a literal.");
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
    eprint!("\x1b[91mError: \x1b[0m");
}

fn print_file_err(text: &String, pos: &FilePos) {
    let mut line: String = String::new();
    let mut cur_pos: usize = 0;
    let mut line_pos: usize = 0;
    let mut line_start: usize = 0;
    let mut line_end: usize = 0;
    let mut line_nr: usize = 1;
    let len = text.chars().count();
    for c in text.chars() {
        cur_pos += 1;
        if cur_pos == pos.start {
            line_start = line_pos + 1;
        }
        if cur_pos == pos.end {
            line_end = line_pos + 2;
        }
        if c == '\n' || cur_pos == len {
            if cur_pos > pos.start {
                let padding = usize::ilog10(line_nr);
                let padstr = " ".repeat(padding as usize + 2);
                eprintln!("\x1b[94m{}|\x1b[0m", padstr);
                eprintln!("\x1b[94m{} |\x1b[0m {}", line_nr, line);
                eprint!("\x1b[94m{}|\x1b[0m", padstr);
                for _ in 0..line_start {
                    eprint!(" ");
                }
                eprint!("\x1b[91m");
                let end;
                if cur_pos > pos.end {
                    end = line_end;
                } else {
                    end = line.len() + 1;
                }
                for _ in 0..end - line_start {
                    eprint!("^")
                }
                eprintln!("\x1b[0m");
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
