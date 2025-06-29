use std::fs;
use std::io::Error;

use crate::gen::GenError;
use crate::ir::OpLoc;
use crate::node::{self, Span};
use crate::parse::ParseError;
use crate::tokenize::{self, TokenizeError};
use crate::validate::SemanticError;

#[derive(Debug, Clone)]
pub struct FilePos {
    pub start: usize,
    pub end: usize,
}
impl FilePos {
    pub fn pos_id(locs: Vec<FilePos>, pos_id: usize) -> Self {
        locs.get(pos_id).unwrap().clone()
    }
    pub fn span(locs: Vec<FilePos>, span: Span) -> Self {
        Self {
            start: locs.get(span.start).unwrap().start,
            end: locs.get(span.end).unwrap().end,
        }
    }
}

pub fn import_error(err: Error, import: &node::Import) {
    if let Some(parent) = &import.parent {
        print_err();
        eprintln!("For path '{}': {err}", import.path);
        print_module_err_id(&parent, import.pos_id);
    } else {
        print_err();
        eprintln!("For path '{}': {err}", import.path);
    }
}

pub fn token_err(path: &String, e: TokenizeError) {
    print_err();
    match e {
        TokenizeError::InvalidCharacter(c) => {
            eprintln!("Invalid character '{}'", c.ch);
            print_module_err_pos(&path, c.pos);
        }
        TokenizeError::InvalidNumberCharacter(c) => {
            eprintln!("Invalid character '{}' in number", c.ch);
            print_module_err_pos(&path, c.pos);
        }
        TokenizeError::UnclosedCharacter(c) => {
            eprintln!("Unclosed character '{}'", c.ch);
            print_module_err_pos(&path, c.pos);
        }
    }
}

pub fn parse_err(path: &String, e: ParseError) {
    print_err();
    match e {
        ParseError::UnexpectedToken(u) => {
            eprintln!("Unexpected token '{}', expected {}.", u.token.to_string(), u.expected);
            print_module_err_id(path, u.pos_id);
        }
        ParseError::UnexpectedEndOfInput(expected) => {
            eprintln!("Unexpected end of input, expected {}.", expected);
            let text = fs::read_to_string(&path).unwrap();
            let mut end = text.len();
            for c in text.chars().rev() {
                if !c.is_whitespace() {
                    break;
                }
                end -= 1;
            }
            print_file_err(&text, path, &FilePos { start: end, end })
        }
        ParseError::ImportNotAtStart(pos_id) => {
            eprintln!("Import not at the top of the file.");
            print_module_err_id(path, pos_id);
        }
    }
}

pub fn sematic_err(path: &String, e: SemanticError) {
    print_err();
    match e {
        SemanticError::InvalidType(span) => {
            eprintln!("Invalid type.");
            print_module_err_span(path, span);
        }
        SemanticError::SymbolExists(pos_str) => {
            eprintln!("Symbol '{}' exists.", pos_str.str);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::UndeclaredSymbol(pos_str) => {
            eprintln!("Undeclared symbol '{}'.", pos_str.str);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::TypeMismatch(pos_str, ty1, ty2) => {
            eprintln!("Type mismatch: can't use '{}' with {} and {}.", pos_str.str, ty1, ty2);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::UnexpectedType(span, ty1, ty2) => {
            eprintln!("Type mismatch: expected {} but got {}.", ty1, ty2);
            print_module_err_span(path, span);
        }
        SemanticError::NotIterable(span, ty) => {
            eprintln!("Can't iterate over {ty}.");
            print_module_err_span(path, span);
        }
        SemanticError::NotUnwrappable(pos_str) => {
            eprintln!("Can't unwrap {}.", pos_str.str);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::NoCapture(span) => {
            eprintln!("There is no capture.");
            print_module_err_span(path, span);
        }
        SemanticError::NoMatch(pos_id, ty) => {
            eprintln!("Couldn't match type {ty}.");
            print_module_err_id(path, pos_id);
        }
        SemanticError::InvalidReturn(pos_id) => {
            eprintln!("Invalid return statement.");
            print_module_err_id(path, pos_id);
        }
        SemanticError::NotInLoop(name, pos_id) => {
            eprintln!("{} statement not in a loop.", name);
            print_module_err_id(path, pos_id);
        }
        SemanticError::InvalidAssign(span) => {
            eprintln!("Invalid assignment.");
            print_module_err_span(path, span);
        }
        SemanticError::ConstAssign(span, ty) => {
            eprintln!("Can't assign to constant type {ty}.");
            print_module_err_span(path, span);
        }
        SemanticError::InvalidUnaryOperator(pos_str) => {
            eprintln!("Invalid unary operator '{}'.", pos_str.str);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::UnaryTypeMismatch(span, str, ty) => {
            eprintln!("Can't use '{}' with type {}.", str, ty);
            print_module_err_span(path, span);
        }
        SemanticError::InvalidAddressOf(pos_str) => {
            eprintln!("Invalid '&' use. Expected &symbol.");
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::InvalidDereference(pos_str, ty) => {
            eprintln!("Invalid dereference: can't dereference {}", ty);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::StructDereference(span) => {
            eprintln!("Can't dereference struct. Use '.' directly with the pointer.");
            print_module_err_span(path, span);
        }
        SemanticError::NoIndirection(span) => {
            eprintln!("Indirection required.");
            print_module_err_span(path, span);
        }
        SemanticError::InvalidGlobal(span) => {
            eprintln!("Invalid global.");
            print_module_err_span(path, span);
        }
        SemanticError::FuncInFunc(pos_str) => {
            eprintln!("Function inside another function.");
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::TypeInFunc(pos_str) => {
            eprintln!("Type declaration inside a function.");
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::DecoratorInFunc(span) => {
            eprintln!("Decorator inside a function.");
            print_module_err_span(path, span);
        }
        SemanticError::InvalidTypeArgCount(span, exp, got) => {
            eprintln!("Invalid type argument count, expected {} type arguments but got {}", exp, got);
            print_module_err_span(path, span);
        }
        SemanticError::InvalidArgCount(span, exp, got) => {
            eprintln!("Invalid argument count, expected {} arguments but got {}", exp, got);
            print_module_err_span(path, span);
        }
        SemanticError::ArgTypeMismatch(span, ty1, ty2) => {
            eprintln!("Argument type mismatch: expected {} but got {}", ty1, ty2);
            print_module_err_span(path, span);
        }
        SemanticError::NoFnSig(str, span, tys, ty) => {
            let ty_list = tys.iter().map(|ty| format!("{}", ty)).collect::<Vec<_>>().join(", ");
            eprint!("No function of signature {}({})", str, ty_list);
            if let Some(t) = ty {
                eprint!(" -> {}", t);
            }
            eprintln!(".");
            print_module_err_span(path, span);
        }
        SemanticError::InvalidStructKey(pos_str1, pos_str2) => {
            eprintln!("Struct '{}' doesn't have key '{}'.", pos_str1.str, pos_str2.str);
            print_module_err_id(path, pos_str2.pos_id);
        }
        SemanticError::MissingStructKey(pos_str, key) => {
            eprintln!("Missing key '{}' for struct '{}'.", key, pos_str.str);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::StructTypeMismatch(pos_str, ty1, ty2) => {
            eprintln!("Struct type mismatch: expected {} but got {}.", ty1, ty2);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::InvalidMemberAccess(span) => {
            eprintln!("Invalid member access.");
            print_module_err_span(path, span);
        }
        SemanticError::EmptyArray(span) => {
            eprintln!("Empty arrays not allowed. Use 'decl' or initialize with values.");
            print_module_err_span(path, span);
        }
        SemanticError::ArrayTypeMismatch(span, ty1, ty2) => {
            eprintln!("Array type mismatch: expected {} but got {}.", ty1, ty2);
            print_module_err_span(path, span);
        }
        SemanticError::GenericTypeMismatch(span, ty1, ty2) => {
            eprintln!("Generic type mismatch: expected {} but got {}.", ty1, ty2);
            print_module_err_span(path, span);
        }
        SemanticError::NoSlice(span, str) => {
            eprintln!("Can't slice {str}.");
            print_module_err_span(path, span);
        }
        SemanticError::InvalidCast(span, ty1, ty2) => {
            eprintln!("Can't cast from {} into {}.", ty1, ty2);
            print_module_err_span(path, span);
        }
        SemanticError::NoBuiltIn(span, str, ty) => {
            eprintln!("Type {ty} has no {str}.");
            print_module_err_span(path, span);
        }
        SemanticError::PrivateAccess(pos_str) => {
            eprintln!("Can't access private symbol '{}'.", pos_str.str);
            print_module_err_id(path, pos_str.pos_id);
        }
        SemanticError::MainFnCall(pos_str) => {
            eprintln!("Can't call main function");
            print_module_err_id(path, pos_str.pos_id);
        }
    }
}

pub fn gen_err(path: &String, e: GenError) {
    print_err();
    match e {
        GenError::NoMainFn => {
            eprintln!("Main function was not found.");
        }
        GenError::ReservedSymbol(loc) => {
            eprintln!("Symbol is reserved");
            print_module_err_op(path, loc);
        }
        GenError::NoFreeRegisters(loc) => {
            eprintln!("Ran out of registers. Consider splitting up this expression.");
            print_module_err_op(path, loc);
        }
        GenError::TooManyArguments(loc) => {
            eprintln!("Too many arguments. Consider passing a struct instead.");
            print_module_err_op(path, loc);
        }
    }
}

fn print_err() {
    eprint!("\x1b[91mError: \x1b[0m");
}

fn print_module_err_id(module: &String, pos_id: usize) {
    let text = fs::read_to_string(&module).unwrap();
    let (_, locs) = tokenize::tokenize(&text).unwrap();
    print_file_err(&text, module, &FilePos::pos_id(locs, pos_id));
}

fn print_module_err_pos(module: &String, file_pos: FilePos) {
    let text = fs::read_to_string(&module).unwrap();
    print_file_err(&text, module, &file_pos);
}

fn print_module_err_span(module: &String, span: Span) {
    let text = fs::read_to_string(&module).unwrap();
    let (_, locs) = tokenize::tokenize(&text).unwrap();
    print_file_err(&text, module, &FilePos::span(locs, span));
}

fn print_module_err_op(module: &String, op: OpLoc) {
    let text = fs::read_to_string(&module).unwrap();
    let (_, locs) = tokenize::tokenize(&text).unwrap();
    print_file_err(
        &text,
        module,
        &FilePos {
            start: locs.get(op.start_id).unwrap().start,
            end: locs.get(op.end_id).unwrap().end,
        },
    );
}

fn print_file_err(text: &String, module: &String, pos: &FilePos) {
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
        if cur_pos == len {
            line.push(c);
            line_pos += 1;
        }
        if c == '\n' || cur_pos == len {
            if cur_pos > pos.start || cur_pos == len {
                let padding = usize::ilog10(line_nr);
                let padstr = " ".repeat(padding as usize + 2);
                eprintln!("\x1b[94m{}> {}\x1b[0m", padstr, module);
                eprintln!("\x1b[94m{}|\x1b[0m", padstr);
                eprint!("\x1b[94m{} |\x1b[0m {}", line_nr, line);
                eprintln!();
                let mut line_pad = String::new();
                let mut one_tab = false;
                for c in line.chars().take(line_start) {
                    if c == '\t' {
                        line_pad.push('\t');
                        one_tab = true;
                    } else {
                        line_pad.push(' ');
                    }
                }
                if one_tab {
                    line_pad.pop(); // removes the last char, which should be a space
                }
                eprint!("\x1b[94m{}|\x1b[0m{}", padstr, line_pad);
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
