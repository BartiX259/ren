use std::fs;

use crate::gen::GenError;
use crate::ir::OpLoc;
use crate::node::{self, Span};
use crate::parse::ParseError;
use crate::tokenize::{self, TokenizeError};
use crate::validate::SemanticError;
use crate::Config;

#[derive(Debug, Clone)]
pub struct FilePos {
    pub start: usize,
    pub end: usize,
}
impl FilePos {
    pub fn pos_id(locs: &[FilePos], pos_id: usize) -> Self {
        locs.get(pos_id).unwrap().clone()
    }
    pub fn span(locs: &[FilePos], span: Span) -> Self {
        Self {
            start: locs.get(span.start).unwrap().start,
            end: locs.get(span.end).unwrap().end,
        }
    }
    pub fn after_pos_id(locs: &[FilePos], pos_id: usize) -> Self {
        let mut res = locs.get(pos_id).unwrap().clone();
        res.start = res.end + 1;
        res.end = res.end + 1;
        res
    }
}

pub enum Location {
    Span(Span),
    PosId(usize),
    AfterPosId(usize),
    FilePos(FilePos),
    OpLoc(OpLoc),
}

pub struct ErrorInfo {
    pub message: String,
    pub location: Location,
    pub level: &'static str,
}

pub fn token_err(e: TokenizeError) -> ErrorInfo {
    match e {
        TokenizeError::InvalidCharacter(c) => ErrorInfo {
            message: format!("Invalid character '{}'", c.ch),
            location: Location::FilePos(c.pos),
            level: "error",
        },
        TokenizeError::InvalidNumberCharacter(c) => ErrorInfo {
            message: format!("Invalid character '{}' in number", c.ch),
            location: Location::FilePos(c.pos),
            level: "error",
        },
        TokenizeError::UnclosedCharacter(c) => ErrorInfo {
            message: format!("Unclosed character '{}'", c.ch),
            location: Location::FilePos(c.pos),
            level: "error",
        },
    }
}

pub fn parse_err(e: ParseError) -> ErrorInfo {
    match e {
        ParseError::UnexpectedToken(u) => ErrorInfo {
            message: format!("Unexpected token '{}', expected {}.", u.token.to_string(), u.expected),
            location: Location::PosId(u.pos_id),
            level: "error",
        },
        ParseError::ExpectedSemicolon(pos_id) => ErrorInfo {
            message: "Expected semicolon.".to_string(),
            location: Location::AfterPosId(pos_id),
            level: "error",
        },
        ParseError::UnexpectedEndOfInput(expected) => ErrorInfo {
            message: format!("Unexpected end of input, expected {}.", expected),
            location: Location::PosId(usize::MAX), // Special case handled in printer
            level: "error",
        },
        ParseError::ImportNotAtStart(pos_id) => ErrorInfo {
            message: "Import not at the top of the file.".to_string(),
            location: Location::PosId(pos_id),
            level: "error",
        },
    }
}

pub fn semantic_err(e: SemanticError) -> ErrorInfo {
    match e {
        SemanticError::InvalidType(span) => ErrorInfo {
            message: "Invalid type.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::SymbolExists(pos_str) => ErrorInfo {
            message: format!("Symbol '{}' exists.", pos_str.str),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::UndeclaredSymbol(pos_str) => ErrorInfo {
            message: format!("Undeclared symbol '{}'.", pos_str.str),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::TypeMismatch(pos_str, ty1, ty2) => ErrorInfo {
            message: format!("Type mismatch: can't use '{}' with {} and {}.", pos_str.str, ty1, ty2),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::UnexpectedType(span, ty1, ty2) => ErrorInfo {
            message: format!("Type mismatch: expected {} but got {}.", ty1, ty2),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NotIterable(span, ty) => ErrorInfo {
            message: format!("Can't iterate over {ty}."),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NotUnwrappable(pos_str) => ErrorInfo {
            message: format!("Can't unwrap {}.", pos_str.str),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::NoCapture(span) => ErrorInfo {
            message: "There is no capture.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NoMatch(pos_id, ty) => ErrorInfo {
            message: format!("Couldn't match type {ty}."),
            location: Location::PosId(pos_id),
            level: "error",
        },
        SemanticError::InvalidReturn(pos_id) => ErrorInfo {
            message: "Invalid return statement.".to_string(),
            location: Location::PosId(pos_id),
            level: "error",
        },
        SemanticError::InvalidCapture(capture, ty) => {
            let (span, capture_str) = match capture {
                node::Capture::Single(pos_str) => (
                    Span { start: pos_str.pos_id, end: pos_str.pos_id },
                    pos_str.str.to_string(),
                ),
                node::Capture::Multiple(pos_strs, dots) => {
                    let text = pos_strs.iter().map(|p| p.str.as_str()).collect::<Vec<_>>().join(", ");
                    let end_extra = if dots { ", .." } else { "" };
                    (
                        Span {
                            start: pos_strs.iter().next().unwrap().pos_id,
                            end: pos_strs.iter().last().unwrap().pos_id + if dots { 2 } else { 0 },
                        },
                        format!("{}{}", text, end_extra),
                    )
                }
            };
            ErrorInfo {
                message: format!("Can't capture {ty} as '{}'.", capture_str),
                location: Location::Span(span),
                level: "error",
            }
        },
        SemanticError::NotInLoop(name, pos_id) => ErrorInfo {
            message: format!("{} statement not in a loop.", name),
            location: Location::PosId(pos_id),
            level: "error",
        },
        SemanticError::InvalidAssign(span) => ErrorInfo {
            message: "Invalid assignment.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::ConstAssign(span, ty) => ErrorInfo {
            message: format!("Can't assign to constant type {ty}."),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::InvalidUnaryOperator(pos_str) => ErrorInfo {
            message: format!("Invalid unary operator '{}'.", pos_str.str),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::UnaryTypeMismatch(span, str, ty) => ErrorInfo {
            message: format!("Can't use '{}' with type {}.", str, ty),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::InvalidAddressOf(pos_str) => ErrorInfo {
            message: "Invalid '&' use. Expected &symbol.".to_string(),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::InvalidDereference(pos_str, ty) => ErrorInfo {
            message: format!("Invalid dereference: can't dereference {}", ty),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::StructDereference(span) => ErrorInfo {
            message: "Can't dereference struct. Use '.' directly with the pointer.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NoIndirection(span) => ErrorInfo {
            message: "Indirection required.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::InvalidGlobal(span) => ErrorInfo {
            message: "Invalid global.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::FuncInFunc(pos_str) => ErrorInfo {
            message: "Function inside another function.".to_string(),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::TypeInFunc(pos_str) => ErrorInfo {
            message: "Type declaration inside a function.".to_string(),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::DecoratorInFunc(span) => ErrorInfo {
            message: "Decorator inside a function.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::InvalidTypeArgCount(span, exp, got) => ErrorInfo {
            message: format!("Invalid type argument count, expected {} type arguments but got {}", exp, got),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::InvalidArgCount(span, exp, got) => ErrorInfo {
            message: format!("Invalid argument count, expected {} arguments but got {}", exp, got),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::ArgTypeMismatch(span, ty1, ty2) => ErrorInfo {
            message: format!("Argument type mismatch: expected {} but got {}", ty1, ty2),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NoFnSig(str, span, tys, ty) => {
            let ty_list = tys.iter().map(|ty| format!("{}", ty)).collect::<Vec<_>>().join(", ");
            let ret_str = if let Some(t) = ty { format!(" -> {}", t) } else { "".to_string() };
            ErrorInfo {
                message: format!("No function of signature {}({}){}.", str, ty_list, ret_str),
                location: Location::Span(span),
                level: "error",
            }
        },
        SemanticError::InvalidStructKey(pos_str1, pos_str2) => ErrorInfo {
            message: format!("Struct '{}' doesn't have key '{}'.", pos_str1.str, pos_str2.str),
            location: Location::PosId(pos_str2.pos_id),
            level: "error",
        },
        SemanticError::MissingStructKey(pos_str, key) => ErrorInfo {
            message: format!("Missing key '{}' for struct '{}'.", key, pos_str.str),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::StructTypeMismatch(pos_str, ty1, ty2) => ErrorInfo {
            message: format!("Struct type mismatch: expected {} but got {}.", ty1, ty2),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::InvalidMemberAccess(span) => ErrorInfo {
            message: "Invalid member access.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::EmptyArray(span) => ErrorInfo {
            message: "Empty arrays not allowed. Use 'decl' or initialize with values.".to_string(),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::ArrayTypeMismatch(span, ty1, ty2) => ErrorInfo {
            message: format!("Array type mismatch: expected {} but got {}.", ty1, ty2),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::GenericTypeMismatch(span, ty1, ty2) => ErrorInfo {
            message: format!("Generic type mismatch: expected {} but got {}.", ty1, ty2),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NoSlice(span, str) => ErrorInfo {
            message: format!("Can't slice {str}."),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::InvalidCast(span, ty1, ty2) => ErrorInfo {
            message: format!("Can't cast from {} into {}.", ty1, ty2),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::NoBuiltIn(span, str, ty) => ErrorInfo {
            message: format!("Type {ty} has no {str}."),
            location: Location::Span(span),
            level: "error",
        },
        SemanticError::PrivateAccess(pos_str) => ErrorInfo {
            message: format!("Can't access private symbol '{}'.", pos_str.str),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::MainFnCall(pos_str) => ErrorInfo {
            message: "Can't call main function".to_string(),
            location: Location::PosId(pos_str.pos_id),
            level: "error",
        },
        SemanticError::GenericInstantiationError { .. } => {
            panic!("GenericInstantiationError should be handled by the trace printer, not semantic_err.");
        }
    }
}

pub fn gen_err(e: GenError) -> ErrorInfo {
    match e {
        GenError::NoMainFn => ErrorInfo {
            message: "Main function was not found.".to_string(),
            location: Location::PosId(usize::MAX), // No location for this error
            level: "error",
        },
        GenError::ReservedSymbol(loc) => ErrorInfo {
            message: "Symbol is reserved".to_string(),
            location: Location::OpLoc(loc),
            level: "error",
        },
        GenError::NoFreeRegisters(loc) => ErrorInfo {
            message: "Ran out of registers. Consider splitting up this expression.".to_string(),
            location: Location::OpLoc(loc),
            level: "error",
        },
        GenError::TooManyArguments(loc) => ErrorInfo {
            message: "Too many arguments. Consider passing a struct instead.".to_string(),
            location: Location::OpLoc(loc),
            level: "error",
        },
    }
}

pub fn print_error(path: &String, info: &ErrorInfo, config: &Config) {
    if config.diagnostics_mode {
        let text = fs::read_to_string(path).unwrap();
        let (_, locs) = tokenize::tokenize(&text).unwrap();

        let file_pos = match &info.location {
            Location::Span(span) => FilePos::span(&locs, *span),
            Location::PosId(pos_id) if *pos_id != usize::MAX => FilePos::pos_id(&locs, *pos_id),
            Location::AfterPosId(pos_id) => FilePos::after_pos_id(&locs, *pos_id),
            Location::FilePos(pos) => pos.clone(),
            Location::OpLoc(op) => FilePos {
                start: locs.get(op.start_id).unwrap().start,
                end: locs.get(op.end_id).unwrap().end,
            },
            _ => { // Handles PosId::MAX and other potential cases
                eprintln!("{}:1:1: {}: {}", path, info.level, info.message);
                return;
            }
        };

        print_diagnostic_line(path, &text, &file_pos, info.level, &info.message);
    } else {
        print_err();
        eprintln!("{}", info.message);

        match &info.location {
            Location::Span(span) => print_module_err_span(path, *span),
            Location::PosId(pos_id) if *pos_id != usize::MAX => print_module_err_id(path, *pos_id),
            Location::AfterPosId(pos_id) => print_module_err_after_id(path, *pos_id),
            Location::FilePos(pos) => print_module_err_pos(path, pos.clone()),
            Location::OpLoc(op) => print_module_err_op(path, op.clone()),
            _ => {} // No pretty printing for errors without a specific location
        }
    }
}

fn print_diagnostic_line(path: &str, text: &str, pos: &FilePos, level: &str, message: &str) {
    let length = if pos.end > pos.start {
        pos.end - pos.start + 1
    } else {
        1
    };
    eprintln!("{}:{}:{}: {}: {}", path, file_location(text, pos), length, level, message);
}

pub fn file_location(text: &str, pos: &FilePos) -> String {
    let mut line_num = 1;
    let mut col_num = 1;
    for (i, char) in text.char_indices() {
        if i >= pos.start {
            break;
        }
        if char == '\n' {
            line_num += 1;
            col_num = 1;
        } else {
            col_num += 1;
        }
    }
    format!("{}:{}", line_num, col_num)
}

fn print_err() {
    eprint!("\x1b[91mError: \x1b[0m");
}

fn print_module_err_id(module: &String, pos_id: usize) {
    let text = fs::read_to_string(&module).unwrap();
    let (_, locs) = tokenize::tokenize(&text).unwrap();
    print_file_err(&text, module, &FilePos::pos_id(&locs, pos_id));
}

fn print_module_err_after_id(module: &String, pos_id: usize) {
    let text = fs::read_to_string(&module).unwrap();
    let (_, locs) = tokenize::tokenize(&text).unwrap();
    print_file_err(&text, module, &FilePos::after_pos_id(&locs, pos_id));
}

fn print_module_err_pos(module: &String, file_pos: FilePos) {
    let text = fs::read_to_string(&module).unwrap();
    print_file_err(&text, module, &file_pos);
}

fn print_module_err_span(module: &String, span: Span) {
    let text = fs::read_to_string(&module).unwrap_or_else(|e| panic!("{} {}", module, e));
    let (_, locs) = tokenize::tokenize(&text).unwrap();
    print_file_err(&text, module, &FilePos::span(&locs, span));
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
                for c in line.chars().take(line_start) {
                    if c == '\t' {
                        line_pad.push('\t');
                    } else {
                        line_pad.push(' ');
                    }
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
