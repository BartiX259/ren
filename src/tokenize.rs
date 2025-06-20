use crate::error::FilePos;
use crate::helpers::{StringFragment, StringLit, VecIter};
use std::vec::Vec;

/// Tokenize the input file
pub fn tokenize(input: &String) -> Result<(Vec<Token>, Vec<FilePos>), TokenizeError> {
    let mut tokens = Vec::new();
    let mut info = Vec::new();
    let mut iter = VecIter::new(input.chars().collect());
    let mut start;
    let mut interps = Vec::new();
    while let Some(ch) = iter.next() {
        start = iter.current_index();
        if let Some(ch_tok) = Token::from_char(ch) {
            if let Token::OpenCurly = ch_tok {
                if let Some(i) = interps.last_mut() {
                    tokens.push(Token::StringInterpolationStart);
                    *i += 1;
                    continue;
                } else {
                    tokens.push(ch_tok);
                }
            } else if let Token::CloseCurly = ch_tok {
                if let Some(i) = interps.last_mut() {
                    *i -= 1;
                    if *i == 0 {
                        tokens.push(Token::StringInterpolationEnd);
                        info.push(FilePos { start, end: iter.current_index() });
                        interps.pop();
                        let (tok, interp) = tok_str('\"', &mut iter)?;
                        tokens.push(tok);
                        if interp {
                            tokens.push(Token::StringInterpolationStart);
                            info.push(FilePos { start, end: iter.current_index() });
                            interps.push(1);
                        }
                    } else {
                        tokens.push(ch_tok);
                    }
                } else {
                    tokens.push(ch_tok);
                }
            } else {
                tokens.push(ch_tok);
            }
        } else if ch.is_alphabetic() {
            tokens.push(tok_word(ch, &mut iter)?);
        } else if ch.is_ascii_digit() {
            tokens.push(tok_num(ch, &mut iter)?);
        } else if is_operator(ch) {
            tokens.push(tok_op(ch, &mut iter));
        } else if ch == '"' {
            let (tok, interp) = tok_str(ch, &mut iter)?;
            tokens.push(tok);
            if interp {
                tokens.push(Token::StringInterpolationStart);
                info.push(FilePos { start, end: iter.current_index() });
                interps.push(1);
            }
        } else if ch == '\'' {
            tokens.push(tok_char(ch, &mut iter)?);
        } else if ch.is_whitespace() {
            continue;
        } else {
            return Err(TokenizeError::InvalidCharacter(InvalidCharacter {
                ch,
                pos: FilePos { start, end: iter.current_index() },
            }));
        }
        info.push(FilePos { start, end: iter.current_index() });
    }
    return Ok((tokens, info));
}

#[derive(Debug)]
pub struct InvalidCharacter {
    pub ch: char,
    pub pos: FilePos,
}

#[derive(Debug)]
pub enum TokenizeError {
    InvalidCharacter(InvalidCharacter),
    InvalidNumberCharacter(InvalidCharacter),
    UnclosedCharacter(InvalidCharacter),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word { value: String },
    IntLit { value: i64 },
    StringLit { value: StringLit },
    CharLit { value: u32 },
    Op { value: String },
    Import { module: String },
    Let,
    Decl,
    Fn,
    Type,
    Return,
    If,
    Else,
    Loop,
    While,
    For,
    Break,
    Continue,
    True,
    False,
    Null,
    None,
    As,
    In,
    Syscall,
    Enum,
    Extern,
    Pub,
    Semi,
    Colon,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    Comma,
    StringInterpolationStart,
    StringInterpolationEnd,
}

impl Token {
    pub fn to_string(&self) -> String {
        match self {
            Token::Word { value } => value.clone(),
            Token::IntLit { value } => value.to_string(),
            Token::StringLit { value } => format!("\"{value}\""),
            Token::CharLit { value } => format!("'{value}'"),
            Token::Op { value } => value.clone(),
            Token::Import { module } => format!("import {module}"),
            Token::Let => "let".to_string(),
            Token::Decl => "decl".to_string(),
            Token::Fn => "fn".to_string(),
            Token::Type => "type".to_string(),
            Token::Return => "return".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Loop => "loop".to_string(),
            Token::While => "while".to_string(),
            Token::For => "for".to_string(),
            Token::Break => "break".to_string(),
            Token::Continue => "continue".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::Null => "null".to_string(),
            Token::None => "none".to_string(),
            Token::As => "as".to_string(),
            Token::In => "in".to_string(),
            Token::Syscall => "syscall".to_string(),
            Token::Enum => "enum".to_string(),
            Token::Extern => "extern".to_string(),
            Token::Pub => "pub".to_string(),
            Token::Semi => ";".to_string(),
            Token::Colon => ":".to_string(),
            Token::OpenParen => "(".to_string(),
            Token::CloseParen => ")".to_string(),
            Token::OpenCurly => "{".to_string(),
            Token::CloseCurly => "}".to_string(),
            Token::OpenSquare => "[".to_string(),
            Token::CloseSquare => "]".to_string(),
            Token::Comma => ",".to_string(),
            Token::StringInterpolationStart => "{".to_string(),
            Token::StringInterpolationEnd => "}".to_string(),
        }
    }

    pub fn from_char(ch: char) -> Option<Self> {
        match ch {
            ';' => Some(Token::Semi),
            ':' => Some(Token::Colon),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenCurly),
            '}' => Some(Token::CloseCurly),
            '[' => Some(Token::OpenSquare),
            ']' => Some(Token::CloseSquare),
            ',' => Some(Token::Comma),
            _ => None,
        }
    }
    pub fn from_string(str: &String) -> Option<Self> {
        match str.as_str() {
            "let" => Some(Token::Let),
            "decl" => Some(Token::Decl),
            "fn" => Some(Token::Fn),
            "type" => Some(Token::Type),
            "return" => Some(Token::Return),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "loop" => Some(Token::Loop),
            "while" => Some(Token::While),
            "for" => Some(Token::For),
            "break" => Some(Token::Break),
            "continue" => Some(Token::Continue),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            "null" => Some(Token::Null),
            "none" => Some(Token::None),
            "as" => Some(Token::As),
            "in" => Some(Token::In),
            "syscall" => Some(Token::Syscall),
            "enum" => Some(Token::Enum),
            "extern" => Some(Token::Extern),
            "pub" => Some(Token::Pub),
            _ => None,
        }
    }
}

/// Builds a word token starting with the given character.
/// Ensures all characters in the word are alphanumeric.
fn tok_word(start: char, iter: &mut VecIter<char>) -> Result<Token, TokenizeError> {
    let mut word = String::new();
    word.push(start);

    while let Some(&next_ch) = iter.peek() {
        if next_ch.is_alphanumeric() || next_ch == '_' {
            word.push(next_ch);
            iter.next(); // Consume the character
        } else {
            break;
        }
    }
    if let Some(str_tok) = Token::from_string(&word) {
        return Ok(str_tok);
    }
    if word == "import" {
        let mut module = String::new();
        while let Some(&next_ch) = iter.peek() {
            if next_ch == '\n' {
                break;
            }
            module.push(next_ch);
            iter.next();
        }
        module = module.trim().to_string();
        return Ok(Token::Import { module });
    }
    Ok(Token::Word { value: word })
}
/// Builds a number literal token starting with the given character.
/// Ensures all characters in the word are digits.
fn tok_num(start: char, iter: &mut VecIter<char>) -> Result<Token, TokenizeError> {
    let mut word = String::new();
    let st = iter.prev_index();
    word.push(start);

    while let Some(&next_ch) = iter.peek() {
        if next_ch.is_ascii_digit() {
            word.push(next_ch);
            iter.next(); // Consume the character
        } else if next_ch.is_alphabetic() {
            return Err(TokenizeError::InvalidNumberCharacter(InvalidCharacter {
                ch: next_ch,
                pos: FilePos { start: st, end: iter.current_index() },
            }));
        } else {
            break;
        }
    }
    Ok(Token::IntLit {
        value: word.parse::<i64>().or_else(|_| word.parse::<u64>().map(|u| i64::from_ne_bytes(u.to_ne_bytes()))).unwrap(),
    })
}

/// Builds an operator token, supporting single, two and three character operators.
fn tok_op(start: char, iter: &mut VecIter<char>) -> Token {
    let mut op = String::new();
    op.push(start);

    if let Some(&next_ch) = iter.peek() {
        // Check for two-character operators like +=, ==, etc.
        if is_operator_pair(start, next_ch) {
            op.push(next_ch);
            iter.next(); // Consume the second character
        }
        if let Some(&next_ch) = iter.peek() {
            // Check for three-character operators like <<= and >>=
            if is_operator_triplet(op.as_str(), next_ch) {
                op.push(next_ch);
                iter.next(); // Consume the third character
            }
        }
    }

    Token::Op { value: op }
}

/// Builds a string literal until `start` appears
fn tok_str(start: char, iter: &mut VecIter<char>) -> Result<(Token, bool), TokenizeError> {
    let mut res = Vec::new();
    let mut str = String::new();
    let start_pos = iter.prev_index();
    let mut closed = false;
    let mut interp = false;
    while let Some(next_ch) = iter.next() {
        if next_ch == start {
            closed = true;
            break;
        } else if next_ch == '{' {
            closed = true;
            interp = true;
            break;
        } else if next_ch == '\\' {
            res.push(StringFragment::String(str));
            str = String::new();
            let Some(esc) = iter.next() else {
                return Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
                    ch: '\\',
                    pos: FilePos {
                        start: iter.prev_index(),
                        end: iter.prev_index(),
                    },
                }));
            };
            res.push(StringFragment::Char(esc_char(esc, iter)?));
        } else {
            str.push(next_ch);
        }
    }
    if !closed {
        Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
            ch: start,
            pos: FilePos { start: start_pos, end: start_pos },
        }))
    } else {
        if str.chars().count() > 0 {
            res.push(StringFragment::String(str));
        }
        res.push(StringFragment::Char(0));
        Ok((Token::StringLit { value: StringLit { frags: res } }, interp))
    }
}

/// Builds a char literal until `start` appears
fn tok_char(start: char, iter: &mut VecIter<char>) -> Result<Token, TokenizeError> {
    let startpos = FilePos {
        start: iter.current_index(),
        end: iter.current_index(),
    };
    let value = match iter.next() {
        Some('\\') => {
            let Some(esc) = iter.next() else {
                return Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
                    ch: '\\',
                    pos: FilePos {
                        start: iter.prev_index(),
                        end: iter.prev_index(),
                    },
                }));
            };
            esc_char(esc, iter)?
        }
        None => return Err(TokenizeError::UnclosedCharacter(InvalidCharacter { ch: start, pos: startpos })),
        Some(val) => val as u32,
    };
    let Some('\'') = iter.next() else {
        return Err(TokenizeError::UnclosedCharacter(InvalidCharacter { ch: start, pos: startpos }));
    };
    Ok(Token::CharLit { value })
}

fn esc_char(esc: char, iter: &mut VecIter<char>) -> Result<u32, TokenizeError> {
    match esc {
        'n' => Ok('\n' as u32),
        'r' => Ok('\r' as u32),
        't' => Ok('\t' as u32),
        '\\' => Ok('\\' as u32),
        '\'' => Ok('\'' as u32),
        '"' => Ok('"' as u32),
        '0' => Ok('\0' as u32),
        _ => Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
            ch: '\\',
            pos: FilePos {
                start: iter.prev_index(),
                end: iter.prev_index(),
            },
        })),
    }
}

/// Checks if a character is a valid single-character operator.
fn is_operator(ch: char) -> bool {
    ['+', '-', '*', '/', '=', '<', '>', '&', '|', '!', '%', '^', '.', '~', '?'].contains(&ch)
}

/// Checks if a pair of characters form a valid two-character operator.
fn is_operator_pair(first: char, second: char) -> bool {
    match (first, second) {
        ('+', '=')
        | ('-', '=')
        | ('*', '=')
        | ('/', '=')
        | ('%', '=')
        | ('|', '=')
        | ('^', '=')
        | ('&', '=')
        | ('=', '=')
        | ('!', '=')
        | ('<', '=')
        | ('>', '=')
        | ('&', '&')
        | ('|', '|')
        | ('<', '<')
        | ('>', '>')
        | ('-', '>')
        | ('.', '.') => true,
        _ => false,
    }
}

fn is_operator_triplet(first_two: &str, third: char) -> bool {
    match (first_two, third) {
        (">>", '=') | ("<<", '=') => true,
        _ => false,
    }
}
