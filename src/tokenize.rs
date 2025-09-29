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
        // Handle comments
        if ch == '/' {
            if let Some(&next_ch) = iter.peek() {
                if next_ch == '/' {
                    // Single-line comment: skip until newline or EOF
                    iter.next();
                    while let Some(nch) = iter.peek() {
                        if *nch == '\n' {
                            break;
                        }
                        iter.next();
                    }
                    continue;
                } else if next_ch == '*' {
                    // Multi-line comment: skip until */
                    iter.next();
                    loop {
                        match iter.next() {
                            Some('*') => {
                                if let Some(&'/') = iter.peek() {
                                    iter.next();
                                    break;
                                }
                            }
                            Some(_) => continue,
                            None => {
                                return Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
                                    ch: '/',
                                    pos: FilePos { start, end: iter.current_index() },
                                }));
                            }
                        }
                    }
                    continue;
                }
            }
        }
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
    InvalidUnicodeEscape(FilePos),
    UnicodeInCharLiteral(FilePos),
    NonAsciiCharLiteral(FilePos),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word { value: String },
    IntLit { value: i64 },
    StringLit { value: StringLit },
    CharLit { value: u8 },
    Op { value: String },
    Import { module: String },
    Let,
    Decl,
    Fn,
    Type,
    Return,
    If,
    Else,
    Match,
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
            Token::Match => "match".to_string(),
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
            "match" => Some(Token::Match),
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

/// Builds a number literal token, supporting both decimal and hexadecimal formats.
/// - Decimal numbers are sequences of digits (e.g., 123).
/// - Hexadecimal numbers must start with "0x" followed by hex digits (0-9, a-f, A-F).
fn tok_num(start: char, iter: &mut VecIter<char>) -> Result<Token, TokenizeError> {
    let mut word = String::new();
    let st = iter.current_index();

    // Check for hexadecimal prefix "0x"
    if start == '0' {
        if let Some('x' | 'X') = iter.peek() {
            iter.next(); // Consume the 'x' or 'X'

            // Read all subsequent hexadecimal digits
            while let Some(&next_ch) = iter.peek() {
                if next_ch.is_ascii_hexdigit() {
                    word.push(next_ch);
                    iter.next(); // Consume the hex digit
                } else {
                    break; // End of the hex number
                }
            }

            // After "0x", we must have at least one hex digit
            if word.is_empty() {
                return Err(TokenizeError::InvalidNumberCharacter(InvalidCharacter {
                    ch: iter.peek().copied().unwrap_or(' '),
                    pos: FilePos { start: st, end: iter.current_index() + 1 },
                }));
            }

            // Parse the hexadecimal string into a number
            let value = i64::from_str_radix(&word, 16).unwrap();
            return Ok(Token::IntLit { value });
        }
    }

    // --- Fallback to Decimal Parsing ---
    // If it's not a hex number, use the original logic for decimal numbers.
    word.push(start);

    while let Some(&next_ch) = iter.peek() {
        if next_ch.is_ascii_digit() {
            word.push(next_ch);
            iter.next(); // Consume the character
        } else if next_ch.is_alphabetic() {
            // Invalid character in a decimal number
            return Err(TokenizeError::InvalidNumberCharacter(InvalidCharacter {
                ch: next_ch,
                pos: FilePos { start: st, end: iter.current_index() + 1 },
            }));
        } else {
            break;
        }
    }
    
    // Parse the decimal string into a number
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
    let mut str_buf = String::new();
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
            // Flush the buffer before handling the escape sequence
            if !str_buf.is_empty() {
                res.push(StringFragment::String(str_buf.clone()));
                str_buf.clear();
            }
            let Some(esc) = iter.next() else {
                return Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
                    ch: '\\',
                    pos: FilePos {
                        start: iter.prev_index(),
                        end: iter.prev_index(),
                    },
                }));
            };
            res.push(esc_char(esc, iter)?);
        } else if next_ch.is_ascii() && !next_ch.is_ascii_control() {
            str_buf.push(next_ch);
        } else {
            // Flush the buffer before handling the Unicode character
            if !str_buf.is_empty() {
                res.push(StringFragment::String(str_buf.clone()));
                str_buf.clear();
            }
            res.push(StringFragment::Unicode(next_ch as u32));
        }
    }

    if !closed {
        Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
            ch: start,
            pos: FilePos { start: start_pos, end: start_pos },
        }))
    } else {
        // Final flush for any remaining characters
        if !str_buf.is_empty() {
            res.push(StringFragment::String(str_buf));
        }
        res.push(StringFragment::Byte(0)); // Null terminator
        Ok((Token::StringLit { value: StringLit { frags: res } }, interp))
    }
}

/// Builds a char literal until `start` appears
fn tok_char(start: char, iter: &mut VecIter<char>) -> Result<Token, TokenizeError> {
    let start_pos = iter.current_index();
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
            let frag = esc_char(esc, iter)?;
            match frag {
                StringFragment::Byte(b) => b,
                StringFragment::Unicode(_) => {
                    return Err(TokenizeError::UnicodeInCharLiteral(FilePos {
                        start: start_pos,
                        end: iter.current_index(),
                    }))
                }
                _ => unreachable!(), // Should not be possible to get a String fragment here
            }
        }
        None => {
            return Err(TokenizeError::UnclosedCharacter(InvalidCharacter {
                ch: start,
                pos: FilePos { start: start_pos, end: start_pos },
            }))
        }
        Some(val) => {
            if !val.is_ascii() {
                return Err(TokenizeError::NonAsciiCharLiteral(FilePos {
                    start: start_pos,
                    end: iter.current_index(),
                }));
            }
            val as u8
        }
    };
    let Some('\'') = iter.next() else {
        return Err(TokenizeError::UnclosedCharacter(InvalidCharacter { ch: start, pos: FilePos { start: start_pos, end: iter.current_index()} }));
    };
    Ok(Token::CharLit { value })
}

fn esc_char(esc: char, iter: &mut VecIter<char>) -> Result<StringFragment, TokenizeError> {
    match esc {
        'n' => Ok(StringFragment::Byte(b'\n')),
        'r' => Ok(StringFragment::Byte(b'\r')),
        't' => Ok(StringFragment::Byte(b'\t')),
        '\\' => Ok(StringFragment::Byte(b'\\')),
        '\'' => Ok(StringFragment::Byte(b'\'')),
        '"' => Ok(StringFragment::Byte(b'"')),
        '{' => Ok(StringFragment::Byte(b'{')),
        '}' => Ok(StringFragment::Byte(b'}')),
        '0' => Ok(StringFragment::Byte(b'\0')),
        'u' => {
            let start_pos = iter.prev_index();
            if iter.next() != Some('{') {
                return Err(TokenizeError::InvalidUnicodeEscape(FilePos { start: start_pos, end: iter.current_index() }));
            }
            let mut hex = String::new();
            let mut closed = false;
            while let Some(&c) = iter.peek() {
                if c == '}' {
                    iter.next(); // consume '}'
                    closed = true;
                    break;
                }
                if !c.is_ascii_hexdigit() {
                    return Err(TokenizeError::InvalidCharacter(InvalidCharacter {
                        ch: c,
                        pos: FilePos {
                            start: iter.current_index() + 1,
                            end: iter.current_index() + 1,
                        },
                    }));
                }
                hex.push(c);
                iter.next();
            }

            if !closed || hex.is_empty() || hex.len() > 6 {
                return Err(TokenizeError::InvalidUnicodeEscape(FilePos {
                    start: start_pos,
                    end: iter.current_index(),
                }));
            }

            let code_point = u32::from_str_radix(&hex, 16).map_err(|_| {
                TokenizeError::InvalidUnicodeEscape(FilePos {
                    start: start_pos,
                    end: iter.current_index(),
                })
            })?;
            std::char::from_u32(code_point).ok_or_else(|| {
                TokenizeError::InvalidUnicodeEscape(FilePos {
                    start: start_pos,
                    end: iter.current_index(),
                })
            })?;
            Ok(StringFragment::Unicode(code_point))
        }
        _ => Err(TokenizeError::InvalidCharacter(InvalidCharacter {
            ch: esc,
            pos: FilePos {
                start: iter.current_index(),
                end: iter.current_index(),
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
