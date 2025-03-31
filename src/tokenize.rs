use crate::error::FilePos;
use crate::helpers::VecIter;
use std::vec::Vec;

/// Tokenize the input file
pub fn tokenize(input: &String) -> Result<(Vec<Token>, Vec<FilePos>), TokenizeError> {
    let mut tokens = Vec::new();
    let mut info = Vec::new();
    let mut iter = VecIter::new(input.chars().collect());
    let mut start;
    while let Some(ch) = iter.next() {
        start = iter.current_index();
        if let Some(ch_tok) = Token::from_char(ch) {
            tokens.push(ch_tok);
        } else if ch.is_alphabetic() {
            tokens.push(tok_word(ch, &mut iter)?);
        } else if ch.is_ascii_digit() {
            tokens.push(tok_num(ch, &mut iter)?);
        } else if is_operator(ch) {
            tokens.push(tok_op(ch, &mut iter));
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

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word { value: String },
    IntLit { value: i64 },
    Op { value: String },
    Let,
    Decl,
    Fn,
    Struct,
    Return,
    If,
    Else,
    Loop,
    While,
    For,
    Break,
    Continue,
    Semi,
    Colon,
    Bang,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    Comma,
    Dot
}

impl Token {
    pub fn to_string(&self) -> String {
        match self {
            Token::Word { value } => value.clone(),
            Token::IntLit { value } => value.to_string(),
            Token::Op { value } => value.clone(),
            Token::Let => "let".to_string(),
            Token::Decl => "decl".to_string(),
            Token::Fn => "fn".to_string(),
            Token::Struct => "struct".to_string(),
            Token::Return => "return".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Loop => "loop".to_string(),
            Token::While => "while".to_string(),
            Token::For => "for".to_string(),
            Token::Break => "break".to_string(),
            Token::Continue => "continue".to_string(),
            Token::Semi => ";".to_string(),
            Token::Colon => ":".to_string(),
            Token::Bang => "!".to_string(),
            Token::OpenParen => "(".to_string(),
            Token::CloseParen => ")".to_string(),
            Token::OpenCurly => "{".to_string(),
            Token::CloseCurly => "}".to_string(),
            Token::OpenSquare => "[".to_string(),
            Token::CloseSquare => "]".to_string(),
            Token::Comma => ",".to_string(),
            Token::Dot => ".".to_string(),
        }
    }

    pub fn from_char(ch: char) -> Option<Self> {
        match ch {
            ';' => Some(Token::Semi),
            ':' => Some(Token::Colon),
            '!' => Some(Token::Bang),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenCurly),
            '}' => Some(Token::CloseCurly),
            '[' => Some(Token::OpenSquare),
            ']' => Some(Token::CloseSquare),
            ',' => Some(Token::Comma),
            '.' => Some(Token::Dot),
            _ => None,
        }
    }
    pub fn from_string(str: &String) -> Option<Self> {
        match str.as_str() {
            "let" => Some(Token::Let),
            "decl" => Some(Token::Decl),
            "fn" => Some(Token::Fn),
            "struct" => Some(Token::Struct),
            "return" => Some(Token::Return),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "loop" => Some(Token::Loop),
            "while" => Some(Token::While),
            "for" => Some(Token::For),
            "break" => Some(Token::Break),
            "continue" => Some(Token::Continue),
            _ => None,
        }
    }
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
    return Ok(Token::Word { value: word });
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
    return Ok(Token::IntLit { value: word.parse::<i64>().unwrap() });
}

/// Builds an operator token, supporting both single-character and two-character operators.
fn tok_op(start: char, iter: &mut VecIter<char>) -> Token {
    let mut op = String::new();
    op.push(start);

    if let Some(&next_ch) = iter.peek() {
        // Check for two-character operators like +=, ==, etc.
        if is_operator_pair(start, next_ch) {
            op.push(next_ch);
            iter.next(); // Consume the second character
        }
    }

    return Token::Op { value: op };
}

/// Checks if a character is a valid single-character operator.
fn is_operator(ch: char) -> bool {
    ['+', '-', '*', '/', '=', '<', '>', '&', '|', '!'].contains(&ch)
}

/// Checks if a pair of characters form a valid two-character operator.
fn is_operator_pair(first: char, second: char) -> bool {
    match (first, second) {
        ('+', '=') |
        ('-', '=') |
        ('*', '=') |
        ('/', '=') |
        ('=', '=') |
        ('<', '=') |
        ('>', '=') |
        ('!', '=') |
        ('&', '&') |
        ('|', '|') |
        ('-', '>')
        => true,
        _ => false,
    }
}
