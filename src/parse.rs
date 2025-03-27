use crate::helpers::VecIter;
use crate::node::{self, PosStr, UnExpr};
use crate::token::Token;
use crate::validate::SemanticError;

/// Parse tokens into the ast
pub fn parse(token_res: Vec<Token>) -> Result<Vec<node::Stmt>, ParseError> {
    let mut tokens = VecIter::new(token_res);
    let mut vec = Vec::new();
    while tokens.peek().is_some() {
        vec.push(parse_stmt(&mut tokens)?);
    }
    return Ok(vec);
}

#[derive(Debug)]
pub struct UnexpectedToken {
    pub token: Token,
    pub info_id: usize,
    pub expected: String,
}
#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(UnexpectedToken),
    UnexpectedEndOfInput(String),
    InvalidMacro(PosStr),
}

fn parse_stmt(tokens: &mut VecIter<Token>) -> Result<node::Stmt, ParseError> {
    match tokens.peek() {
        Some(Token::Let) => return Ok(node::Stmt::Let(parse_let(tokens)?)),
        Some(Token::Decl) => return Ok(node::Stmt::Decl(parse_decl(tokens)?)),
        Some(Token::Fn) => return Ok(node::Stmt::Fn(parse_fn_decl(tokens)?)),
        Some(Token::Return) => {
            tokens.next();
            let pos_id = tokens.prev_index();
            let expr = parse_opt_expr(tokens)?;
            check_semi(tokens)?;
            return Ok(node::Stmt::Ret(node::Ret { pos_id, expr }));
        }
        Some(Token::If) => return Ok(node::Stmt::If(parse_if(tokens)?)),
        Some(Token::Loop) => {
            tokens.next();
            return Ok(node::Stmt::Loop(node::Loop {
                pos_id: tokens.prev_index(),
                scope: parse_scope(tokens)?,
            }));
        }
        Some(Token::While) => {
            tokens.next();
            return Ok(node::Stmt::While(node::While {
                pos_id: tokens.prev_index(),
                expr: parse_expr(tokens, 0)?,
                scope: parse_scope(tokens)?
            }));
        }
        Some(Token::For) => return Ok(node::Stmt::For(parse_for(tokens)?)),
        Some(Token::Break) => {
            tokens.next();
            check_semi(tokens)?;
            return Ok(node::Stmt::Break(tokens.prev_index() - 1));
        }
        Some(Token::Continue) => {
            tokens.next();
            check_semi(tokens)?;
            return Ok(node::Stmt::Continue(tokens.prev_index() - 1));
        }
        _ => (),
    }
    let expr = parse_expr(tokens, 0)?;
    check_semi(tokens)?;
    Ok(node::Stmt::Expr(expr))
}

fn parse_opt_expr(tokens: &mut VecIter<Token>) -> Result<Option<node::Expr>, ParseError> {
    if let Some(Token::Semi) = tokens.peek() {
        return Ok(None);
    } else {
        return parse_expr(tokens, 0).map(|e| Some(e));
    }
}

fn parse_expr(tokens: &mut VecIter<Token>, min_prec: u8) -> Result<node::Expr, ParseError> {
    let mut root = parse_atom(tokens)?;
    loop {
        let peek = tokens.peek();
        if peek.is_none() {
            break;
        }
        let prec: u8;
        let opstr: String;
        if let Token::Op { value } = &peek.unwrap() {
            opstr = value.to_string();
            prec = op_prec(value.as_str());
            if prec < min_prec {
                break;
            }
        } else {
            break;
        }
        let op_pos = tokens.current_index();
        tokens.next();
        let rhs = parse_expr(tokens, prec + op_assoc(opstr.as_str()))?;
        let lhs = root;
        root = node::Expr::BinExpr(node::BinExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: PosStr { str: opstr, pos_id: op_pos },
        });
    }
    return Ok(root);
}

fn parse_atom(tokens: &mut VecIter<Token>) -> Result<node::Expr, ParseError> {
    let tok = check_none(tokens, "a term")?;
    match tok {
        Token::IntLit { value } => Ok(node::Expr::IntLit(PosStr {
            str: value,
            pos_id: tokens.prev_index(),
        })),
        Token::Word { value } => {
            if let Some(Token::OpenParen) = tokens.peek() {
                tokens.next();
                return Ok(node::Expr::Call(node::Call {
                    name: PosStr {
                        str: value,
                        pos_id: tokens.prev_index() - 1,
                    },
                    args: parse_args(tokens)?,
                }));
            } else if let Some(Token::Bang) = tokens.peek() {
                tokens.next();
                return parse_macro(
                    tokens,
                    PosStr {
                        str: value,
                        pos_id: tokens.prev_index() - 1,
                    },
                );
            }
            return Ok(node::Expr::Variable(PosStr {
                str: value,
                pos_id: tokens.prev_index(),
            }));
        }
        Token::OpenParen => {
            let res = parse_expr(tokens, 1)?;
            let tok = check_none(tokens, "')'")?;
            if let Token::CloseParen = tok {
                return Ok(res);
            } else {
                return Err(unexp(tok, tokens.prev_index(), "')'"));
            }
        }
        Token::Op { value } => Ok(node::Expr::UnExpr(UnExpr {
            op: PosStr {
                str: value,
                pos_id: tokens.prev_index(),
            },
            expr: Box::new(parse_atom(tokens)?),
        })),
        Token::OpenSquare => {
            let pos_id = tokens.current_index();
            let mut exprs = Vec::new();
            loop {
                exprs.push(parse_expr(tokens, 0)?);
                let tok = check_none(tokens, "']'")?;
                match tok {
                    Token::Comma => (),
                    Token::CloseSquare => break,
                    _ => return Err(unexp(tok, tokens.current_index(), "']'"))
                }
            }
            Ok(node::Expr::ArrLit(node::ArrLit {
                exprs, pos_id
            }))
        }
        _ => Err(unexp(tok, tokens.prev_index(), "a term")),
    }
}

fn parse_let(tokens: &mut VecIter<Token>) -> Result<node::Let, ParseError> {
    tokens.next();
    let tok = check_none(tokens, "an identifier")?;
    let Token::Word { value: name } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "an identifier"));
    };
    let tok = check_none(tokens, "'='")?;
    if let Token::Op { value } = &tok {
        if value != "=" {
            return Err(unexp(tok, tokens.prev_index(), "'='"));
        }
        let res = Ok(node::Let {
            name: PosStr {
                str: name,
                pos_id: tokens.prev_index() - 1,
            },
            expr: parse_expr(tokens, 0)?,
        });
        check_semi(tokens)?;
        return res;
    } else {
        return Err(unexp(tok, tokens.prev_index(), "'='"));
    }
}
fn parse_decl(tokens: &mut VecIter<Token>) -> Result<node::Decl, ParseError> {
    tokens.next();
    let tok = check_none(tokens, "an identifier")?;
    let Token::Word { value: name } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "an identifier"));
    };
    let tok = check_none(tokens, "':'")?;
    if let Token::Colon = &tok {
        let res = Ok(node::Decl {
            name: PosStr {
                str: name,
                pos_id: tokens.prev_index() - 1,
            },
            r#type: parse_type(tokens)?
        });
        check_semi(tokens)?;
        return res;
    } else {
        return Err(unexp(tok, tokens.prev_index(), "':'"));
    }
}

fn parse_args(tokens: &mut VecIter<Token>) -> Result<Vec<node::Expr>, ParseError> {
    let mut res = Vec::new();
    if tokens.peek().is_none() {
        return Err(ParseError::UnexpectedEndOfInput("')'".to_string()));
    }
    if let Token::CloseParen = tokens.peek().unwrap() {
        tokens.next();
        return Ok(res);
    }
    loop {
        res.push(parse_expr(tokens, 0)?);
        let tok = check_none(tokens, "')'")?;
        if let Token::CloseParen = tok {
            break;
        }
        if let Token::Comma = tok {
        } else {
            return Err(unexp(tok, tokens.prev_index(), "')'"));
        }
    }
    Ok(res)
}

fn parse_fn_decl(tokens: &mut VecIter<Token>) -> Result<node::Fn, ParseError> {
    tokens.next();
    let tok = check_none(tokens, "an identifier.")?;
    let Token::Word { value: name } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "an identifier"));
    };
    let name_pos = tokens.prev_index();
    let tok = check_none(tokens, "'('")?;
    let Token::OpenParen = tok else {
        return Err(unexp(tok, tokens.prev_index(), "'('"));
    };
    let type_args = parse_type_args(tokens)?;
    let decl_type;
    if let Some(Token::Op {value}) = tokens.peek() {
        if value == "->" {
            tokens.next();
            decl_type = Some(parse_type(tokens)?);
        } else {
            return Err(unexp(tokens.peek().unwrap().clone(), tokens.current_index(), "'->'"));
        }
    } else {
        decl_type = None;
    }
    Ok(node::Fn {
        name: PosStr { str: name, pos_id: name_pos },
        arg_names: type_args.0,
        arg_types: type_args.1,
        decl_type,
        scope: parse_scope(tokens)?,
    })
}

fn parse_type_args(tokens: &mut VecIter<Token>) -> Result<(Vec<PosStr>, Vec<node::Type>), ParseError> {
    let mut types = Vec::new();
    let mut names = Vec::new();
    if tokens.peek().is_none() {
        return Err(ParseError::UnexpectedEndOfInput("')'".to_string()));
    }
    if let Some(Token::CloseParen) = tokens.peek() {
        tokens.next();
        return Ok((names, types));
    }
    loop {
        let name = check_none(tokens, "an identifier")?;
        if let Token::Word { value } = name {
            names.push(PosStr {
                str: value,
                pos_id: tokens.prev_index(),
            });
        } else {
            return Err(unexp(name, tokens.prev_index(), "an identifier"));
        }
        let tok = check_none(tokens, "':'")?;
        if let Token::Colon = tok {
            types.push(parse_type(tokens)?);
        } else {
            return Err(unexp(tok, tokens.prev_index(), "':'"));
        }
        let tok = check_none(tokens, "')'")?;
        if let Token::CloseParen = tok {
            break;
        } else if let Token::Comma = tok {
        } else {
            return Err(unexp(tok, tokens.prev_index(), "')'"));
        }
    }
    Ok((names, types))
}

fn parse_scope(tokens: &mut VecIter<Token>) -> Result<Vec<node::Stmt>, ParseError> {
    let tok = check_none(tokens, "'{'")?;
    if let Token::OpenCurly = tok {
    } else {
        return Err(unexp(tok, tokens.prev_index(), "'{'"));
    }
    let mut res = Vec::new();
    loop {
        if tokens.peek().is_none() {
            return Err(ParseError::UnexpectedEndOfInput("'}'".to_string()));
        }
        if let Some(Token::CloseCurly) = tokens.peek() {
            tokens.next();
            break;
        }
        res.push(parse_stmt(tokens)?);
    }
    return Ok(res);
}

fn parse_if(tokens: &mut VecIter<Token>) -> Result<node::If, ParseError> {
    tokens.next();
    let mut res = node::If {
        pos_id: tokens.prev_index(),
        expr: Some(parse_expr(tokens, 0)?),
        scope: parse_scope(tokens)?,
        els: None,
    };
    if let Some(Token::Else) = tokens.peek() {
        tokens.next();
        if let Some(Token::If) = tokens.peek() {
            res.els = Some(Box::new(parse_if(tokens)?));
        } else {
            res.els = Some(Box::new(node::If {
                pos_id: tokens.prev_index(),
                expr: None,
                scope: parse_scope(tokens)?,
                els: None,
            }));
        }
    }
    Ok(res)
}

fn parse_for(tokens: &mut VecIter<Token>) -> Result<node::For, ParseError> {
    let pos_id = tokens.current_index();
    let init;
    tokens.next();
    if let Some(Token::Let) = tokens.peek() {
        init = node::LetOrExpr::Let(parse_let(tokens)?);
    } else {
        init = node::LetOrExpr::Expr(parse_expr(tokens, 0)?);
        check_semi(tokens)?;
    }
    let cond = parse_expr(tokens, 0)?;
    check_semi(tokens)?;
    let incr = parse_expr(tokens, 0)?;
    let scope = parse_scope(tokens)?;
    Ok(node::For { pos_id, init, cond, incr, scope})
}

fn parse_type(tokens: &mut VecIter<Token>) -> Result<node::Type, ParseError> {
    let tok = check_none(tokens, "a type")?;
    if let Token::Word { value } = tok {
        let str = PosStr {
            str: value,
            pos_id: tokens.prev_index(),
        };
        let mut sub = None;
        if let Some(Token::OpenSquare) = tokens.peek() {
            tokens.next();
            sub = Some(Box::new(parse_type(tokens)?));
            let cl = check_none(tokens, "']'")?;
            if let Token::CloseSquare = cl {
            } else {
                return Err(unexp(cl, tokens.prev_index(), "']'"));
            }
        }
        Ok(node::Type { str, sub })
    } else {
        Err(unexp(tok, tokens.prev_index(), "a type"))
    }
}

fn parse_macro(tokens: &mut VecIter<Token>, name: PosStr) -> Result<node::Expr, ParseError> {
    match name.str.as_str() {
        "salloc" => {
            let tok = check_none(tokens, "'('")?;
            if let Token::OpenParen = tok {
            } else {
                return Err(unexp(tok, tokens.prev_index(), "'('"));
            }
            let tok = check_none(tokens, "a count")?;
            let count;
            if let Token::IntLit { value } = tok {
                count = value.parse::<u32>().unwrap();
            } else {
                return Err(unexp(tok, tokens.prev_index(), "an integer"));
            }
            let tok = check_none(tokens, "','")?;
            if let Token::Comma = tok {
            } else {
                return Err(unexp(tok, tokens.prev_index(), "','"));
            }
            let ty = parse_type(tokens)?;
            let tok = check_none(tokens, "')'")?;
            if let Token::CloseParen = tok {
            } else {
                return Err(unexp(tok, tokens.prev_index(), "')'"));
            }
            return Ok(node::Expr::Macro(node::Macro::Salloc { count, ty }));
        }
        _ => Err(ParseError::InvalidMacro(name)),
    }
}

fn check_none(tokens: &mut VecIter<Token>, expected: &str) -> Result<Token, ParseError> {
    if tokens.peek().is_none() {
        Err(ParseError::UnexpectedEndOfInput(expected.to_string()))
    } else {
        Ok(tokens.next().unwrap())
    }
}

fn check_semi(tokens: &mut VecIter<Token>) -> Result<(), ParseError> {
    let tok = check_none(tokens, "';'")?;
    if let Token::Semi = tok {
        Ok(())
    } else {
        Err(unexp(tok, tokens.prev_index(), "';'"))
    }
}

fn unexp(token: Token, info_id: usize, expected: &str) -> ParseError {
    return ParseError::UnexpectedToken(UnexpectedToken {
        token,
        info_id,
        expected: expected.to_string(),
    });
}

fn op_prec(op: &str) -> u8 {
    match op {
        "=" => 0,  // Simple assignment
        "+=" => 0, // Assignment by sum and difference
        "-=" => 0, // Assignment by sum and difference
        "*=" => 0, // Assignment by product, quotient, and remainder
        "/=" => 0, // Assignment by product, quotient, and remainder
        "&=" => 0, // Assignment by bitwise AND
        "|=" => 0, // Assignment by bitwise OR
        "||" => 1, // Logical OR
        "&&" => 2, // Logical AND
        "|" => 3,  // Bitwise OR
        "&" => 4,  // Bitwise AND
        "==" => 5, // Equality
        "!=" => 5, // Not equal
        "<" => 6,  // Relational operators
        "<=" => 6,
        ">" => 6, // Relational operators
        ">=" => 6,
        "<<" => 7, // Bitwise left shift
        ">>" => 7, // Bitwise right shift
        "+" => 8,
        "-" => 8,
        "*" => 9,
        "/" => 9,
        "!" => 10, // Highest precedence
        _ => 0,
    }
}

fn op_assoc(op: &str) -> u8 {
    match op {
        "=" => 0,  // Right-associative (assignment)
        "+=" => 0, // Right-associative (assignment)
        "-=" => 0, // Right-associative (assignment)
        "*=" => 0, // Right-associative (assignment)
        "/=" => 0, // Right-associative (assignment)
        "&=" => 0, // Right-associative (assignment)
        "|=" => 0, // Right-associative (assignment)
        "||" => 1, // Left-associative (logical OR)
        "&&" => 1, // Left-associative (logical AND)
        "|" => 1,  // Left-associative (bitwise OR)
        "&" => 1,  // Left-associative (bitwise AND)
        "==" => 1, // Left-associative (equality)
        "!=" => 1, // Left-associative (not equal)
        "<" => 1,  // Left-associative (relational)
        "<=" => 1, // Left-associative (relational)
        ">" => 1,  // Left-associative (relational)
        ">=" => 1, // Left-associative (relational)
        "<<" => 1, // Left-associative (bitwise shift)
        ">>" => 1, // Left-associative (bitwise shift)
        "+" => 1,  // Left-associative (addition)
        "-" => 1,  // Left-associative (subtraction)
        "*" => 1,  // Left-associative (multiplication)
        "/" => 1,  // Left-associative (division)
        "!" => 0,  // Right-associative (unary NOT)
        _ => 0,    // Default to right-associative if unknown
    }
}
