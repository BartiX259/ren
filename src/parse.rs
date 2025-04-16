use crate::helpers::VecIter;
use crate::node::{self, ExprKind, PosStr, Span};
use crate::tokenize::Token;

/// Parse tokens into the ast
pub fn parse(token_res: Vec<Token>, parent: Option<String>) -> Result<(Vec<node::Stmt>, Vec<node::Import>), ParseError> {
    let mut tokens = VecIter::new(token_res);
    let mut vec = Vec::new();
    let mut imports = Vec::new();
    while let Some(Token::Import { module }) = tokens.peek() {
        imports.push(node::Import { path: module.clone(), parent: parent.clone(), pos_id: tokens.current_index() });
        tokens.next();
    }
    while tokens.peek().is_some() {
        vec.push(parse_stmt(&mut tokens)?);
    }
    Ok((vec, imports))
}

#[derive(Debug)]
pub struct UnexpectedToken {
    pub token: Token,
    pub pos_id: usize,
    pub expected: String,
}
#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(UnexpectedToken),
    UnexpectedEndOfInput(String),
    ImportNotAtStart(usize)
}

fn parse_stmt(tokens: &mut VecIter<Token>) -> Result<node::Stmt, ParseError> {
    match tokens.peek() {
        Some(Token::Let) => return Ok(node::Stmt::Let(parse_let(tokens)?)),
        Some(Token::Decl) => return Ok(node::Stmt::Decl(parse_decl(tokens)?)),
        Some(Token::Fn) => return Ok(node::Stmt::Fn(parse_fn_decl(tokens)?)),
        Some(Token::Type) => return Ok(node::Stmt::TypeDecl(parse_type_decl(tokens)?)),
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
        Some(Token::Syscall) => return Ok(node::Stmt::Syscall(parse_syscall(tokens)?)),
        Some(Token::Import { .. }) => return Err(ParseError::ImportNotAtStart(tokens.current_index())),
        _ => (),
    }
    let expr = parse_expr(tokens, 0)?;
    check_semi(tokens)?;
    Ok(node::Stmt::Expr(expr))
}

fn expr(start: usize, end: usize, kind: ExprKind) -> node::Expr {
    node::Expr { kind, ty: crate::types::Type::Void, span: Span { start, end } }
}

fn parse_opt_expr(tokens: &mut VecIter<Token>) -> Result<Option<node::Expr>, ParseError> {
    if let Some(Token::Semi) = tokens.peek() {
        return Ok(None);
    } else {
        return parse_expr(tokens, 0).map(|e| Some(e));
    }
}

fn parse_expr(tokens: &mut VecIter<Token>, min_prec: u8) -> Result<node::Expr, ParseError> {
    let start = tokens.current_index();
    let mut root = parse_atom(tokens)?;
    loop {
        let peek = tokens.peek();
        if peek.is_none() {
            break;
        }
        let prec: u8;
        let opstr: String;
        let mut unclosed = None;
        let tok = peek.unwrap();
        if let Token::Op { value } = tok {
            opstr = value.to_string();
            prec = op_prec(value.as_str());
        } else if let Token::OpenSquare = tok {
            opstr = "[]".to_string();
            prec = op_prec("[]");
            unclosed = Some((Token::CloseSquare, "']'"));
        } else if let Token::Dot = tok {
            opstr = ".".to_string();
            prec = op_prec(".");
        } else if let Token::As = tok {
            tokens.next();
            let ty = parse_type(tokens)?;
            root = expr(start, tokens.prev_index(), node::ExprKind::TypeCast(node::TypeCast {
                r#type: ty,
                expr: Box::new(root),
            }));
            continue;
        } else {
            break;
        }
        if prec < min_prec {
            break;
        }
        let op_pos = tokens.current_index();
        tokens.next();
        let rhs = parse_expr(tokens, prec + op_assoc(opstr.as_str()))?;
        if let Some(unc) = unclosed {
            let tok = check_none(tokens, unc.1)?;
            if unc.0 != tok {
                return Err(unexp(tok, tokens.current_index(), unc.1));
            }
        }
        let lhs = root;
        root = expr(start, tokens.prev_index(), 
        node::ExprKind::BinExpr(node::BinExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: PosStr { str: opstr, pos_id: op_pos },
        }));
    }
    return Ok(root);
}

fn parse_atom(tokens: &mut VecIter<Token>) -> Result<node::Expr, ParseError> {
    let tok = check_none(tokens, "a term")?;
    let start = tokens.prev_index();
    match tok {
        Token::IntLit { value } => Ok(expr(start, start,  node::ExprKind::IntLit(value))),
        Token::CharLit { value } => Ok(expr(start, start,  node::ExprKind::CharLit(value))),
        Token::True => Ok(expr(start, start, node::ExprKind::BoolLit(true))),
        Token::False => Ok(expr(start, start, node::ExprKind::BoolLit(false))),
        Token::Null => Ok(expr(start, start, node::ExprKind::Null)),
        Token::Word { value } => { // Term
            let pos_str = PosStr {
                str: value,
                pos_id: tokens.prev_index(),
            };
            if let Some(Token::OpenParen) = tokens.peek() {
                tokens.next();
                let kind = node::ExprKind::Call(node::Call {
                    name: pos_str,
                    args: parse_args(tokens)?,
                });
                return Ok(expr(start, tokens.prev_index(), kind));
            }
            return Ok(expr(start, start, node::ExprKind::Variable(pos_str)));
        }
        Token::OpenParen => { // Brackets
            if let Some(Token::Word { .. }) = tokens.peek() {
                if let Some(Token::Colon) = tokens.peek2() { // Struct
                    let mut field_names = Vec::new();
                    let mut field_exprs = Vec::new();
                    loop {
                        let tok = check_none(tokens, "a field name")?;
                        let Token::Word { value } = tok else {
                            return Err(unexp(tok, tokens.prev_index(), "a field name"));
                        };
                        field_names.push(PosStr { str: value, pos_id: tokens.prev_index() });
                        let col = check_none(tokens, "':'")?;
                        let Token::Colon = col else {
                            return Err(unexp(col, tokens.prev_index(), "':'"));
                        };
                        let res = parse_expr(tokens, 0)?;
                        field_exprs.push(res);
                        let tok = check_none(tokens, "')'")?;
                        if let Token::CloseParen = tok {
                            return Ok(expr(start, tokens.prev_index(), node::ExprKind::StructLit(node::StructLit { field_names, field_exprs })));
                        } else if let Token::Comma = tok {
                            continue;
                        } else {
                            return Err(unexp(tok, tokens.prev_index(), "')'"));
                        }
                    }
                }
            }
            let mut tuple = Vec::new();
            loop {
                let res = parse_expr(tokens, 1)?;
                let tok = check_none(tokens, "')'")?;
                if let Token::CloseParen = tok {
                    if tuple.len() > 0 {
                        tuple.push(res);
                        return Ok(expr(start, tokens.prev_index(), node::ExprKind::TupleLit(tuple)));
                    }
                    return Ok(res);
                } else if let Token::Comma = tok {
                    tuple.push(res);
                } else {
                    return Err(unexp(tok, tokens.prev_index(), "')'"));
                }
            }
        }
        Token::Op { value } => { // Unary expression
            let kind = node::ExprKind::UnExpr(node::UnExpr {
                op: PosStr {
                    str: value,
                    pos_id: tokens.prev_index(),
                },
                expr: Box::new(parse_atom(tokens)?),
            });
            Ok(expr(start, tokens.prev_index(), kind))
        }
        Token::OpenSquare => { // Array literal
            let pos_id = tokens.current_index();
            let mut exprs = Vec::new();
            if let Some(Token::CloseSquare) = tokens.peek() {
                tokens.next();
                return Ok(expr(start, tokens.prev_index(), node::ExprKind::ArrLit(node::ArrLit {
                    exprs, pos_id
                })));
            }
            loop {
                exprs.push(parse_expr(tokens, 0)?);
                let tok = check_none(tokens, "']'")?;
                match tok {
                    Token::Comma => (),
                    Token::CloseSquare => break,
                    _ => return Err(unexp(tok, tokens.prev_index(), "']'"))
                }
            }
            Ok(expr(start, tokens.prev_index(), node::ExprKind::ArrLit(node::ArrLit {
                exprs, pos_id
            })))
        }
        Token::StringLit { value } => Ok(expr(start, tokens.prev_index(), node::ExprKind::StringLit(value))),
        // Token::DoubleQuote => { // String literal
        //     let pos_id = tokens.current_index();
        // }
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
            r#type: parse_type(tokens)?,
            ty: crate::types::Type::Void
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
    Ok(res)
}

fn parse_types_list(tokens: &mut VecIter<Token>, closing: Token) -> Result<Vec<node::Type>, ParseError> {
    let mut types = Vec::new();
    let close_str = format!("'{}'", closing.to_string());
    if tokens.peek().is_none() {
        return Err(ParseError::UnexpectedEndOfInput(close_str));
    }
    if let Some(t) = tokens.peek() {
        if *t == closing {
            tokens.next();
            return Ok(types);
        }
    }
    loop {
        types.push(parse_type(tokens)?);
        let tok = check_none(tokens, &close_str)?;
        if closing == tok {
            break;
        } else if let Token::Comma = tok {
            continue;
        } else {
            return Err(unexp(tok, tokens.prev_index(), &close_str));
        }
    }
    Ok(types)
}

fn parse_type_args(tokens: &mut VecIter<Token>, closing: Token) -> Result<(Vec<PosStr>, Vec<node::Type>), ParseError> {
    let mut types = Vec::new();
    let mut names = Vec::new();
    let close_str = format!("'{}'", closing.to_string());
    if tokens.peek().is_none() {
        return Err(ParseError::UnexpectedEndOfInput(close_str));
    }
    if let Some(t) = tokens.peek() {
        if *t == closing {
            tokens.next();
            return Ok((names, types));
        }
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
        let tok = check_none(tokens, &close_str)?;
        if closing == tok {
            break;
        } else if let Token::Comma = tok {
            continue;
        } else {
            return Err(unexp(tok, tokens.prev_index(), &close_str));
        }
    }
    Ok((names, types))
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
    let type_args = parse_type_args(tokens, Token::CloseParen)?;
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

fn parse_type_decl(tokens: &mut VecIter<Token>) -> Result<node::TypeDecl, ParseError> {
    tokens.next();
    let tok = check_none(tokens, "an identifier.")?;
    let Token::Word { value: name } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "an identifier"));
    };
    let name_pos = tokens.prev_index();
    let tok = check_none(tokens, "'='")?;
    let Token::Op { value: op } = &tok else {
        return Err(unexp(tok, tokens.prev_index(), "'='"));
    };
    if op != "=" {
        return Err(unexp(tok, tokens.prev_index(), "'='"));
    }
    let r#type = parse_type(tokens)?;
    check_semi(tokens)?;
    Ok(node::TypeDecl {
        name: PosStr { str: name, pos_id: name_pos },
        r#type
    })
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

fn r#type(start: usize, end: usize, kind: node::TypeKind) -> node::Type {
    node::Type { kind, span: Span { start, end } }
}

fn parse_type(tokens: &mut VecIter<Token>) -> Result<node::Type, ParseError> {
    let start = tokens.current_index();
    let tok = check_none(tokens, "a type")?;
    if let Token::Word { value } = &tok {
        let ty = r#type(start, start, node::TypeKind::Word(value.clone()));
        if let Some(Token::OpenSquare) = tokens.peek() {
            let mut len = None;
            tokens.next();
            if let Some(Token::IntLit { value }) = tokens.peek() {
                len = Some(value.clone());
                tokens.next();
            }
            let cl = check_none(tokens, "']'")?;
            let Token::CloseSquare = cl else {
                return Err(unexp(cl, tokens.prev_index(), "']'"));
            };
            Ok(r#type(start, tokens.prev_index(), node::TypeKind::Array(Box::new(ty), len)))
        } else {
            Ok(ty)
        }
    } else if let Token::Op { value } = &tok {
        if value == "*" {
            Ok(r#type(start, tokens.prev_index(), node::TypeKind::Pointer(Box::new(parse_type(tokens)?))))
        } else {
            Err(unexp(tok, tokens.prev_index(), "a type"))
        }
    } else if let Token::OpenParen = &tok {
        if let Some(Token::Word { .. }) = tokens.peek() {
            if let Some(Token::Colon) = tokens.peek2() { // Struct
                let (names, types) = parse_type_args(tokens, Token::CloseParen)?;
                return Ok(r#type(start, tokens.prev_index(), node::TypeKind::Struct(names, types)));
            }
        }
        let list = parse_types_list(tokens, Token::CloseParen)?;
        Ok(r#type(start, tokens.prev_index(), node::TypeKind::Tuple(list)))
    } else {
        Err(unexp(tok, tokens.prev_index(), "a type"))
    }
}

fn parse_syscall(tokens: &mut VecIter<Token>) -> Result<node::Syscall, ParseError> {
    tokens.next();
    let tok = check_none(tokens, "the syscall id")?;
    let Token::IntLit { value: id } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "the syscall id"));
    };
    let tok = check_none(tokens, "':'")?;
    let Token::Colon = tok else {
        return Err(unexp(tok, tokens.prev_index(), "':'"));
    };
    let tok = check_none(tokens, "the syscall name")?;
    let Token::Word { value: name_str } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "the syscall name"));
    };
    let name = PosStr { str: name_str, pos_id: tokens.prev_index() };
    let tok = check_none(tokens, "'('")?;
    let Token::OpenParen = tok else {
        return Err(unexp(tok, tokens.prev_index(), "'('"));
    };
    let types = parse_types_list(tokens, Token::CloseParen)?;
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
    check_semi(tokens)?;
    Ok(node::Syscall { id, name, types, decl_type })
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

fn unexp(token: Token, pos_id: usize, expected: &str) -> ParseError {
    return ParseError::UnexpectedToken(UnexpectedToken {
        token,
        pos_id,
        expected: expected.to_string(),
    });
}

fn op_prec(op: &str) -> u8 {
    match op {
        "," => 0,
        "=" => 1,  // Lowest precedence (done last)
        "+=" => 1,
        "-=" => 1,
        "*=" => 1,
        "/=" => 1,
        "%=" => 1,
        "|=" => 1,
        "^=" => 1,
        "&=" => 1,
        "<<=" => 1,
        ">>=" => 1,
        "||" => 2,
        "&&" => 3,
        "|" => 4,
        "^" => 5,
        "&" => 6,
        "==" => 7,
        "!=" => 7,
        "<" => 8,
        "<=" => 8,
        ">" => 8,
        ">=" => 8,
        "<<" => 9,
        ">>" => 9,
        "+" => 10,
        "-" => 10,
        "*" => 11,
        "/" => 11,
        "%" => 11,
        "!" => 12,
        "[]" => 13,
        "." => 14,
        "as" => 15, // Highest precedence (done first)
        _ => panic!("No precedence for operator {}", op),
    }
}

fn op_assoc(op: &str) -> u8 {
    match op {
        "=" => 0,  // Right to left
        "+=" => 0,
        "-=" => 0,
        "*=" => 0,
        "/=" => 0,
        "%=" => 0,
        "|=" => 0,
        "^=" => 0,
        "&=" => 0,
        "<<=" => 0,
        ">>=" => 0,
        "||" => 1, // Left to right
        "&&" => 1,
        "|" => 1,
        "^" => 1,
        "&" => 1,
        "==" => 1,
        "!=" => 1,
        "<" => 1,
        "<=" => 1,
        ">" => 1,
        ">=" => 1,
        "<<" => 1,
        ">>" => 1,
        "+" => 1,
        "-" => 1,
        "*" => 1,
        "/" => 1,
        "%" => 1,
        "." => 1,
        "[]" => 1,
        "," => 1,
        _ => panic!("No associativity for operator {}", op),
    }
}
