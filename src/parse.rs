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
        Some(Token::Let) => Ok(node::Stmt::Let(parse_let(tokens)?)),
        Some(Token::Decl) => Ok(node::Stmt::Decl(parse_decl(tokens)?)),
        Some(Token::Fn) => {
            tokens.next();
            let tok = check_none(tokens, "a function name")?;
            let Token::Word { value } = tok else {
                return Err(unexp(tok, tokens.prev_index(), "a function name"));
            };
            if value == "main" {
                Ok(node::Stmt::MainFn(parse_main(tokens)?))
            } else {
                Ok(node::Stmt::Fn(parse_fn_decl(tokens, value)?))
            }
        }
        Some(Token::Type) => Ok(node::Stmt::TypeDecl(parse_type_decl(tokens)?)),
        Some(Token::Enum) => Ok(node::Stmt::Enum(parse_enum(tokens)?)),
        Some(Token::Pub) => Ok(node::Stmt::Decorator(parse_decorator(tokens)?)),
        Some(Token::Return) => {
            tokens.next();
            let pos_id = tokens.prev_index();
            let expr = parse_opt_expr(tokens)?;
            check_semi(tokens)?;
            Ok(node::Stmt::Ret(node::Ret { pos_id, expr }))
        }
        Some(Token::If) => Ok(node::Stmt::If(parse_if(tokens)?)),
        Some(Token::Loop) => {
            tokens.next();
            Ok(node::Stmt::Loop(node::Loop {
                pos_id: tokens.prev_index(),
                scope: parse_scope(tokens)?,
            }))
        }
        Some(Token::While) => {
            tokens.next();
            Ok(node::Stmt::While(node::While {
                pos_id: tokens.prev_index(),
                expr: parse_expr(tokens, 0)?,
                scope: parse_scope(tokens)?
            }))
        }
        Some(Token::For) => return Ok(parse_for(tokens)?),
        Some(Token::Break) => {
            tokens.next();
            check_semi(tokens)?;
            Ok(node::Stmt::Break(tokens.prev_index() - 1))
        }
        Some(Token::Continue) => {
            tokens.next();
            check_semi(tokens)?;
            Ok(node::Stmt::Continue(tokens.prev_index() - 1))
        }
        Some(Token::Syscall) => Ok(node::Stmt::Syscall(parse_syscall(tokens)?)),
        Some(Token::Import { .. }) => Err(ParseError::ImportNotAtStart(tokens.current_index())),
        _ => {
            let expr = parse_expr(tokens, 0)?;
            check_semi(tokens)?;
            Ok(node::Stmt::Expr(expr))
        }
    }
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
        let tok = peek.unwrap();
        if let Token::Op { value } = tok {
            opstr = value.to_string();
            if ["?", "!"].contains(&value.as_str()) {
                tokens.next();
                root = expr(start, tokens.prev_index(), node::ExprKind::PostUnExpr(node::UnExpr { expr: Box::new(root), op: PosStr { str: opstr, pos_id: tokens.prev_index() } }));
                continue;
            }
            prec = op_prec(value.as_str());
            if prec == u8::MAX {
                return Err(unexp(tok.clone(), tokens.current_index(), "a valid binary or post-unary operator"));
            }
        } else if let Token::OpenSquare = tok {
            root = parse_array_access(tokens, root, start)?;
            continue;
        } else if let Token::As = tok {
            let prec = op_prec("as");
            if prec < min_prec {
                break;
            }
            tokens.next();
            let ty = parse_type(tokens)?;
            let lhs = root;
            root = expr(start, tokens.prev_index(), node::ExprKind::TypeCast(node::TypeCast {
                r#type: ty,
                expr: Box::new(lhs),
            }));
            continue;
        } else if let Token::Else = tok {
            let prec = op_prec("else");
            if prec < min_prec {
                break;
            }
            tokens.next();
            let pos_str = PosStr {
                str: "else".to_string(),
                pos_id: tokens.prev_index()
            };
            let is_scope = if let Some(Token::OpenCurly) = tokens.peek() { true }
            else if let Some(Token::OpenCurly) = tokens.peek2() { true }
            else { false };
            if is_scope {
                let mut capture = None;
                if let Some(Token::Word { value }) = tokens.peek() {
                    capture = Some(PosStr { str: value.clone(), pos_id: tokens.current_index() });
                    tokens.next();
                }
                let scope = parse_scope(tokens)?;
                let lhs = root;
                root = expr(start, tokens.prev_index(), node::ExprKind::ElseScope(node::ElseScope {
                    expr: Box::new(lhs),
                    pos_str,
                    capture,
                    scope
                }));
            } else {
                let lhs = root;
                root = expr(start, tokens.prev_index(), node::ExprKind::ElseExpr(node::ElseExpr {
                    expr: Box::new(lhs),
                    pos_str,
                    else_expr: Box::new(parse_expr(tokens, 0)?)
                }));
            }
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
                str: value.clone(),
                pos_id: tokens.prev_index(),
            };
            if let Some(Token::OpenParen) = tokens.peek() {
                tokens.next();
                let kind = match value.as_str() {
                    "len" | "sp" | "copy" | "sizeof" | "param" => {
                        let args = parse_args(tokens)?;
                        node::ExprKind::BuiltIn(node::BuiltIn { 
                            kind: match value.as_str() {
                                "len" => node::BuiltInKind::Len,
                                "sp" => node::BuiltInKind::StackPointer,
                                "copy" => node::BuiltInKind::Copy,
                                "sizeof" => node::BuiltInKind::Sizeof,
                                "param" => node::BuiltInKind::Param,
                                _ => unreachable!()
                            },
                            args
                        })
                    }
                    _ => node::ExprKind::Call(node::Call {
                        name: pos_str,
                        args: parse_args(tokens)?,
                    })
                };
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
        Token::StringLit { value } => {
            let mut res = vec![node::StringFragment::Lit(value)];
            while let Some(Token::StringInterpolationStart) = tokens.peek() {
                tokens.next();
                res.push(node::StringFragment::Expr {expr: parse_expr(tokens, 0)?, str_fn: "".to_string() });
                let tok = check_none(tokens, "'}'")?;
                let Token::StringInterpolationEnd = tok else {
                    return Err(unexp(tok, tokens.prev_index(), "'}'"));
                };
                if let Some(Token::StringLit { value }) = tokens.peek() {
                    res.push(node::StringFragment::Lit(value.clone()));
                    tokens.next();
                }
            }
            Ok(expr(start, tokens.prev_index(), node::ExprKind::StringLit(res, "".to_string())))
        }
        _ => Err(unexp(tok, tokens.prev_index(), "a term")),
    }
}

fn parse_array_access(tokens: &mut VecIter<Token>, root: node::Expr, start: usize) -> Result<node::Expr, ParseError> {
    tokens.next(); // consume `[`
    
    let mut lhs: Option<node::Expr> = None;
    let mut rhs: Option<node::Expr> = None;

    if tokens.peek() != Some(&Token::Op { value: "..".to_string() }) {
        lhs = Some(parse_expr(tokens, op_prec("..") + 1)?);
        if tokens.peek() != Some(&Token::Op { value: "..".to_string() }) {
            let access_expr = expr(start, tokens.prev_index(), node::ExprKind::BinExpr(node::BinExpr {
                lhs: Box::new(root),
                rhs: Box::new(lhs.unwrap()),
                op: PosStr { str: "[]".to_string(), pos_id: tokens.prev_index() },
            }));
            let tok = check_none(tokens, "']'")?;
            let Token::CloseSquare = tok else {
                return Err(unexp(tok, tokens.prev_index(), "']'"));
            };
    
            return Ok(access_expr)
        }
    }

    tokens.next(); // consume '..'

    if tokens.peek() != Some(&Token::CloseSquare) {
        rhs = Some(parse_expr(tokens, 0)?);
    }

    let tok = check_none(tokens, "']'")?;
    let Token::CloseSquare = tok else {
        return Err(unexp(tok, tokens.prev_index(), "']'"));
    };

    // Build missing sides
    let start_expr = lhs.unwrap_or_else(|| expr(start, tokens.prev_index(), node::ExprKind::IntLit(0)));
    let end_expr = rhs.unwrap_or_else(|| expr(start, tokens.prev_index(), node::ExprKind::BuiltIn(node::BuiltIn {
        kind: node::BuiltInKind::Len,
        args: vec![root.clone()],
    })));

    let range_expr = expr(start, tokens.prev_index(), node::ExprKind::BinExpr(node::BinExpr {
        lhs: Box::new(start_expr),
        rhs: Box::new(end_expr),
        op: PosStr { str: "..".to_string(), pos_id: tokens.prev_index() },
    }));

    let access_expr = expr(start, tokens.prev_index(), node::ExprKind::BinExpr(node::BinExpr {
        lhs: Box::new(root),
        rhs: Box::new(range_expr),
        op: PosStr { str: "[]".to_string(), pos_id: tokens.prev_index() },
    }));

    Ok(access_expr)
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

fn parse_main(tokens: &mut VecIter<Token>) -> Result<node::MainFn, ParseError> {
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
    Ok(node::MainFn {
        pos_id: name_pos,
        arg_names: type_args.0,
        arg_types: type_args.1,
        decl_type,
        scope: parse_scope(tokens)?,
    })
}

fn parse_fn_decl(tokens: &mut VecIter<Token>, name: String) -> Result<node::Fn, ParseError> {
    let mut generics = vec![];
    let name_pos = tokens.prev_index();
    let mut tok = check_none(tokens, "'('")?;
    if let Token::Op { value } = &tok {
        if value == "<" {
            loop {
                tok = check_none(tokens, "a generic identifier")?;
                let Token::Word { value } = tok else {
                    return Err(unexp(tok, tokens.prev_index(), "a generic identifier"));
                };
                generics.push(value);
                tok = check_none(tokens, "'>'")?;
                if let Token::Comma = &tok {
                    continue;
                } else if let Token::Op { value } = &tok {
                    if value == ">" {
                        break;
                    } else {
                        return Err(unexp(tok, tokens.prev_index(), "'>'"));
                    }
                } else {
                    return Err(unexp(tok, tokens.prev_index(), "'>'"));
                }
            }
            tok = check_none(tokens, "'('")?;
        }
    }
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
        generics,
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

fn parse_enum(tokens: &mut VecIter<Token>) -> Result<node::Enum, ParseError> {
    tokens.next();
    let tok = check_none(tokens, "a word")?;
    let Token::Word { value } = tok else {
        return Err(unexp(tok, tokens.prev_index(), "a word"));
    };
    let name = PosStr { str: value, pos_id: tokens.prev_index() };
    let tok = check_none(tokens, "'{'")?;
    let Token::OpenCurly = tok else {
        return Err(unexp(tok, tokens.prev_index(), "'{'"));
    };
    let mut variants = Vec::new();
    loop {
        let pos_str;
        match tokens.peek() {
            Some(Token::Word { value }) => {
                pos_str = PosStr {
                    str: value.to_string(),
                    pos_id: tokens.current_index()
                };
            }
            _ => break
        }
        tokens.next();
        let mut ty = None;
        if let Some(Token::OpenParen) = tokens.peek() {
            tokens.next();
            ty = Some(parse_type(tokens)?);
            let tok = check_none(tokens, "')'")?;
            let Token::CloseParen = tok else {
                return Err(unexp(tok, tokens.prev_index(), "')'"));
            };
        }
        variants.push((pos_str, ty));
        let Some(Token::Comma) = tokens.peek() else {
            break;
        };
        tokens.next();
    }
    let tok = check_none(tokens, "'}'")?;
    let Token::CloseCurly = tok else {
        return Err(unexp(tok, tokens.prev_index(), "'}'"));
    };
    Ok(node::Enum { name, variants })
}

fn parse_decorator(tokens: &mut VecIter<Token>) -> Result<node::Decorator, ParseError> {
    let mut kinds = Vec::new();
    let mut pos_ids = Vec::new();
    let start = tokens.current_index();
    loop {
        match tokens.peek() {
            Some(Token::Pub) => {
                tokens.next();
                kinds.push(node::DecoratorKind::Pub);
                pos_ids.push(tokens.prev_index());
            }
            _ => break
        }
    }
    Ok(node::Decorator {
        kinds,
        pos_ids,
        span: Span { start, end: tokens.prev_index() },
        inner: Box::new(parse_stmt(tokens)?)
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

fn parse_for(tokens: &mut VecIter<Token>) -> Result<node::Stmt, ParseError> {
    let pos_id = tokens.current_index();
    tokens.next();
    if let Some(Token::In) = tokens.peek2() {
        let tok = check_none(tokens, "a word")?;
        let Token::Word { value } = tok else {
            return Err(unexp(tok, tokens.prev_index(), "a word"));
        };
        let capture = PosStr {
            str: value,
            pos_id: tokens.prev_index()
        };
        tokens.next(); // 'in'
        let expr = parse_expr(tokens, 0)?;
        let scope = parse_scope(tokens)?;
        return Ok(node::Stmt::ForIn(node::ForIn { pos_id, capture, expr, scope }));
    }
    let init;
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
    Ok(node::Stmt::For(node::For { pos_id, init, cond, incr, scope}))
}

fn r#type(start: usize, end: usize, kind: node::TypeKind) -> node::Type {
    node::Type { kind, span: Span { start, end } }
}

fn parse_type(tokens: &mut VecIter<Token>) -> Result<node::Type, ParseError> {
    let start = tokens.current_index();
    let tok = check_none(tokens, "a type")?;
    let mut ty = if let Token::Word { value } = &tok {
        Ok(r#type(start, start, node::TypeKind::Word(value.clone())))
    } else if let Token::Op { value } = &tok {
        if value == "*" {
            Ok(r#type(start, tokens.prev_index(), node::TypeKind::Pointer(Box::new(parse_type(tokens)?))))
        } else if value == "<" {
            let ty = parse_type(tokens)?;
            let cl = check_none(tokens, "'>'")?;
            if cl != (Token::Op { value: ">".to_string() }) {
                return Err(unexp(cl, tokens.prev_index(), "'>'"));
            };
            Ok(r#type(start, tokens.prev_index(), node::TypeKind::Slice(Box::new(ty))))
        } else if value == "<<" {
            let ty = parse_type(tokens)?;
            let cl = check_none(tokens, "'>>'")?;
            if cl != (Token::Op { value: ">>".to_string() }) {
                return Err(unexp(cl, tokens.prev_index(), "'>>'"));
            };
            Ok(r#type(start, tokens.prev_index(), node::TypeKind::Slice(Box::new(r#type(start, tokens.prev_index(), node::TypeKind::Slice(Box::new(ty)))))))
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
    } else if let Token::OpenSquare = &tok {
        let ty = parse_type(tokens)?;
        let cl = check_none(tokens, "']'")?;
        let Token::CloseSquare = cl else {
            return Err(unexp(cl, tokens.prev_index(), "']'"));
        };
        Ok(r#type(start, tokens.prev_index(), node::TypeKind::List(Box::new(ty))))
    } else {
        Err(unexp(tok, tokens.prev_index(), "a type"))
    }?;
    // Post
    if let Some(Token::OpenSquare) = tokens.peek() {
        tokens.next();
        let tok = check_none(tokens, "a length")?;
        let Token::IntLit { value: len } = tok else {
            return Err(unexp(tok, tokens.prev_index(), "a length"));
        };
        let cl = check_none(tokens, "']'")?;
        let Token::CloseSquare = cl else {
            return Err(unexp(cl, tokens.prev_index(), "']'"));
        };
        ty = r#type(start, tokens.prev_index(), node::TypeKind::Array(Box::new(ty), len))
    }
    if let Some(Token::Op { value }) = tokens.peek() {
        if value == "?" {
            tokens.next();
            let err = parse_type(tokens)?;
            ty = r#type(start, tokens.prev_index(), node::TypeKind::Result(Box::new(ty), Box::new(err)));
        }
    }
    Ok(ty)
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
        "else" => 0,
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
        ".." => 2,
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
        "as" => 14, // Highest precedence (done first)
        "." => 15,
        _ => u8::MAX,
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
        ".." => 1,
        "[]" => 1,
        "," => 1,
        _ => u8::MAX,
    }
}
