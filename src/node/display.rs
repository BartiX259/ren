use std::fmt;
use crate::node; // Assuming your AST structs are in crate::node

// types::Type (resolved type) already has Display, used as crate::types::Type
// helpers::StringLit already has Display, used as crate::helpers::StringLit

// An indent string, can be configured if needed.
const INDENT: &str = "    ";

// Helper for indenting multi-line strings
fn indent_lines(text: &str, indent: &str) -> String {
    if text.is_empty() {
        return String::new();
    }
    let mut result = String::new();
    let mut first_line = true;
    for line in text.lines() {
        if !first_line {
            result.push_str("\n");
            result.push_str(indent);
        }
        result.push_str(line);
        first_line = false;
    }
    // If the original text ended with a newline, preserve it.
    // text.lines() drops trailing newline.
    if text.ends_with('\n') && !result.ends_with('\n') {
         result.push('\n');
    }
    result
}


// ============== Display for node::PosStr ==============
impl fmt::Display for node::PosStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.str)
    }
}

// ============== Display for AST node::Type and node::TypeKind ==============
impl fmt::Display for node::Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for node::TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            node::TypeKind::Word(s) => write!(f, "{}", s),
            node::TypeKind::Pointer(t) => write!(f, "*{}", t),
            node::TypeKind::Array(t, size) => write!(f, "[{}; {}]", t, size),
            node::TypeKind::Slice(t) => write!(f, "[]{}", t),
            node::TypeKind::List(t) => write!(f, "List<{}>", t),
            node::TypeKind::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", t)?;
                }
                if types.len() == 1 { write!(f, ",")?; }
                write!(f, ")")
            }
            node::TypeKind::Struct(names, types) => {
                write!(f, "struct {{ ")?;
                for (i, (name, ty)) in names.iter().zip(types.iter()).enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", name, ty)?;
                }
                write!(f, " }}")
            }
            node::TypeKind::Result(ok_t, err_t) => write!(f, "Result<{}, {}>", ok_t, err_t),
        }
    }
}

// ============== Display for Individual Expr Node Types ==============

impl fmt::Display for node::ArrLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, expr) in self.exprs.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", expr)?;
        }
        write!(f, "]")
    }
}

impl fmt::Display for node::StructLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ ")?;
        for (i, (name, expr)) in self.field_names.iter().zip(&self.field_exprs).enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", name, expr)?;
        }
        write!(f, " }}")
    }
}

impl fmt::Display for node::Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for node::BuiltIn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self.kind {
            node::BuiltInKind::Len => "len",
            node::BuiltInKind::Copy => "copy",
            node::BuiltInKind::StackPointer => "stack_pointer",
            node::BuiltInKind::Sizeof => "sizeof",
            node::BuiltInKind::Param => "param",
        };
        write!(f, "{}(", name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for node::BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.op, self.rhs)
    }
}

impl fmt::Display for node::UnExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.op, self.expr)
    }
}

impl fmt::Display for node::ElseScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.expr, self.pos_str)?;
        if let Some(capture) = &self.capture {
            write!(f, " |{}|", capture)?;
        }
        write!(f, " {{")?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::ElseExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.expr, self.pos_str, self.else_expr)
    }
}

impl fmt::Display for node::TypeCast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} as {})", self.expr, self.r#type) // self.r#type is node::Type
    }
}

// ============== Display for node::Expr (dispatcher) ==============
impl fmt::Display for node::Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            node::ExprKind::IntLit(val) => write!(f, "{}", val),
            node::ExprKind::CharLit(val) => {
                if let Some(c) = std::char::from_u32(*val) {
                    write!(f, "'{}'", c.escape_default().collect::<String>())
                } else {
                    write!(f, "'\\u{{{:x}}}'", val)
                }
            }
            node::ExprKind::BoolLit(val) => write!(f, "{}", val),
            node::ExprKind::Null => write!(f, "null"),
            node::ExprKind::ArrLit(arr_lit) => write!(f, "{}", arr_lit),
            node::ExprKind::ListLit(arr_lit, _label) => write!(f, "list{}", arr_lit), // e.g. list[...]
            node::ExprKind::StructLit(struct_lit) => write!(f, "{}", struct_lit),
            node::ExprKind::StringLit(fragments, _label) => {
                write!(f, "(")?;
                for fragment in fragments {
                    match fragment {
                        node::StringFragment::Lit(lit_val) => write!(f, "{}", lit_val)?, // crate::helpers::StringLit
                        node::StringFragment::Expr { expr, str_fn: _ } => write!(f, "{{{}}}", expr)?,
                    }
                }
                write!(f, ")")
            }
            node::ExprKind::TupleLit(exprs) => {
                write!(f, "(")?;
                for (i, expr_item) in exprs.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", expr_item)?;
                }
                if exprs.len() == 1 { write!(f, ",")?; }
                write!(f, ")")
            }
            node::ExprKind::Variable(name) => write!(f, "{}", name),
            node::ExprKind::Call(call) => write!(f, "{}", call),
            node::ExprKind::BuiltIn(builtin) => write!(f, "{}", builtin),
            node::ExprKind::BinExpr(bin_expr) => write!(f, "{}", bin_expr),
            node::ExprKind::UnExpr(un_expr) => write!(f, "{}", un_expr),
            node::ExprKind::PostUnExpr(un_expr) => write!(f, "({}{})", un_expr.expr, un_expr.op),
            node::ExprKind::ElseScope(else_scope) => write!(f, "{}", else_scope),
            node::ExprKind::ElseExpr(else_expr) => write!(f, "{}", else_expr),
            node::ExprKind::TypeCast(type_cast) => write!(f, "{}", type_cast),
        }
    }
}

// ============== Display for node::LetOrExpr (used in For loops) ==============
impl fmt::Display for node::LetOrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            node::LetOrExpr::Let(l) => write!(f, "let {} = {}", l.name, l.expr),
            node::LetOrExpr::Expr(e) => write!(f, "{}", e),
        }
    }
}

// ============== Display for Individual Stmt Node Types ==============

impl fmt::Display for node::Let {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // self.expr.ty is crate::types::Type
        write!(f, "let {}: {} = {};", self.name, self.expr.ty, self.expr)
    }
}

impl fmt::Display for node::Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // self.ty is crate::types::Type
        write!(f, "let {}: {};", self.name, self.ty)
    }
}

impl fmt::Display for node::TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {} = {};", self.name, self.r#type) // self.r#type is node::Type
    }
}

impl fmt::Display for node::Fn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, gen) in self.generics.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{}", gen)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (i, (name, ty)) in self.arg_names.iter().zip(&self.arg_types).enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", name, ty)?; // ty is node::Type
        }
        write!(f, ")")?;
        if let Some(decl_type) = &self.decl_type {
            write!(f, " -> {}", decl_type)?; // decl_type is node::Type
        }
        write!(f, " {{")?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::MainFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn main(")?;
        for (i, (name, ty)) in self.arg_names.iter().zip(&self.arg_types).enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", name, ty)?; // ty is node::Type
        }
        write!(f, ")")?;
        if let Some(decl_type) = &self.decl_type {
            write!(f, " -> {}", decl_type)?; // decl_type is node::Type
        }
        write!(f, " {{")?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::Decorator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for kind in &self.kinds {
            match kind {
                node::DecoratorKind::Pub => write!(f, "pub ")?,
            }
        }
        write!(f, "{}", self.inner)
    }
}

impl fmt::Display for node::Ret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expr) = &self.expr {
            write!(f, "return {};", expr)
        } else {
            write!(f, "return;")
        }
    }
}

impl fmt::Display for node::If {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_if_recursive(sif: &node::If, f: &mut fmt::Formatter<'_>, is_else_context: bool) -> fmt::Result {
            if is_else_context {
                write!(f, " else ")?;
            }

            if let Some(expr) = &sif.expr {
                write!(f, "if {} {{", expr)?;
            } else {
                write!(f, "{{")?; // This is an `else {}` block
            }

            if !sif.scope.is_empty() {
                for stmt_in_scope in &sif.scope {
                    let formatted_stmt = format!("{}", stmt_in_scope);
                    write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
                }
                write!(f, "\n}}")?;
            } else {
                write!(f, "}}")?;
            }

            if let Some(next_if_box) = &sif.els {
                fmt_if_recursive(next_if_box, f, true)?;
            }
            Ok(())
        }
        fmt_if_recursive(self, f, false)
    }
}

impl fmt::Display for node::Loop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "loop {{")?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::While {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while {} {{", self.expr)?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::For {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "for {}; {}; {} {{", self.init, self.cond, self.incr)?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::ForIn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "for {} in {} {{", self.capture, self.expr)?;
        if !self.scope.is_empty() {
            for stmt_in_scope in &self.scope {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::Syscall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "syscall {} {}(", self.id, self.name)?;
        for (i, ty_arg) in self.types.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", ty_arg)?; // ty_arg is node::Type
        }
        write!(f, ")")?;
        if let Some(decl_type) = &self.decl_type {
            write!(f, " -> {}", decl_type)?; // decl_type is node::Type
        }
        write!(f, ";")
    }
}

// ============== Display for node::Stmt (dispatcher) ==============
impl fmt::Display for node::Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            node::Stmt::Expr(expr_stmt) => write!(f, "{};", expr_stmt),
            node::Stmt::Let(l) => write!(f, "{}", l),
            node::Stmt::Decl(d) => write!(f, "{}", d),
            node::Stmt::Fn(sfn) => write!(f, "{}", sfn),
            node::Stmt::MainFn(mfn) => write!(f, "{}", mfn),
            node::Stmt::TypeDecl(td) => write!(f, "{}", td),
            node::Stmt::Decorator(dec) => write!(f, "{}", dec),
            node::Stmt::Ret(r) => write!(f, "{}", r),
            node::Stmt::If(sif) => write!(f, "{}", sif),
            node::Stmt::Loop(sl) => write!(f, "{}", sl),
            node::Stmt::While(sw) => write!(f, "{}", sw),
            node::Stmt::For(sfor) => write!(f, "{}", sfor),
            node::Stmt::ForIn(sforin) => write!(f, "{}", sforin),
            node::Stmt::Break(_pos_id) => write!(f, "break;"),
            node::Stmt::Continue(_pos_id) => write!(f, "continue;"),
            node::Stmt::Syscall(sc) => write!(f, "{}", sc),
        }
    }
}
