use crate::node; // Assuming your AST structs are in crate::node
use std::fmt;

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
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                if types.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            node::TypeKind::Struct(names, types) => {
                write!(f, "struct {{ ")?;
                for (i, (name, ty)) in names.iter().zip(types.iter()).enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty)?;
                }
                write!(f, " }}")
            }
            node::TypeKind::Result(ok_t, err_t) => write!(f, "Result<{}, {}>", ok_t, err_t),
            node::TypeKind::Option(t) => write!(f, "Option<{}>", t),
            node::TypeKind::Map(k, v) => write!(f, "Map<{}, {}>", k, v),
            node::TypeKind::Fn(args, ret) => {
                write!(f, "fn(")?;
                for (i, t) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")?;
                if let Some(ret_type) = ret {
                    write!(f, " -> {}", ret_type)?;
                }
                Ok(())
            }
        }
    }
}

// ============== Display for Helper Node Types ==============

impl fmt::Display for node::Capture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            node::Capture::Single(s) => write!(f, "{}", s),
            node::Capture::Multiple(names, has_brackets) => {
                if *has_brackets {
                    write!(f, "[")?;
                } else {
                    write!(f, "(")?;
                }
                for (i, name) in names.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", name)?;
                }
                if *has_brackets {
                    write!(f, "]")
                } else {
                    write!(f, ")")
                }
            }
        }
    }
}

impl fmt::Display for node::Unpack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(brackets) = &self.brackets {
            write!(f, "{}", brackets)?;
        }
        write!(f, "{}", self.lhs)?;
        if let Some(rhs) = &self.rhs {
            write!(f, ", {}", rhs)?;
        }
        write!(f, " = {}", self.expr)
    }
}

impl fmt::Display for node::IfKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            node::IfKind::Expr(e) => write!(f, "{}", e),
            node::IfKind::Unpack(u) => write!(f, "let {}", u),
            node::IfKind::None => Ok(()),
        }
    }
}

impl fmt::Display for node::FnSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        for (i, ty_arg) in self.arg_types.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty_arg)?;
        }
        write!(f, ")")?;
        if let Some(decl_type) = &self.decl_type {
            write!(f, " -> {}", decl_type)?;
        }
        Ok(())
    }
}

// ============== Display for Individual Expr Node Types ==============

impl fmt::Display for node::ArrLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, expr) in self.exprs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", expr)?;
        }
        write!(f, "]")
    }
}

impl fmt::Display for node::MapLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.keys.iter().zip(&self.values).enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for node::StructLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ ")?;
        for (i, (name, expr)) in self.field_names.iter().zip(&self.field_exprs).enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", name, expr)?;
        }
        write!(f, " }}")
    }
}

impl fmt::Display for node::Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
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
            node::BuiltInKind::IsType => "is_type",
        };
        write!(f, "{}(", name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
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
                write!(
                    f,
                    "'{}'",
                    (*val as char).escape_default().collect::<String>()
                )
            }
            node::ExprKind::BoolLit(val) => write!(f, "{}", val),
            node::ExprKind::Null => write!(f, "null"),
            node::ExprKind::None => write!(f, "none"),
            node::ExprKind::ArrLit(arr_lit) => write!(f, "{}", arr_lit),
            node::ExprKind::ListLit(arr_lit, _label) => write!(f, "list{}", arr_lit), // e.g. list[...]
            node::ExprKind::MapLit(map_lit) => write!(f, "{}", map_lit),
            node::ExprKind::StructLit(struct_lit) => write!(f, "{}", struct_lit),
            node::ExprKind::StringLit(fragments, _label) => {
                write!(f, "(")?;
                for fragment in fragments {
                    match fragment {
                        node::StringFragment::Lit(lit_val) => write!(f, "{}", lit_val)?, // crate::helpers::StringLit
                        node::StringFragment::Expr {
                            expr,
                            str_fn: _,
                        } => write!(f, "{{{}}}", expr)?,
                    }
                }
                write!(f, ")")
            }
            node::ExprKind::TupleLit(exprs) => {
                write!(f, "(")?;
                for (i, expr_item) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr_item)?;
                }
                if exprs.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            node::ExprKind::Variable(name) => write!(f, "{}", name),
            node::ExprKind::Call(call) => write!(f, "{}", call),
            node::ExprKind::BuiltIn(builtin) => write!(f, "{}", builtin),
            node::ExprKind::BinExpr(bin_expr) => write!(f, "{}", bin_expr),
            node::ExprKind::UnExpr(un_expr) => write!(f, "{}", un_expr),
            node::ExprKind::PostUnExpr(un_expr) => write!(f, "({}{})", un_expr.expr, un_expr.op),
            node::ExprKind::TypeCast(type_cast) => write!(f, "{}", type_cast),
        }
    }
}

// ============== Display for Individual Stmt Node Types ==============

impl fmt::Display for node::Let {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "let {}: {} = {};",
            self.capture, self.expr.ty, self.expr
        )
    }
}

impl fmt::Display for node::ElseExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} else {}", self.unpack, self.else_expr)
    }
}

impl fmt::Display for node::ElseScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} else {{", self.unpack)?;
        if !self.scope.stmts.is_empty() {
            for stmt_in_scope in &self.scope.stmts {
                let formatted_stmt = format!("{}", stmt_in_scope);
                write!(f, "\n{}", indent_lines(&formatted_stmt, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {}: {};", self.name, self.r#type)
    }
}

impl fmt::Display for node::TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {} = {};", self.name, self.r#type) // self.r#type is node::Type
    }
}

impl fmt::Display for node::Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum {} {{", self.name)?;
        for (name, ty) in &self.variants {
            write!(f, "{}", INDENT)?;
            if let Some(t) = ty {
                writeln!(f, "{}({}),", name, t)?;
            } else {
                writeln!(f, "{},", name)?;
            }
        }
        write!(f, "}}")
    }
}

impl fmt::Display for node::Extern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extern {};", self.sig)
    }
}

impl fmt::Display for node::Fn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, gen) in self.generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", gen)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (i, (name, ty)) in self.arg_names.iter().zip(&self.arg_types).enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", name, ty)?; // ty is node::Type
        }
        write!(f, ")")?;
        if let Some(decl_type) = &self.decl_type {
            write!(f, " -> {}", decl_type)?; // decl_type is node::Type
        }
        write!(f, " {{")?;
        if !self.scope.stmts.is_empty() {
            for stmt_in_scope in &self.scope.stmts {
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
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", name, ty)?; // ty is node::Type
        }
        write!(f, ")")?;
        if let Some(decl_type) = &self.decl_type {
            write!(f, " -> {}", decl_type)?; // decl_type is node::Type
        }
        write!(f, " {{")?;
        if !self.scope.stmts.is_empty() {
            for stmt_in_scope in &self.scope.stmts {
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
        fn fmt_if_recursive(
            sif: &node::If,
            f: &mut fmt::Formatter<'_>,
            is_else_context: bool,
        ) -> fmt::Result {
            if is_else_context {
                write!(f, " else ")?;
            }

            match &sif.cond {
                node::IfKind::None => {
                    write!(f, "{{")?;
                }
                _ => {
                    write!(f, "if {} {{", sif.cond)?;
                }
            }

            if !sif.scope.stmts.is_empty() {
                for stmt_in_scope in &sif.scope.stmts {
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

impl fmt::Display for node::Match {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match {} {{", self.match_expr)?;
        if !self.branches.is_empty() {
            for (expr, scope) in &self.branches {
                let scope_str = if scope.stmts.is_empty() {
                    String::from(" {}")
                } else {
                    let mut s = String::from(" {\n");
                    for stmt in &scope.stmts {
                        s.push_str(&indent_lines(&format!("{}", stmt), INDENT));
                        s.push('\n');
                    }
                    s.push('}');
                    s
                };
                let branch_str = format!("{} =>{}", expr, scope_str);
                write!(f, "\n{}", indent_lines(&branch_str, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::MatchType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match type {} {{", self.match_type)?;
        if !self.branches.is_empty() {
            for (ty, scope, _is_default) in &self.branches {
                let scope_str = if scope.stmts.is_empty() {
                    String::from(" {}")
                } else {
                    let mut s = String::from(" {\n");
                    for stmt in &scope.stmts {
                        s.push_str(&indent_lines(&format!("{}", stmt), INDENT));
                        s.push('\n');
                    }
                    s.push('}');
                    s
                };
                let branch_str = format!("{} =>{}", ty, scope_str);
                write!(f, "\n{}", indent_lines(&branch_str, INDENT))?;
            }
            write!(f, "\n}}")
        } else {
            write!(f, "}}")
        }
    }
}

impl fmt::Display for node::Loop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "loop {{")?;
        if !self.scope.stmts.is_empty() {
            for stmt_in_scope in &self.scope.stmts {
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
        if !self.scope.stmts.is_empty() {
            for stmt_in_scope in &self.scope.stmts {
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
        if !self.scope.stmts.is_empty() {
            for stmt_in_scope in &self.scope.stmts {
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
        write!(f, "syscall {} {};", self.id, self.sig)
    }
}

// ============== Display for node::Stmt (dispatcher) ==============
impl fmt::Display for node::Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            node::Stmt::Expr(expr_stmt) => write!(f, "{};", expr_stmt),
            node::Stmt::Let(l) => write!(f, "{}", l),
            node::Stmt::LetElseExpr(lee) => write!(f, "{};", lee),
            node::Stmt::LetElseScope(les) => write!(f, "{}", les),
            node::Stmt::Decl(d) => write!(f, "{}", d),
            node::Stmt::Fn(sfn) => write!(f, "{}", sfn),
            node::Stmt::MainFn(mfn) => write!(f, "{}", mfn),
            node::Stmt::TypeDecl(td) => write!(f, "{}", td),
            node::Stmt::Enum(e) => write!(f, "{}", e),
            node::Stmt::Extern(e) => write!(f, "{}", e),
            node::Stmt::Decorator(dec) => write!(f, "{}", dec),
            node::Stmt::Ret(r) => write!(f, "{}", r),
            node::Stmt::If(sif) => write!(f, "{}", sif),
            node::Stmt::Match(m) => write!(f, "{}", m),
            node::Stmt::MatchType(mt) => write!(f, "{}", mt),
            node::Stmt::Loop(sl) => write!(f, "{}", sl),
            node::Stmt::While(sw) => write!(f, "{}", sw),
            node::Stmt::ForIn(sforin) => write!(f, "{}", sforin),
            node::Stmt::Break(_pos_id) => write!(f, "break;"),
            node::Stmt::Continue(_pos_id) => write!(f, "continue;"),
            node::Stmt::Syscall(sc) => write!(f, "{}", sc),
            node::Stmt::Marker => write!(f, "<marker>;"),
        }
    }
}
