// ast.rs
// Abstract Syntax Tree (AST) definitions for the language described in the grammar.
// Contains node types, helper constructors, a simple pretty-printer, and a small Visitor trait.

use std::fmt;

use crate::parser::errors::ParseError;

// ----- Types -----
#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Float,
    Bool,
    String,          // added explicit String variant
    Custom(String),  // for user-defined or other named types
    Void,
}

impl fmt::Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeAnnotation::Int => write!(f, "int"),
            TypeAnnotation::Float => write!(f, "float"),
            TypeAnnotation::Bool => write!(f, "bool"),
            TypeAnnotation::String => write!(f, "string"), // handle new variant
            TypeAnnotation::Custom(s) => write!(f, "{}", s),
            TypeAnnotation::Void => write!(f, "void"),
        }
    }
}

// ----- Program and Declarations -----
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FunctionDecl),
    Variable(VariableDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub ret_type: TypeAnnotation,
    pub name: String,
    pub params: Vec<Parameter>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub ty: TypeAnnotation,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl {
    pub ty: TypeAnnotation,
    pub declarators: Vec<Declarator>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declarator {
    pub name: String,
    pub initializer: Option<Expression>,
}

// ----- Statements -----
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Option<Expression>),
    If {
        cond: Expression,
        then_block: Block,
        else_block: Option<Block>,
    },
    While {
        cond: Expression,
        body: Block,
    },
    For {
        init: Option<Expression>,
        cond: Option<Expression>,
        post: Option<Expression>,
        body: Block,
    },
    DoWhile {
        body: Block,
        cond: Expression,
    },
    Switch {
        discr: Expression,
        cases: Vec<CaseStmt>,
    },
    Block(Block),          // <--- NEW: allow blocks as statements
    Expr(Expression),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CaseStmt {
    Case { literal: Literal, stmts: Vec<Statement> },
    Default { stmts: Vec<Statement> },
}

// ----- Expressions -----
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Assignment {
        left: Box<Expression>,
        op: AssignmentOp,
        right: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Literal(Literal),
    Identifier(String),
    Grouping(Box<Expression>),
    Call { callee: Box<Expression>, args: Vec<Expression> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOp {
    Assign,     // =
    AddAssign,  // +=
    SubAssign,  // -=
    MulAssign,  // *=
    DivAssign,  // /=
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // relational / equality / logical ops
    Eq,   // ==
    Ne,   // !=
    Lt,   // <
    Le,   // <=
    Gt,   // >
    Ge,   // >=
    And,  // &&
    Or,   // ||
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,  // -x
    Not,  // !x
    Increment, // ++x or x++
    Decrement, // --x or x--
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

// ----- Constructors / Helpers -----
impl Program {
    pub fn new() -> Self {
        Program { decls: vec![] }
    }
}

impl Block {
    pub fn empty() -> Self {
        Block { stmts: vec![] }
    }
}

impl Declarator {
    pub fn new(name: impl Into<String>, initializer: Option<Expression>) -> Self {
        Declarator { name: name.into(), initializer }
    }
}

impl Parameter {
    pub fn new(ty: TypeAnnotation, name: impl Into<String>) -> Self {
        Parameter { ty, name: name.into() }
    }
}

// ----- Simple AST Printer -----
// Produces a nicely indented textual representation of the AST.

pub struct AstPrinter {
    out: String,
    indent: usize,
}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {
            out: String::new(),
            indent: 0,
        }
    }

    fn write_indented(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.out.push_str("  ");
        }
        self.out.push_str(s);
    }

    fn writeln_indented(&mut self, s: &str) {
        self.write_indented(s);
        self.out.push('\n');
    }

    fn inc(&mut self) {
        self.indent += 1;
    }

    fn dec(&mut self) {
        if self.indent > 0 { self.indent -= 1; }
    }

    pub fn print_program(mut self, p: &Program) -> String {
        self.writeln_indented("Program");
        self.inc();
        for decl in &p.decls {
            self.print_decl(decl);
        }
        self.dec();
        self.out
    }

    fn print_decl(&mut self, d: &Declaration) {
        match d {
            Declaration::Function(f) => self.print_function(f),
            Declaration::Variable(v) => self.print_vardecl(v),
        }
    }

    fn print_function(&mut self, f: &FunctionDecl) {
        self.writeln_indented(&format!("FunctionDecl {} -> {}", f.name, f.ret_type));
        self.inc();
        self.writeln_indented("Params:");
        self.inc();
        for p in &f.params {
            self.writeln_indented(&format!("{} : {}", p.name, p.ty));
        }
        self.dec();
        self.writeln_indented("Body:");
        self.print_block(&f.body);
        self.dec();
    }

    fn print_vardecl(&mut self, v: &VariableDecl) {
        self.writeln_indented(&format!("VariableDecl : {}", v.ty));
        self.inc();
        for d in &v.declarators {
            match &d.initializer {
                Some(init) => self.writeln_indented(&format!("{} = <initializer>", d.name)),
                None => self.writeln_indented(&format!("{}", d.name)),
            }
            if let Some(init) = &d.initializer {
                self.inc();
                self.writeln_indented("Initializer:");
                self.inc();
                self.print_expression(init);
                self.dec();
                self.dec();
            }
        }
        self.dec();
    }

    fn print_block(&mut self, b: &Block) {
        self.writeln_indented("Block");
        self.inc();
        for s in &b.stmts {
            self.print_stmt(s);
        }
        self.dec();
    }

    fn print_stmt(&mut self, s: &Statement) {
        match s {
            Statement::Return(expr) => {
                if let Some(e) = expr {
                    self.writeln_indented("Return:");
                    self.inc();
                    self.print_expression(e);
                    self.dec();
                } else {
                    self.writeln_indented("Return;");
                }
            }
            Statement::If { cond, then_block, else_block } => {
                self.writeln_indented("If");
                self.inc();
                self.writeln_indented("Cond:");
                self.inc();
                self.print_expression(cond);
                self.dec();
                self.writeln_indented("Then:");
                self.print_block(then_block);
                if let Some(eb) = else_block {
                    self.writeln_indented("Else:");
                    self.print_block(eb);
                }
                self.dec();
            }
            Statement::While { cond, body } => {
                self.writeln_indented("While");
                self.inc();
                self.writeln_indented("Cond:");
                self.inc(); self.print_expression(cond); self.dec();
                self.writeln_indented("Body:");
                self.print_block(body);
                self.dec();
            }
            Statement::For { init, cond, post, body } => {
                self.writeln_indented("For");
                self.inc();
                if let Some(i) = init { self.writeln_indented("Init:"); self.inc(); self.print_expression(i); self.dec(); }
                if let Some(c) = cond { self.writeln_indented("Cond:"); self.inc(); self.print_expression(c); self.dec(); }
                if let Some(p) = post { self.writeln_indented("Post:"); self.inc(); self.print_expression(p); self.dec(); }
                self.writeln_indented("Body:");
                self.print_block(body);
                self.dec();
            }
            Statement::DoWhile { body, cond } => {
                self.writeln_indented("DoWhile");
                self.inc();
                self.writeln_indented("Body:"); self.print_block(body);
                self.writeln_indented("Cond:"); self.inc(); self.print_expression(cond); self.dec();
                self.dec();
            }
            Statement::Switch { discr, cases } => {
                self.writeln_indented("Switch");
                self.inc();
                self.writeln_indented("Discriminant:"); self.inc(); self.print_expression(discr); self.dec();
                for c in cases { self.print_case(c); }
                self.dec();
            }
            Statement::Block(b) => {
                self.writeln_indented("Block (statement):");
                self.print_block(b);
            }
            Statement::Expr(e) => { self.writeln_indented("ExprStmt:"); self.inc(); self.print_expression(e); self.dec(); }
            Statement::Break => self.writeln_indented("Break;"),
            Statement::Continue => self.writeln_indented("Continue;"),
        }
    }

    fn print_case(&mut self, c: &CaseStmt) {
        match c {
            CaseStmt::Case { literal, stmts } => {
                self.writeln_indented(&format!("Case: {:?}", literal));
                self.inc();
                for s in stmts { self.print_stmt(s); }
                self.dec();
            }
            CaseStmt::Default { stmts } => {
                self.writeln_indented("Default:");
                self.inc();
                for s in stmts { self.print_stmt(s); }
                self.dec();
            }
        }
    }

    fn print_expression(&mut self, e: &Expression) {
        match e {
            Expression::Assignment { left, op, right } => {
                self.writeln_indented(&format!("Assignment ({:?})", op));
                self.inc();
                self.writeln_indented("Left:"); self.inc(); self.print_expression(left); self.dec();
                self.writeln_indented("Right:"); self.inc(); self.print_expression(right); self.dec();
                self.dec();
            }
            Expression::Binary { left, op, right } => {
                self.writeln_indented(&format!("Binary ({:?})", op));
                self.inc();
                self.writeln_indented("Left:"); self.inc(); self.print_expression(left); self.dec();
                self.writeln_indented("Right:"); self.inc(); self.print_expression(right); self.dec();
                self.dec();
            }
            Expression::Unary { op, expr } => {
                self.writeln_indented(&format!("Unary ({:?})", op));
                self.inc();
                self.print_expression(expr);
                self.dec();
            }
            Expression::Literal(l) => { self.writeln_indented(&format!("Literal: {:?}", l)); }
            Expression::Identifier(name) => { self.writeln_indented(&format!("Identifier: {}", name)); }
            Expression::Grouping(inner) => { self.writeln_indented("Grouping:"); self.inc(); self.print_expression(inner); self.dec(); }
            Expression::Call { callee, args } => {
                self.writeln_indented("Call"); self.inc();
                self.writeln_indented("Callee:"); self.inc(); self.print_expression(callee); self.dec();
                if !args.is_empty() { self.writeln_indented("Args:"); self.inc(); for a in args { self.print_expression(a); } self.dec(); }
                self.dec();
            }
        }
    }
}

// Convenience: top-level function to get printed AST string
pub fn format_program(p: &Program) -> String {
    AstPrinter::new().print_program(p)
}

// ----- Visitor trait (simple) -----
// A small visitor trait that can be used by later passes (type-check, codegen, etc.).
pub trait Visitor {
    fn visit_program(&mut self, _p: &Program) {}
    fn visit_decl(&mut self, _d: &Declaration) {}
    fn visit_function(&mut self, _f: &FunctionDecl) {}
    fn visit_vardecl(&mut self, _v: &VariableDecl) {}
    fn visit_stmt(&mut self, _s: &Statement) {}
    fn visit_expr(&mut self, _e: &Expression) {}
}

// Example traversal that calls visitor hooks. This is intentionally shallow —
// more fine-grained traversal can be implemented when needed.
impl Program {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        v.visit_program(self);
        for d in &self.decls { v.visit_decl(d); match d { Declaration::Function(f) => f.walk(v), Declaration::Variable(var) => { v.visit_vardecl(var); } } }
    }
}

impl FunctionDecl {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        v.visit_function(self);
        for p in &self.params { /* parameters could be visited */ let _ = p; }
        self.body.walk(v);
    }
}

impl Block {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        for s in &self.stmts { v.visit_stmt(s); s.walk(v); }
    }
}

impl Statement {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        match self {
            Statement::Return(Some(e)) => { v.visit_expr(e); e.walk(v); }
            Statement::Return(None) => {}
            Statement::If { cond, then_block, else_block } => { v.visit_expr(cond); cond.walk(v); then_block.walk(v); if let Some(eb) = else_block { eb.walk(v); } }
            Statement::While { cond, body } => { v.visit_expr(cond); cond.walk(v); body.walk(v); }
            Statement::For { init, cond, post, body } => { if let Some(i) = init { v.visit_expr(i); i.walk(v); } if let Some(c) = cond { v.visit_expr(c); c.walk(v); } if let Some(p) = post { v.visit_expr(p); p.walk(v); } body.walk(v); }
            Statement::DoWhile { body, cond } => { body.walk(v); v.visit_expr(cond); cond.walk(v); }
            Statement::Switch { discr, cases } => { v.visit_expr(discr); discr.walk(v); for c in cases { match c { CaseStmt::Case { literal: _, stmts } => for s in stmts { s.walk(v); }, CaseStmt::Default { stmts } => for s in stmts { s.walk(v); }, } } }
            Statement::Block(b) => { b.walk(v); }
            Statement::Expr(e) => { v.visit_expr(e); e.walk(v); }
            Statement::Break | Statement::Continue => {}
        }
    }
}

impl Expression {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        v.visit_expr(self);
        match self {
            Expression::Assignment { left, right, .. } => { left.walk(v); right.walk(v); }
            Expression::Binary { left, right, .. } => { left.walk(v); right.walk(v); }
            Expression::Unary { expr, .. } => { expr.walk(v); }
            Expression::Literal(_) => {}
            Expression::Identifier(_) => {}
            Expression::Grouping(inner) => inner.walk(v),
            Expression::Call { callee, args } => { callee.walk(v); for a in args { a.walk(v); } }
        }
    }
}

impl Literal {
    pub fn as_string(&self) -> String {
        match self {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::String(s) => s.clone(),

        }
    }
}
