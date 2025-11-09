use super::symbol_table::{Scope, Symbol, SymbolKind, Type as SymbolType};
use super::errors::{ScopeError, Location}; 

use crate::parser::ast::{
    Program, Declaration, FunctionDecl, Parameter, VariableDecl, Declarator,
    Block, Statement, Expression, TypeAnnotation, Literal
};


impl SymbolType {
    pub fn from_ast_type(ast_type: &TypeAnnotation) -> Self {
        match ast_type {
            TypeAnnotation::Int => SymbolType::Int,
            TypeAnnotation::Float => SymbolType::Float,
            TypeAnnotation::Bool => SymbolType::Bool,
            TypeAnnotation::String => SymbolType::String,
            TypeAnnotation::Custom(_) | TypeAnnotation::Void => SymbolType::Void,
        }
    }
}


pub struct ScopeAnalyzer {
    current_scope: Box<Scope>,
    errors: Vec<ScopeError>,
    default_loc: Location, 
}

impl ScopeAnalyzer {
    pub fn new() -> Self {
        ScopeAnalyzer {
            current_scope: Box::new(Scope::new(None)),
            errors: Vec::new(),
            // Re-initialize the fallback location
            default_loc: Location { line: 0, column: 0 }, 
        }
    }

   
    fn get_func_location(&self, _f: &FunctionDecl) -> Location {
        self.default_loc.clone() 
    }

    fn get_declarator_location(&self, _d: &Declarator) -> Location {
        self.default_loc.clone()
    }
    
    fn get_identifier_location(&self, _e: &Expression) -> Location {
        self.default_loc.clone() 
    }
    
   
    fn push_scope(&mut self) {
        let parent = std::mem::replace(&mut self.current_scope, Box::new(Scope::new(None)));
        self.current_scope = Box::new(Scope::new(Some(parent)));
    }

    fn pop_scope(&mut self) {
        if let Some(parent) = self.current_scope.take_parent() {
            self.current_scope = parent;
        } else {
            panic!("Attempted to pop the global scope. Scope management error.");
        }
    }

    

    pub fn analyze_program(&mut self, program: &Program) -> Result<(), Vec<ScopeError>> {
        for decl in &program.decls {
            self.analyze_declaration(decl);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
    
    fn analyze_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Function(f) => self.analyze_function_declaration(f),
            Declaration::Variable(v) => self.analyze_global_variable_declaration(v),
        }
    }
    
    fn analyze_global_variable_declaration(&mut self, var_decl: &VariableDecl) {
        self.analyze_declarators(&var_decl.declarators, &var_decl.ty, true);
    }

    fn analyze_function_declaration(&mut self, func: &FunctionDecl) {
        let name = &func.name;
        let loc = self.get_func_location(func); 

        
        if let Some(original_symbol) = self.current_scope.lookup_current(name) {
            self.errors.push(ScopeError::FunctionPrototypeRedefinition {
                name: name.clone(),
                current_location: loc.clone(),
                original_location: original_symbol.declared_at.clone(),
            });
        } else {
            let new_symbol = Symbol {
                name: name.clone(),
                kind: SymbolKind::Function { 
                    return_type: SymbolType::from_ast_type(&func.ret_type),
                    param_types: func.params.iter().map(|p| SymbolType::from_ast_type(&p.ty)).collect(),
                },
                declared_at: loc,
            };
            let _ = self.current_scope.insert(new_symbol);
        }
        
     
        self.analyze_function_body(func);
    }
    
    fn analyze_function_body(&mut self, func: &FunctionDecl) {
        self.push_scope(); 
        

        for param in &func.params {
            // Fallback to default_loc since Parameter lacks a `loc` field
            let loc = self.default_loc.clone(); 
            let new_symbol = Symbol {
                name: param.name.clone(),
                kind: SymbolKind::Variable { 
                    type_info: SymbolType::from_ast_type(&param.ty) 
                },
                declared_at: loc.clone(),
            };
            
            if let Err(original_symbol) = self.current_scope.insert(new_symbol) {
                self.errors.push(ScopeError::VariableRedefinition {
                    name: param.name.clone(),
                    current_location: loc.clone(),
                    original_location: original_symbol.declared_at.clone(),
                });
            }
        }

        self.analyze_block(&func.body);

        self.pop_scope(); 
    }
    
    fn analyze_declarators(&mut self, declarators: &[Declarator], ty: &TypeAnnotation, is_global: bool) {
        for declarator in declarators {
            let loc = self.get_declarator_location(declarator); 

            if let Some(expr) = &declarator.initializer {
                self.analyze_expression(expr);
            }
            
            let new_symbol = Symbol {
                name: declarator.name.clone(),
                kind: SymbolKind::Variable { 
                    type_info: SymbolType::from_ast_type(ty) 
                },
                declared_at: loc.clone(),
            };

            if let Err(original_symbol) = self.current_scope.insert(new_symbol) {
                if is_global {
                    self.errors.push(ScopeError::FunctionPrototypeRedefinition {
                        name: declarator.name.clone(),
                        current_location: loc.clone(),
                        original_location: original_symbol.declared_at.clone(),
                    });
                } else {
                    self.errors.push(ScopeError::VariableRedefinition {
                        name: declarator.name.clone(),
                        current_location: loc.clone(),
                        original_location: original_symbol.declared_at.clone(),
                    });
                }
            }
        }
    }
    
    fn analyze_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.analyze_statement(stmt);
        }
    }

    fn analyze_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Block(b) => {
                self.push_scope();
                self.analyze_block(b);
                self.pop_scope();
            }
            Statement::For { init, cond, post, body } => {
                self.push_scope(); 
                
                if let Some(i) = init { self.analyze_expression(i); } 
                if let Some(c) = cond { self.analyze_expression(c); } 
                
                self.analyze_block(body); 
                
                if let Some(p) = post { self.analyze_expression(p); } 
                
                self.pop_scope(); 
            }
            
            // --- Control Flow & Expression Statements ---
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt { self.analyze_expression(expr); }
            }
            Statement::If { cond, then_block, else_block } => {
                self.analyze_expression(cond);
                self.analyze_block(then_block);
                if let Some(eb) = else_block { self.analyze_block(eb); }
            }
            Statement::While { cond, body } => {
                self.analyze_expression(cond);
                self.analyze_block(body);
            }
            Statement::DoWhile { body, cond } => {
                self.analyze_block(body);
                self.analyze_expression(cond);
            }
            Statement::Switch { discr, cases } => {
                self.analyze_expression(discr);
                for case in cases {
                    match case {
            
                        crate::parser::ast::CaseStmt::Case { stmts, .. } |
                        crate::parser::ast::CaseStmt::Default { stmts } => {
                            for s in stmts { self.analyze_statement(s); }
                        }
                    }
                }
            }
            Statement::Expr(e) => {
                self.analyze_expression(e);
            }
            Statement::Break | Statement::Continue => {  }
        }
    }

    fn analyze_expression(&mut self, expr: &Expression) {
        match expr {
            
            Expression::Assignment { left, op: _, right } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
            }
            Expression::Binary { left, op: _, right } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
            }
            Expression::Unary { op: _, expr } => { 
                self.analyze_expression(expr);
            }
            Expression::Literal(_) => { }
            Expression::Identifier(name) => {
                let loc = self.get_identifier_location(expr);
                
                if self.current_scope.lookup(name).is_none() {
                    self.errors.push(ScopeError::UndeclaredVariableAccessed { 
                        name: name.clone(),
                        location: loc,
                    });
                }
            }
            Expression::Grouping(inner) => {
                self.analyze_expression(inner);
            }
            
            Expression::Call { callee, args } => { 
                let loc = self.get_identifier_location(callee); 
                
                if let Expression::Identifier(name) = callee.as_ref() {
                    if self.current_scope.lookup(name).is_none() {
                        self.errors.push(ScopeError::UndefinedFunctionCalled {
                            name: name.clone(),
                            location: loc,
                        });
                    }
                } else {
                    self.analyze_expression(callee);
                }
                
                for arg in args {
                    self.analyze_expression(arg);
                }
            }
        }
    }
}