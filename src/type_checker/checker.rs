use crate::parser::ast::{
    Program, Declaration, FunctionDecl, Statement, Expression, BinaryOp, UnaryOp
};
use crate::scope::symbol_table::{Scope, SymbolKind, Type as SymbolType};
use super::errors::TypeChkError;

pub type Type = SymbolType;

pub struct TypeChecker<'a> {
    global_scope: &'a Scope,
    errors: Vec<TypeChkError>,
    current_scope: &'a Scope,
    current_fn_return_type: Type,
    loop_depth: usize,
}

impl<'a> TypeChecker<'a> {
    pub fn new(global_scope: &'a Scope) -> Self {
        TypeChecker {
            global_scope,
            errors: Vec::new(),
            current_scope: global_scope,
            current_fn_return_type: Type::Void,
            loop_depth: 0,
        }
    }

    fn add_error(&mut self, error: TypeChkError) {
        eprintln!("Type Check Error: {}", error);
        self.errors.push(error);
    }

    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(ty, Type::Int | Type::Float)
    }

    fn is_integer(&self, ty: &Type) -> bool {
        matches!(ty, Type::Int)
    }

    fn is_boolean(&self, ty: &Type) -> bool {
        matches!(ty, Type::Bool)
    }

    fn types_match(&self, ty1: &Type, ty2: &Type) -> bool {
        ty1 == ty2
    }

    fn check_for_return_stmt(&self, block: &'a crate::parser::ast::Block) -> bool {
        for stmt in &block.stmts {
            match stmt {
                Statement::Return(_) => return true,
                Statement::Block(b) => {
                    if self.check_for_return_stmt(b) { return true; }
                }
                Statement::If { then_block, else_block, .. } => {
                    if self.check_for_return_stmt(then_block) { return true; }
                    if let Some(else_b) = else_block {
                        if self.check_for_return_stmt(else_b) { return true; }
                    }
                }
                Statement::For { body, .. } |
                Statement::While { body, .. } |
                Statement::DoWhile { body, .. } => {
                    if self.check_for_return_stmt(body) { return true; }
                }
                Statement::Switch { cases, .. } => {
                    for case in cases {
                        let stmts = match case {
                            crate::parser::ast::CaseStmt::Case { stmts, .. } => stmts,
                            crate::parser::ast::CaseStmt::Default { stmts } => stmts,
                        };
                        for s in stmts {
                            if matches!(s, Statement::Return(_)) { return true; }
                            if let Statement::Block(b) = s {
                                if self.check_for_return_stmt(b) { return true; }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn check_conditional_expression(&mut self, cond: Option<&'a Expression>) {
        if let Some(c) = cond {
            match self.check_expression(c) {
                Ok(ty) => {
                    if !self.is_boolean(&ty) {
                        self.add_error(TypeChkError::NonBooleanCondStmt);
                    }
                }
                Err(e) => self.add_error(e),
            }
        }
    }

    pub fn check_program(&mut self, program: &'a Program) -> Result<(), Vec<TypeChkError>> {
        for decl in &program.decls {
            self.check_declaration(decl);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_declaration(&mut self, decl: &'a Declaration) {
        match decl {
            Declaration::Function(f) => self.check_function_declaration(f),
            Declaration::Variable(v) => {
                for declarator in &v.declarators {
                    if let Some(init_expr) = &declarator.initializer {
                        match self.check_expression(init_expr) {
                            Ok(init_type) => {
                                let decl_type = SymbolType::from_ast_type(&v.ty);
                                if !self.types_match(&init_type, &decl_type) {
                                    self.add_error(TypeChkError::ErroneousVarDecl(
                                        format!("Initializer type '{}' does not match variable type '{}'",
                                            init_type.to_string(), decl_type.to_string())
                                    ));
                                }
                            }
                            Err(e) => self.add_error(e),
                        }
                    }
                }
            }
        }
    }

    fn check_function_declaration(&mut self, func: &'a FunctionDecl) {
        self.current_fn_return_type = SymbolType::from_ast_type(&func.ret_type);
        let fn_return_type = self.current_fn_return_type.clone();

        let child_scope = match self.current_scope.child_scopes.get(&func.name) {
            Some(scope) => scope,
            None => {
                self.add_error(TypeChkError::GeneralTypeCheckError(
                    format!("Internal Error: Function scope missing for {}", func.name)
                ));
                return;
            }
        };
        let saved_scope = self.current_scope;
        self.current_scope = child_scope;

        self.check_block(&func.body);

        let has_return = self.check_for_return_stmt(&func.body);
        if fn_return_type != Type::Void && !has_return {
            self.add_error(TypeChkError::ReturnStmtNotFound);
        }

        self.current_scope = saved_scope;
        self.current_fn_return_type = Type::Void;
    }

    fn with_block_scope<F>(&mut self, f: F)
where
    F: FnOnce(&mut Self),
{
    let saved_scope = self.current_scope;
    f(self);
    self.current_scope = saved_scope;
}

    fn check_block(&mut self, block: &'a crate::parser::ast::Block) {
        self.with_block_scope(|checker| {
            for stmt in &block.stmts {
                checker.check_statement(stmt);
            }
        });
    }

    fn check_statement(&mut self, stmt: &'a Statement) {
        match stmt {
            Statement::Block(b) => self.check_block(b),

            Statement::For { init, cond, post, body } => {
                self.with_block_scope(|checker| {
                    if let Some(i) = init { checker.check_expression(i); }
                    checker.check_conditional_expression(cond.as_ref());
                    if let Some(p) = post { checker.check_expression(p); }

                    checker.loop_depth += 1;
                    checker.check_block(body);
                    checker.loop_depth -= 1;
                });
            }

            Statement::While { cond, body } |
            Statement::DoWhile { body, cond } => {
                self.with_block_scope(|checker| {
                    checker.check_conditional_expression(Some(cond));
                    checker.loop_depth += 1;
                    checker.check_block(body);
                    checker.loop_depth -= 1;
                });
            }

            Statement::If { cond, then_block, else_block } => {
                self.check_conditional_expression(Some(cond));
                self.with_block_scope(|checker| checker.check_block(then_block));
                if let Some(eb) = else_block {
                    self.with_block_scope(|checker| checker.check_block(eb));
                }
            }

            Statement::Return(expr_opt) => {
                let expected = self.current_fn_return_type.clone();
                match expr_opt {
                    Some(expr) => {
                        match self.check_expression(expr) {
                            Ok(found) => {
                                if expected == Type::Void || !self.types_match(&expected, &found) {
                                    self.add_error(TypeChkError::ErroneousReturnType {
                                        expected: expected.to_string(),
                                        found: found.to_string()
                                    });
                                }
                            }
                            Err(e) => self.add_error(e),
                        }
                    }
                    None => {
                        if expected != Type::Void {
                            self.add_error(TypeChkError::EmptyExpression);
                        }
                    }
                }
            }

            Statement::Break | Statement::Continue => {
                if self.loop_depth == 0 {
                    self.add_error(TypeChkError::ErroneousBreak);
                }
            }

            Statement::Switch { discr, cases } => {
                self.check_expression(discr); 
                for case in cases {
                    match case {
                        crate::parser::ast::CaseStmt::Case { stmts, .. } |
                        crate::parser::ast::CaseStmt::Default { stmts } => {
                            for s in stmts { self.check_statement(s); }
                        }
                    }
                }
            }

            Statement::Expr(e) => { self.check_expression(e); }
            _ => {}
        }
    }

    fn check_expression(&mut self, expr: &'a Expression) -> Result<Type, TypeChkError> {
        match expr {
            Expression::Literal(lit) => Ok(match lit {
                crate::parser::ast::Literal::Int(_) => Type::Int,
                crate::parser::ast::Literal::Float(_) => Type::Float,
                crate::parser::ast::Literal::String(_) => Type::String,
                crate::parser::ast::Literal::Bool(_) => Type::Bool,
            }),

            Expression::Identifier(name) => {
                match self.current_scope.lookup(name) {
                    Some(symbol) => match &symbol.kind {
                        SymbolKind::Variable { type_info } => Ok(type_info.clone()),
                        SymbolKind::Function { .. } => Err(TypeChkError::ExpressionTypeMismatch {
                            expected: "Variable".to_string(),
                            found: "Function".to_string(),
                        }),
                    },
                    None => Err(TypeChkError::GeneralTypeCheckError(
                        format!("Internal Error: Undeclared identifier '{}'", name)
                    )),
                }
            }

            Expression::Grouping(inner) => self.check_expression(inner),

            Expression::Unary { op, expr } => {
                let expr_type = self.check_expression(expr)?;
                match op {
                    UnaryOp::Neg => {
                        if !self.is_numeric(&expr_type) {
                            return Err(TypeChkError::AttemptedBitOpOnNonNumeric);
                        }
                        Ok(expr_type)
                    }
                    UnaryOp::Not => {
                        if !self.is_boolean(&expr_type) {
                            return Err(TypeChkError::ExpectedBooleanExpression);
                        }
                        Ok(Type::Bool)
                    }
                    _ => Ok(expr_type),
                }
            }

            Expression::Assignment { left, op: _, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                if !self.types_match(&left_type, &right_type) {
                    return Err(TypeChkError::ExpressionTypeMismatch {
                        expected: left_type.to_string(),
                        found: right_type.to_string(),
                    });
                }
                Ok(left_type)
            }

            Expression::Binary { left, op, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                if !self.types_match(&left_type, &right_type) {
                    return Err(TypeChkError::ExpressionTypeMismatch {
                        expected: left_type.to_string(),
                        found: right_type.to_string(),
                    });
                }

                match op {
                    BinaryOp::And | BinaryOp::Or => {
                        if !self.is_boolean(&left_type) {
                            return Err(TypeChkError::AttemptedBoolOpOnNonBools);
                        }
                        Ok(Type::Bool)
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Gt | BinaryOp::Lt | BinaryOp::Ge | BinaryOp::Le => Ok(Type::Bool),
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => Ok(left_type),
                    BinaryOp::Mod => Ok(Type::Int),
                }
            }

            Expression::Call { callee, args } => {
                let func_name = match callee.as_ref() {
                    Expression::Identifier(name) => name,
                    _ => return Err(TypeChkError::GeneralTypeCheckError(
                        "Attempted to call a non-identifier expression".to_string()
                    )),
                };

                let func_symbol = self.current_scope.lookup(func_name);
                let (ret_type, expected_params) = match func_symbol {
                    Some(sym) => match &sym.kind {
                        SymbolKind::Function { return_type, param_types } => (return_type.clone(), param_types),
                        _ => return Err(TypeChkError::ExpressionTypeMismatch {
                            expected: "Function".to_string(),
                            found: "Variable".to_string(),
                        }),
                    },
                    None => return Err(TypeChkError::GeneralTypeCheckError(
                        format!("Internal Error: Undefined function called: {}", func_name)
                    )),
                };

                if args.len() != expected_params.len() {
                    self.add_error(TypeChkError::FnCallParamCount { expected: expected_params.len(), found: args.len() });
                }

                for (i, arg) in args.iter().enumerate() {
                    let arg_type = match self.check_expression(arg) {
                        Ok(ty) => ty,
                        Err(e) => { self.add_error(e); Type::Void },
                    };

                    if let Some(expected_type) = expected_params.get(i) {
                        if !self.types_match(expected_type, &arg_type) {
                            self.add_error(TypeChkError::FnCallParamType {
                                expected: expected_type.to_string(),
                                found: arg_type.to_string(),
                            });
                        }
                    }
                }

                Ok(ret_type)
            }

            _ => Ok(Type::Void),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
        }
    }
}
