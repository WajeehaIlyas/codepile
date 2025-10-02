use crate::lexer::token::Token;
use crate::parser::ast;
use crate::parser::errors::ParseError;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    // Entry point
    // Program -> { Declaration }*
    pub fn parse_program(&mut self) -> Result<ast::Program, ParseError> {
        let mut decls = Vec::new();
        while !matches!(self.peek(), Token::EOF) {
            decls.push(self.parse_declaration()?);
        }
        Ok(ast::Program { decls })
    }

    // Declarations
    // Declaration -> FunctionDecl | VariableDecl
    // FunctionDecl -> Type Identifier '(' [Parameters] ')' Block
    // VariableDecl -> Type Declarator { ',' Declarator }* ';'
    fn parse_declaration(&mut self) -> Result<ast::Declaration, ParseError> {
        let ty = self.expect_type_token()?;
        let name = self.expect_identifier()?;

        if matches!(self.peek(), Token::ParenL) {
            let func = self.parse_function_decl_from_header(ty, name)?;
            Ok(ast::Declaration::Function(func))
        } else {
            let var = self.parse_variable_decl_from_header(ty, name)?;
            Ok(ast::Declaration::Variable(var))
        }
    }

    fn parse_function_decl_from_header(
        &mut self,
        ret_type: ast::TypeAnnotation,
        name: String,
    ) -> Result<ast::FunctionDecl, ParseError> {
        self.expect_paren_l()?;
        let params = self.parse_parameters()?;
        self.expect_paren_r()?;
        let body = self.parse_block()?;
        Ok(ast::FunctionDecl {
            ret_type,
            name,
            params,
            body,
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<ast::Parameter>, ParseError> {
        let mut params = Vec::new();
        if matches!(self.peek(), Token::ParenR) {
            return Ok(params);
        }
        loop {
            let ty = self.expect_type_token()?;
            let name = self.expect_identifier()?;
            params.push(ast::Parameter { ty, name });
            if matches!(self.peek(), Token::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        Ok(params)
    }

    fn parse_variable_decl_from_header(
        &mut self,
        ty: ast::TypeAnnotation,
        first_name: String,
    ) -> Result<ast::VariableDecl, ParseError> {
        let mut declarators = Vec::new();
        declarators.push(self.parse_declarator_with_name(first_name)?);

        while matches!(self.peek(), Token::Comma) {
            self.advance();
            let name = self.expect_identifier()?;
            declarators.push(self.parse_declarator_with_name(name)?);
        }

        self.expect_semicolon()?;
        Ok(ast::VariableDecl { ty, declarators })
    }

    fn parse_declarator_with_name(&mut self, name: String) -> Result<ast::Declarator, ParseError> {
        let initializer = if matches!(self.peek(), Token::Assign) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok(ast::Declarator { name, initializer })
    }

    // Statements
    // Block -> '{' { Statement }* '}'
    // Statement -> ReturnStmt | IfStmt | LoopStmt | ExpressionStmt
    // LoopStmt -> WhileStmt | ForStmt
    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        match self.peek() {
            Token::Return => self.parse_return_stmt(),
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::For => self.parse_for_stmt(),
            Token::BraceL => {
                let b = self.parse_block()?;
                Ok(ast::Statement::Block(b))
            }
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_block(&mut self) -> Result<ast::Block, ParseError> {
        self.expect_brace_l()?;
        let mut stmts = Vec::new();
        while !matches!(self.peek(), Token::BraceR | Token::EOF) {
            stmts.push(self.parse_statement()?);
        }
        self.expect_brace_r()?;
        Ok(ast::Block { stmts })
    }

    fn parse_return_stmt(&mut self) -> Result<ast::Statement, ParseError> {
        self.advance(); // consume 'return'
        let expr = if !matches!(self.peek(), Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(ast::Statement::Return(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<ast::Statement, ParseError> {
        self.advance(); // consume 'if'
        self.expect_paren_l()?;
        let cond = self.parse_expression()?;
        self.expect_paren_r()?;
        let then_block = self.parse_block()?;
        let else_block = if matches!(self.peek(), Token::Else) {
            self.advance(); // consume 'else'
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(ast::Statement::If {
            cond,
            then_block,
            else_block,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<ast::Statement, ParseError> {
        self.advance(); // consume 'while'
        self.expect_paren_l()?;
        let cond = self.parse_expression()?;
        self.expect_paren_r()?;
        let body = self.parse_block()?;
        Ok(ast::Statement::While { cond, body })
    }

    fn parse_for_stmt(&mut self) -> Result<ast::Statement, ParseError> {
        self.advance(); // consume 'for'
        self.expect_paren_l()?;

        let init = if !matches!(self.peek(), Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect_semicolon()?;

        let cond = if !matches!(self.peek(), Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect_semicolon()?;

        let post = if !matches!(self.peek(), Token::ParenR) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect_paren_r()?;
        let body = self.parse_block()?;
        Ok(ast::Statement::For {
            init,
            cond,
            post,
            body,
        })
    }

    fn parse_expression_stmt(&mut self) -> Result<ast::Statement, ParseError> {
        let expr = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(ast::Statement::Expr(expr))
    }

    // Expressions (precedence levels)
    // Expression -> Assignment
    // Assignment -> LogicalOr [ ( = | += | -= | ... ) Assignment ]
    fn parse_expression(&mut self) -> Result<ast::Expression, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<ast::Expression, ParseError> {
        let left = self.parse_logical_or()?;
        match self.peek() {
            Token::Assign => {
                self.advance();
                let right = self.parse_assignment()?;
                Ok(ast::Expression::Assignment {
                    left: Box::new(left),
                    op: ast::AssignmentOp::Assign,
                    right: Box::new(right),
                })
            }
            Token::PlusAssign => {
                self.advance();
                let right = self.parse_assignment()?;
                Ok(ast::Expression::Assignment {
                    left: Box::new(left),
                    op: ast::AssignmentOp::AddAssign,
                    right: Box::new(right),
                })
            }
            Token::MinusAssign => {
                self.advance();
                let right = self.parse_assignment()?;
                Ok(ast::Expression::Assignment {
                    left: Box::new(left),
                    op: ast::AssignmentOp::SubAssign,
                    right: Box::new(right),
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_logical_or(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_logical_and()?;
        while matches!(self.peek(), Token::Or) {
            self.advance();
            let rhs = self.parse_logical_and()?;
            expr = ast::Expression::Binary {
                left: Box::new(expr),
                op: ast::BinaryOp::Or,
                right: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_equality()?;
        while matches!(self.peek(), Token::And) {
            self.advance();
            let rhs = self.parse_equality()?;
            expr = ast::Expression::Binary {
                left: Box::new(expr),
                op: ast::BinaryOp::And,
                right: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_relational()?;
        loop {
            let op = match self.peek() {
                Token::Equals => Some(ast::BinaryOp::Eq),
                Token::NotEquals => Some(ast::BinaryOp::Ne),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let rhs = self.parse_relational()?;
                expr = ast::Expression::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(rhs),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_relational(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_additive()?;
        loop {
            let op = match self.peek() {
                Token::Less => Some(ast::BinaryOp::Lt),
                Token::LessEq => Some(ast::BinaryOp::Le),
                Token::Greater => Some(ast::BinaryOp::Gt),
                Token::GreaterEq => Some(ast::BinaryOp::Ge),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let rhs = self.parse_additive()?;
                expr = ast::Expression::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(rhs),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            let op = match self.peek() {
                Token::Plus => Some(ast::BinaryOp::Add),
                Token::Minus => Some(ast::BinaryOp::Sub),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let rhs = self.parse_multiplicative()?;
                expr = ast::Expression::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(rhs),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = match self.peek() {
                Token::Mul => Some(ast::BinaryOp::Mul),
                Token::Div => Some(ast::BinaryOp::Div),
                Token::Mod => Some(ast::BinaryOp::Mod),
                _ => None,
            };
            if let Some(op) = op {
                self.advance();
                let rhs = self.parse_unary()?;
                expr = ast::Expression::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(rhs),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    // Correctly handle prefix and postfix unary operators
    fn parse_unary(&mut self) -> Result<ast::Expression, ParseError> {
        match self.peek() {
            Token::Not => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOp::Not,
                    expr: Box::new(expr),
                })
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOp::Neg,
                    expr: Box::new(expr),
                })
            }
            // Add prefix increment/decrement here
            Token::Increment => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOp::Increment,
                    expr: Box::new(expr),
                })
            }
            Token::Decrement => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOp::Decrement,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_postfix_unary(), // Delegate to a new function for postfix
        }
    }

    // New function to handle postfix unary operators
    fn parse_postfix_unary(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.peek() {
                Token::Increment => {
                    self.advance();
                    expr = ast::Expression::Unary {
                        op: ast::UnaryOp::Increment,
                        expr: Box::new(expr),
                    };
                }
                Token::Decrement => {
                    self.advance();
                    expr = ast::Expression::Unary {
                        op: ast::UnaryOp::Decrement,
                        expr: Box::new(expr),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<ast::Expression, ParseError> {
        match self.advance() {
            Some(Token::Identifier(name)) => {
                if matches!(self.peek(), Token::ParenL) {
                    self.advance();
                    let args = self.parse_call_args()?;
                    self.expect_paren_r()?;
                    Ok(ast::Expression::Call {
                        callee: Box::new(ast::Expression::Identifier(name)),
                        args,
                    })
                } else {
                    Ok(ast::Expression::Identifier(name))
                }
            }
            Some(Token::IntLit(v)) => Ok(ast::Expression::Literal(ast::Literal::Int(v))),
            Some(Token::FloatLit(v)) => Ok(ast::Expression::Literal(ast::Literal::Float(v))),
            Some(Token::StringLit(s)) => Ok(ast::Expression::Literal(ast::Literal::String(s))),
            Some(Token::True) => Ok(ast::Expression::Literal(ast::Literal::Bool(true))),
            Some(Token::False) => Ok(ast::Expression::Literal(ast::Literal::Bool(false))),
            Some(Token::ParenL) => {
                let expr = self.parse_expression()?;
                self.expect_paren_r()?;
                Ok(ast::Expression::Grouping(Box::new(expr)))
            }
            Some(tok) => Err(ParseError::UnexpectedToken(tok)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_call_args(&mut self) -> Result<Vec<ast::Expression>, ParseError> {
        let mut args = Vec::new();
        if matches!(self.peek(), Token::ParenR) {
            return Ok(args);
        }
        loop {
            args.push(self.parse_expression()?);
            if matches!(self.peek(), Token::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        Ok(args)
    }

    // Token helpers & expectations
    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn peek(&self) -> &Token {
        self.current().unwrap_or(&Token::EOF)
    }

    fn peek_n(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.is_at_end() {
            return None;
        }
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        Some(tok)
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Token::EOF)
    }

    fn expect_token(&mut self, expected: &Token) -> Result<Token, ParseError> {
        if self.peek() == expected {
            Ok(self.advance().unwrap())
        } else {
            Err(ParseError::FailedToFindToken(expected.clone()))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.advance() {
            Some(Token::Identifier(s)) => Ok(s),
            Some(_) => Err(ParseError::ExpectedIdentifier),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_type_token(&mut self) -> Result<ast::TypeAnnotation, ParseError> {
        match self.advance() {
            Some(Token::Int) => Ok(ast::TypeAnnotation::Int),
            Some(Token::Float) => Ok(ast::TypeAnnotation::Float),
            Some(Token::Bool) => Ok(ast::TypeAnnotation::Bool),
            Some(Token::String) => Ok(ast::TypeAnnotation::String),
            Some(_) => Err(ParseError::ExpectedTypeToken),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_semicolon(&mut self) -> Result<(), ParseError> {
        match self.advance() {
            Some(Token::Semicolon) => Ok(()),
            Some(_) => Err(ParseError::ExpectedSemicolon),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_paren_l(&mut self) -> Result<(), ParseError> {
        match self.advance() {
            Some(Token::ParenL) => Ok(()),
            Some(_) => Err(ParseError::ExpectedParenL),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_paren_r(&mut self) -> Result<(), ParseError> {
        match self.advance() {
            Some(Token::ParenR) => Ok(()),
            Some(_) => Err(ParseError::ExpectedParenR),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_brace_l(&mut self) -> Result<(), ParseError> {
        match self.advance() {
            Some(Token::BraceL) => Ok(()),
            Some(_) => Err(ParseError::ExpectedBraceL),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_brace_r(&mut self) -> Result<(), ParseError> {
        match self.advance() {
            Some(Token::BraceR) => Ok(()),
            Some(_) => Err(ParseError::ExpectedBraceR),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_comma(&mut self) -> Result<(), ParseError> {
        match self.advance() {
            Some(Token::Comma) => Ok(()),
            Some(_) => Err(ParseError::ExpectedComma),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_literal(&mut self) -> Result<ast::Literal, ParseError> {
        match self.advance() {
            Some(Token::IntLit(i)) => Ok(ast::Literal::Int(i)),
            Some(Token::FloatLit(f)) => Ok(ast::Literal::Float(f)),
            Some(Token::StringLit(s)) => Ok(ast::Literal::String(s)),
            Some(_) => Err(ParseError::ExpectedLiteral),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    // Utilities
    fn unexpected_token_error(&self) -> ParseError {
        ParseError::UnexpectedToken(self.peek().clone())
    }

    fn unexpected_eof_error(&self) -> ParseError {
        ParseError::UnexpectedEOF
    }
}