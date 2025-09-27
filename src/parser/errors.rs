use crate::lexer::token::Token; // adjust path if necessary
use std::fmt;

#[derive(Debug, Clone)]
pub enum ParseError {
    // End-of-file errors
    UnexpectedEOF,

    // Token-related errors
    FailedToFindToken(Token),
    UnexpectedToken(Token),

    // Expected specific tokens
    ExpectedSemicolon,
    ExpectedParenL,
    ExpectedParenR,
    ExpectedBraceL,
    ExpectedBraceR,
    ExpectedDot,
    ExpectedComma,

    // Missing Operators
    ExpectedAssignOp,
    ExpectedLogicalOp,
    ExpectedRelationalOp,
    ExpectedAdditiveOp,
    ExpectedMultiplicativeOp,

    // Expected specific language constructs
    ExpectedTypeToken,
    ExpectedIdentifier,
    ExpectedExpression,
    ExpectedLiteral,
    ExpectedIntLit,
    ExpectedFloatLit,
    ExpectedStringLit,
    ExpectedBoolLit,
    ExpectedStatement,
    ExpectedBlock,
    ExpectedKeyword(String),

    RedeclaredIdentifier(String),
}

// printing for errors
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseError::*;
        match self {
            UnexpectedEOF => write!(f, "Unexpected end of input."),
            FailedToFindToken(tok) => write!(f, "Failed to find expected token: {:?}.", tok),
            UnexpectedToken(tok) => write!(f, "Unexpected token: {:?}.", tok),

            ExpectedSemicolon => write!(f, "Expected ';' (semicolon)."),
            ExpectedParenL => write!(f, "Expected '(' (left parenthesis)."),
            ExpectedParenR => write!(f, "Expected ')' (right parenthesis)."),
            ExpectedBraceL => write!(f, "Expected '{{' (left brace)."),
            ExpectedBraceR => write!(f, "Expected '}}' (right brace)."),
            ExpectedDot => write!(f, "Expected '.' (dot/statement terminator)."),
            ExpectedComma => write!(f, "Expected ',' (comma)."),

            ExpectedAssignOp => write!(f, "Expected assignment operator '='."),
            ExpectedLogicalOp => write!(f, "Expected logical operator (&& or ||)."),
            ExpectedRelationalOp => write!(f, "Expected relational operator (==, !=, <, <=, >, >=)."),
            ExpectedAdditiveOp => write!(f, "Expected additive operator (+ or -)."),
            ExpectedMultiplicativeOp => write!(f, "Expected multiplicative operator (*, / or %)."),

            ExpectedTypeToken => write!(f, "Expected a type token (e.g., ginti / int / float / bool / string)."),
            ExpectedIdentifier => write!(f, "Expected an identifier (variable or function name)."),
            ExpectedExpression => write!(f, "Expected an expression."),
            ExpectedLiteral => write!(f, "Expected a literal (int, float, string, bool)."),
            ExpectedIntLit => write!(f, "Expected an integer literal."),
            ExpectedFloatLit => write!(f, "Expected a float literal."),
            ExpectedStringLit => write!(f, "Expected a string literal."),
            ExpectedBoolLit => write!(f, "Expected a boolean literal."),
            ExpectedStatement => write!(f, "Expected a statement."),
            ExpectedBlock => write!(f, "Expected a block '{{ ... }}' ."),
            ExpectedKeyword(s) => write!(f, "Expected keyword '{}'.", s),

            RedeclaredIdentifier(name) => write!(f, "Redeclared identifier: '{}'.", name),
        }
    }   
}   

// optional: implement std::error::Error
impl std::error::Error for ParseError {}
