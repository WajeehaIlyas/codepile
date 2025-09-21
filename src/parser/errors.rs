use crate::lexer::token::Token;

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

    RedeclaredIdentifier(String)
}