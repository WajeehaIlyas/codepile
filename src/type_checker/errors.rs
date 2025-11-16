use std::fmt;

#[derive(Debug, Clone)]
pub enum TypeChkError {
    /// Variable declaration has an erroneous type or initializer.
    ErroneousVarDecl(String),
    /// Function call has an incorrect number of parameters.
    FnCallParamCount { expected: usize, found: usize },
    /// Function call parameter type mismatch.
    FnCallParamType { expected: String, found: String },
    /// Return statement type does not match function's declared return type.
    ErroneousReturnType { expected: String, found: String },
    /// Type mismatch in a general expression (e.g., assignment, binary op).
    ExpressionTypeMismatch { expected: String, found: String },
    /// An expression was expected to resolve to a Boolean type (e.g., inside an `if` or `while`).
    ExpectedBooleanExpression,
    /// A break statement was found outside of a loop construct.
    ErroneousBreak,
    /// Condition statement (e.g., in if/while) evaluates to a non-Boolean type.
    NonBooleanCondStmt,
    /// An expression required content but was empty (e.g., `return;` in a non-void function).
    EmptyExpression,
    /// Attempted a boolean operation (AND, OR) on non-Boolean operands.
    AttemptedBoolOpOnNonBools,
    /// Attempted a bitwise operation (e.g., XOR, AND, OR) on non-numeric operands.
    AttemptedBitOpOnNonNumeric,
    /// Attempted a shift operation (LSHIFT, RSHIFT) on non-integer operands.
    AttemptedShiftOnNonInt,
    /// Attempted an additive operation on non-numeric operands.
    AttemptedAddOpOnNonNumeric,
    /// Attempted exponentiation (**) on non-numeric base or exponent.
    AttemptedExponentiationOfNonNumeric,
    /// A return statement was expected at the end of a non-void function, but none was found.
    ReturnStmtNotFound,
    /// A general error to catch any other unhandled type issues.
    GeneralTypeCheckError(String),
}

impl fmt::Display for TypeChkError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeChkError::ErroneousVarDecl(details) => write!(f, "Erroneous variable declaration: {}", details),
            TypeChkError::FnCallParamCount { expected, found } => write!(f, "Function call parameter count mismatch: Expected {} but found {}", expected, found),
            TypeChkError::FnCallParamType { expected, found } => write!(f, "Function call parameter type mismatch: Expected '{}' but found '{}'", expected, found),
            TypeChkError::ErroneousReturnType { expected, found } => write!(f, "Function return type mismatch: Expected '{}' but found '{}'", expected, found),
            TypeChkError::ExpressionTypeMismatch { expected, found } => write!(f, "Expression type mismatch: Expected '{}' but found '{}'", expected, found),
            TypeChkError::ExpectedBooleanExpression => write!(f, "Expected a boolean expression"),
            TypeChkError::ErroneousBreak => write!(f, "Break statement found outside of a loop"),
            TypeChkError::NonBooleanCondStmt => write!(f, "Conditional statement requires a boolean expression"),
            TypeChkError::EmptyExpression => write!(f, "Expression is empty where content is required"),
            TypeChkError::AttemptedBoolOpOnNonBools => write!(f, "Attempted boolean operation (AND, OR) on non-boolean operands"),
            TypeChkError::AttemptedBitOpOnNonNumeric => write!(f, "Attempted bitwise operation on non-numeric operands"),
            TypeChkError::AttemptedShiftOnNonInt => write!(f, "Attempted shift operation on non-integer operands"),
            TypeChkError::AttemptedAddOpOnNonNumeric => write!(f, "Attempted additive operation on non-numeric operands"),
            TypeChkError::AttemptedExponentiationOfNonNumeric => write!(f, "Attempted exponentiation on non-numeric base or exponent"),
            TypeChkError::ReturnStmtNotFound => write!(f, "Return statement not found in non-void function"),
            TypeChkError::GeneralTypeCheckError(details) => write!(f, "General Type Check Error: {}", details),
        }
    }
}