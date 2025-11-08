#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Line {}, Column {}", self.line, self.column)
    }
}

// --- Scope Error Definitions ---

#[derive(Debug, Clone)]
pub enum ScopeError {
    UndeclaredVariableAccessed { 
        name: String, 
        location: Location 
    },
    
    UndefinedFunctionCalled { 
        name: String, 
        location: Location 
    },
    
    VariableRedefinition { 
        name: String, 
        current_location: Location,
        original_location: Location,
    },
    
    FunctionPrototypeRedefinition { 
        name: String, 
        current_location: Location,
        original_location: Location,
    },
}

impl std::fmt::Display for ScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeError::UndeclaredVariableAccessed { name, location } => {
                write!(f, "Scope Error [Undeclared]: Variable or function '{}' accessed at {}.", name, location)
            }
            ScopeError::UndefinedFunctionCalled { name, location } => {
                write!(f, "Scope Error [Undefined Call]: Function '{}' called at {} but is not defined.", name, location)
            }
            ScopeError::VariableRedefinition { name, current_location, original_location } => {
                write!(f, 
                    "Scope Error [Redefinition]: Local variable '{}' redefined at {}. Originally declared at {}.", 
                    name, current_location, original_location
                )
            }
            ScopeError::FunctionPrototypeRedefinition { name, current_location, original_location } => {
                write!(f, 
                    "Scope Error [Global Redefinition]: Global symbol/function '{}' redefined at {}. Originally declared at {}.", 
                    name, current_location, original_location
                )
            }
        }
    }
}