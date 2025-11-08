use std::collections::HashMap;
use crate::scope::errors::Location; 

#[derive(Debug, Clone)]
pub enum Type {
    Int, 
    Float, 
    String, 
    Bool, 
    Void,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Variable { type_info: Type },
    Function { return_type: Type, param_types: Vec<Type> },
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub declared_at: Location,
}


// Spaghetti Stack Node
#[derive(Debug)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(parent: Option<Box<Scope>>) -> Self {
        Scope {
            symbols: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, symbol: Symbol) -> Result<(), Symbol> {
        if self.symbols.contains_key(&symbol.name) {
            // Re-definition found in the current scope. Return the existing symbol.
            let original_symbol = self.symbols.get(&symbol.name).unwrap().clone();
            Err(original_symbol) 
        } else {
            // No conflict, insert the new symbol.
            self.symbols.insert(symbol.name.clone(), symbol);
            Ok(())
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Check current scope's HashMap
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol);
        }
        
        // Recursively check the parent scope
        if let Some(parent_scope) = &self.parent {
            return parent_scope.lookup(name);
        }

        // Not found in the entire chain
        None
    }
    
    /// Looks up a symbol only in the current scope
    pub fn lookup_current(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn take_parent(&mut self) -> Option<Box<Scope>> {
        self.parent.take()
    }
}