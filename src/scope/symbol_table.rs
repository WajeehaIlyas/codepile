use std::collections::HashMap;
use crate::scope::errors::Location; 

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, Clone)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
    parent: Option<Box<Scope>>,
    pub child_scopes: HashMap<String, Scope>,
}

impl Scope {
    pub fn new(parent: Option<Box<Scope>>) -> Self {
        Scope {
            symbols: HashMap::new(),
            parent,
            child_scopes: HashMap::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol) -> Result<(), Symbol> {
        if self.symbols.contains_key(&symbol.name) {
            let original_symbol = self.symbols.get(&symbol.name).unwrap().clone();
            Err(original_symbol)
        } else {
            self.symbols.insert(symbol.name.clone(), symbol);
            Ok(())
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol);
        }
        
        if let Some(parent_scope) = &self.parent {
            return parent_scope.lookup(name);
        }

        None
    }
    
    pub fn lookup_current(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn take_parent(&mut self) -> Option<Box<Scope>> {
        self.parent.take()
    }
    
    pub fn get_child_scope(&self, name: &str) -> Option<&Scope> {
        self.child_scopes.get(name)
    }
}
