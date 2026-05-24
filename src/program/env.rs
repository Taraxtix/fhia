use std::collections::HashMap;

use crate::parser::expr::{ConstValue, Ty};

struct Scope<'src> {
    types:  HashMap<&'src str, Ty>,
    consts: HashMap<&'src str, ConstValue>,
}

impl Scope<'_> {
    pub fn new() -> Self {
        Self {
            types:  HashMap::new(),
            consts: HashMap::new(),
        }
    }
}

pub struct Env<'src> {
    scopes: Vec<Scope<'src>>,
}

impl<'src> Env<'src> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Ty> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.types.get(name).copied())
    }

    pub fn lookup_const(&self, name: &str) -> Option<&ConstValue> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.consts.get(name))
    }

    pub fn declare(&mut self, name: &'src str, ty: Ty) {
        self.scopes.last_mut().unwrap().types.insert(name, ty);
    }

    pub fn declare_const(&mut self, name: &'src str, val: ConstValue) {
        self.scopes.last_mut().unwrap().consts.insert(name, val);
    }

    pub fn is_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.types.contains_key(name))
    }

    pub fn push_scope(&mut self) { self.scopes.push(Scope::new()); }

    /// # Panics
    /// - Panics if you attempt to pop the root scope
    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() > 1, "attempted to pop the root scope");
        self.scopes.pop();
    }
}
