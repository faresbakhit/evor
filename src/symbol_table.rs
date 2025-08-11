use std::{
    borrow::Borrow,
    collections::HashMap,
    hash::{BuildHasher, Hash, RandomState},
};

use crate::handle::Handle;

/// A [symbol table].
///
/// [symbol table]: https://en.wikipedia.org/wiki/Symbol_table
#[derive(Debug)]
pub struct SymbolTable<Symbol: Eq + Hash, H: Handle, S: BuildHasher = RandomState> {
    scopes: Vec<HashMap<Symbol, H, S>>,
    symbol_count: usize,
}

impl<Symbol: Eq + Hash, H: Handle, S: BuildHasher> Default for SymbolTable<Symbol, H, S> {
    /// Creates an empty `SymbolTable`.
    #[inline]
    fn default() -> Self {
        Self {
            scopes: Vec::new(),
            symbol_count: 0,
        }
    }
}

impl<Symbol: Eq + Hash, H: Handle, S: BuildHasher> SymbolTable<Symbol, H, S> {
    /// Creates an empty `SymbolTable`.
    #[inline]
    pub const fn new() -> Self {
        Self {
            scopes: Vec::new(),
            symbol_count: 0,
        }
    }

    /// Construct a new, empty `SymbolTable` with at least the specified scope stack capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            scopes: Vec::with_capacity(capacity),
            symbol_count: 0,
        }
    }

    /// Enter a new scope which will use the given hash builder to hash symbols.
    pub fn enter_scope_with_hasher(&mut self, hash_builder: S) {
        self.scopes.push(HashMap::with_hasher(hash_builder))
    }

    /// Enter a new scope with at least the specified symbol capacity, using
    /// `hasher` to hash the symbols.
    pub fn enter_scope_with_capacity_and_hasher(&mut self, capacity: usize, hasher: S) {
        self.scopes
            .push(HashMap::with_capacity_and_hasher(capacity, hasher))
    }

    /// Exit the top most scope.
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Insert a symbol onto the top most scope.
    ///
    /// This method is non-idempotent, meaning that if the same symbol is
    /// inserted multiple times within the same scope, a new handle is
    /// generated each time, and the previous handle becomes invalid.
    ///
    /// # Safety
    ///
    /// Calling this method on an empty scope stack is undefined behavior.
    pub fn insert(&mut self, sym: Symbol) -> H {
        let top_index = unsafe { self.scopes.len().unchecked_sub(1) };
        let top = unsafe { self.scopes.get_unchecked_mut(top_index) };
        let handle = H::from_usize(self.symbol_count);
        self.symbol_count += 1;
        top.insert(sym, handle);
        handle
    }

    /// Retrieve the handle associated with a symbol, if any.
    pub fn resolve<Q: ?Sized>(&self, k: &Q) -> Option<&H>
    where
        Symbol: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.scopes.iter().rev().find_map(|m| m.get(k))
    }

    /// Retrieve the handle associated with a symbol, if any, without doing
    /// bounds checking.
    ///
    /// # Safety
    ///
    /// Calling this method on an empty scope stack is undefined behavior.
    ///
    /// # Note
    ///
    /// This is not actually used, but I'm keeping it for benchmarks.
    pub unsafe fn resolve_unchecked<Q: ?Sized>(&self, k: &Q) -> Option<&H>
    where
        Symbol: Borrow<Q>,
        Q: Hash + Eq,
    {
        let mut i = unsafe { self.scopes.len().unchecked_sub(1) };
        loop {
            if let Some(key) = unsafe { self.scopes.get_unchecked(i) }.get(k) {
                return Some(key);
            }
            if i == 0 {
                return None;
            }
            i = unsafe { i.unchecked_sub(1) };
        }
    }
}

impl<K: Eq + Hash, V: Handle> SymbolTable<K, V, RandomState> {
    /// Enter a new scope.
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Enter a new scope, with at least the specified symbol capacity.
    pub fn enter_scope_with_capacity(&mut self, capacity: usize) {
        self.scopes.push(HashMap::with_capacity(capacity))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_usage() {
        let mut t = SymbolTable::<String, u32>::new();
        t.enter_scope();
        t.insert("key1".to_string());
        assert_eq!(Some(0), t.resolve("key1").copied());
        t.enter_scope();
        t.insert("key1".to_string());
        assert_eq!(Some(1), t.resolve("key1").copied());
        t.exit_scope();
        assert_eq!(Some(0), t.resolve("key1").copied());
    }

    #[test]
    fn insert_non_idempotent() {
        let mut t = SymbolTable::<String, u32>::new();
        t.enter_scope();
        t.insert("key1".to_string());
        t.insert("key1".to_string());
        assert_eq!(Some(1), t.resolve("key1").copied());
        t.enter_scope();
        assert_eq!(Some(1), t.resolve("key1").copied());
        t.insert("key1".to_string());
        assert_eq!(Some(2), t.resolve("key1").copied());
        t.exit_scope();
        assert_eq!(Some(1), t.resolve("key1").copied());
    }
}
