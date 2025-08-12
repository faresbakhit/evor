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
pub struct SymbolTable<Symbol: Copy + Eq + Hash, H: Handle, S: BuildHasher = RandomState> {
    scopes: Vec<HashMap<Symbol, H, S>>,
    symbols: Vec<Symbol>,
}

impl<Symbol: Copy + Eq + Hash, H: Handle, S: BuildHasher> Default for SymbolTable<Symbol, H, S> {
    /// Creates an empty `SymbolTable`.
    #[inline]
    fn default() -> Self {
        Self {
            scopes: Vec::new(),
            symbols: Vec::new(),
        }
    }
}

impl<Symbol: Copy + Eq + Hash, H: Handle, S: BuildHasher> SymbolTable<Symbol, H, S> {
    /// Creates an empty `SymbolTable`.
    #[inline]
    pub const fn new() -> Self {
        Self {
            scopes: Vec::new(),
            symbols: Vec::new(),
        }
    }

    /// Construct a new, empty `SymbolTable` with at least the specified scope stack capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            scopes: Vec::with_capacity(capacity),
            symbols: Vec::new(),
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
        debug_assert!(self.scopes.len() > 0, "empty scope stack");
        let top_index = unsafe { self.scopes.len().unchecked_sub(1) };
        let top = unsafe { self.scopes.get_unchecked_mut(top_index) };
        let handle = H::from_usize(self.symbols.len());
        self.symbols.push(sym);
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
        debug_assert!(self.scopes.len() > 0, "empty scope stack");
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

    /// Returns the symbol for a given handle.
    pub fn get(&self, handle: H) -> Symbol {
        self.symbols[handle.to_usize()]
    }

    /// Returns the number of inserted symbols.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }
}

impl<K: Copy + Eq + Hash, V: Handle> SymbolTable<K, V, RandomState> {
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
        let mut t = SymbolTable::<&str, u32>::new();
        t.enter_scope();
        let k1s1 = t.insert("key1");
        let k2s1 = t.insert("key2");
        assert_ne!(k1s1, k2s1);
        assert_eq!(Some(k1s1), t.resolve("key1").copied());
        t.enter_scope();
        let k1s2 = t.insert("key1");
        assert_ne!(k1s1, k1s2);
        assert_eq!(Some(k1s2), t.resolve("key1").copied());
        assert_eq!(Some(k2s1), t.resolve("key2").copied());
        t.exit_scope();
        assert_eq!(Some(k1s1), t.resolve("key1").copied());
    }

    #[test]
    fn insert_non_idempotent() {
        let mut t = SymbolTable::<&str, u32>::new();
        t.enter_scope();
        let k11s1 = t.insert("key1");
        let k12s1 = t.insert("key1");
        assert_ne!(k11s1, k12s1);
        assert_eq!(Some(k12s1), t.resolve("key1").copied());
        t.enter_scope();
        assert_eq!(Some(k12s1), t.resolve("key1").copied());
        let k1s2 = t.insert("key1");
        assert_ne!(k1s2, k11s1);
        assert_ne!(k1s2, k12s1);
        assert_eq!(Some(k1s2), t.resolve("key1").copied());
        t.exit_scope();
        assert_eq!(Some(k12s1), t.resolve("key1").copied());
    }
}
