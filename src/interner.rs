use crate::handle::Handle;
use std::collections::HashMap;
use std::hash::Hash;
use std::mem;

/// A generalized interner for any type `T`.
///
/// For `T = String`, use [`StringInterner`].
#[derive(Debug)]
pub struct Interner<T: Eq + Hash + Clone, H: Handle> {
    map: HashMap<T, H>,
    vec: Vec<T>,
}

impl<T: Eq + Hash + Clone, H: Handle> Interner<T, H> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    /// Interns the value, returning its unique handle.
    pub fn intern(&mut self, value: T) -> H {
        if let Some(&handle) = self.map.get(&value) {
            return handle;
        }
        let handle = Handle::from_usize(self.vec.len());
        self.vec.push(value.clone());
        self.map.insert(value, handle);
        handle
    }

    /// Resolves an interned handle back to its original value.
    pub fn resolve(&self, handle: H) -> &T {
        &self.vec[handle.to_usize()]
    }
}

/// A [string interner].
///
/// Based on [Fast and Simple Rust Interner] by matklad.
///
/// [string interner]: https://en.wikipedia.org/wiki/String_interning
/// [Fast and Simple Rust Interner]: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
#[derive(Debug)]
pub struct StringInterner<H: Handle> {
    map: HashMap<&'static str, H>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl<H: Handle> StringInterner<H> {
    pub fn with_capacity(capacity: usize) -> Self {
        let capacity = capacity.next_power_of_two();
        Self {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(capacity),
            full: Vec::new(),
        }
    }

    pub fn intern(&mut self, string: &str) -> H {
        if let Some(&handle) = self.map.get(string) {
            return handle;
        }

        let string = self.alloc(string);
        let handle = H::from_usize(self.map.len());
        self.map.insert(string, handle);
        self.vec.push(string);

        handle
    }

    pub fn resolve(&self, handle: H) -> &str {
        self.vec[handle.to_usize()]
    }

    fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        unsafe { &*(interned as *const str) }
    }
}
