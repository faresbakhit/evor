use crate::{handle::Handle, span::Span};
use std::marker::PhantomData;

/// A [`Handle`]/index-based arena without deletion.
///
/// Also see:
/// - [slotmap](https://docs.rs/slotmap/)
#[derive(Default, Debug)]
pub struct Pool<T, H: Handle> {
    vec: Vec<T>,
    marker: PhantomData<H>,
}

impl<T, H: Handle> Pool<T, H> {
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            marker: PhantomData,
        }
    }

    pub fn insert(&mut self, value: T) -> H {
        let handle = H::from_usize(self.vec.len());
        self.vec.push(value);
        handle
    }

    pub fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) -> Span<H> {
        let start = self.vec.len();
        self.vec.extend(iter);
        Span::new(start, self.vec.len())
    }

    pub fn get(&self, handle: H) -> &T {
        &self.vec[handle.to_usize()]
    }
}
