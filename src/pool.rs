use crate::{handle::Handle, span::Span};
use std::{marker::PhantomData, ops::Index};

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

    pub fn insert_many<I: IntoIterator<Item = T>>(&mut self, iter: I) -> Span<H> {
        let start = self.vec.len();
        self.vec.extend(iter);
        Span::new(start, self.vec.len())
    }

    pub fn get<I>(&self, index: I) -> &<Self as Index<I>>::Output
    where
        Pool<T, H>: Index<I>,
    {
        self.index(index)
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }
}

impl<T, H: Handle> Index<H> for Pool<T, H> {
    type Output = T;
    fn index(&self, index: H) -> &Self::Output {
        &self.vec[index.to_usize()]
    }
}

impl<T, H: Handle> Index<Span<H>> for Pool<T, H> {
    type Output = [T];
    fn index(&self, index: Span<H>) -> &Self::Output {
        &self.vec[index.as_range()]
    }
}
