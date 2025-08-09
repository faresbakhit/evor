use crate::handle::{impl_handle, Handle};
use std::fmt;
use std::ops::{Index, IndexMut, Range};

impl_handle! {
    /// A byte offset.
    pub struct ByteIdx(u32);
}

/// A [`Handle`]-based range over indexed elements (e.g., source positions, AST nodes)
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span<H: Handle = ByteIdx> {
    start: H,
    end: H,
}

impl<H: Handle> fmt::Debug for Span<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({:?}..{:?})", H::NAME, self.start(), self.end())
    }
}

impl<H: Handle> Span<H> {
    pub fn new(start: usize, end: usize) -> Span<H> {
        debug_assert!(start <= end);
        Span {
            start: H::from_usize(start),
            end: H::from_usize(end),
        }
    }

    #[inline(always)]
    pub fn start(&self) -> usize {
        self.start.to_usize()
    }

    #[inline(always)]
    pub fn end(&self) -> usize {
        self.end.to_usize()
    }

    #[inline(always)]
    pub fn as_range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.end() - self.start()
    }

    #[inline(always)]
    pub fn join(&self, other: &Span<H>) -> Span<H> {
        let start = if self.start < other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end > other.end {
            self.end
        } else {
            other.end
        };
        Span { start, end }
    }
}

macro_rules! impl_index {
    ($(impl $(<$generic:ident>)? for $type:ty => $output_type:ty;)*) => {
        $(
            impl<H: Handle $(, $generic)?> Index<Span<H>> for $type {
                type Output = $output_type;

                #[inline(always)]
                fn index(&self, index: Span<H>) -> &$output_type {
                    &self[index.as_range()]
                }
            }

            impl<H: Handle $(, $generic)?> IndexMut<Span<H>> for $type {
                #[inline(always)]
                fn index_mut(&mut self, index: Span<H>) -> &mut $output_type {
                    &mut self[index.as_range()]
                }
            }
        )*
    };
}

impl_index! {
    impl<U> for [U] => [U];
    impl for str => str;
    impl for String => str;
}
