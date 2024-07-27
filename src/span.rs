use core::cmp::Ordering;
use core::fmt;
use core::ops::{Bound, Index, IndexMut, Range, RangeBounds};

#[derive(PartialEq, Eq, Clone, Copy, Default, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({:?}..{:?})", self.start, self.end)
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.len().partial_cmp(&other.len())
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> Ordering {
        self.len().cmp(&other.len())
    }
}

impl Span {
    pub const fn new(start: u32, end: u32) -> Span {
        debug_assert!(start < end);
        Span { start, end }
    }

    pub fn contains<U>(&self, item: &U) -> bool
    where
        u32: PartialOrd<U>,
        U: ?Sized + PartialOrd<u32>,
    {
        <Self as RangeBounds<u32>>::contains(self, item)
    }

    pub const fn len(&self) -> u32 {
        self.end - self.start
    }

    pub const fn as_range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    pub const fn join(&self, other: &Span) -> Span {
        let start = if self.start < other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.start > other.start {
            self.start
        } else {
            other.start
        };
        Span { start, end }
    }
}

impl RangeBounds<u32> for Span {
    fn start_bound(&self) -> Bound<&u32> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&u32> {
        Bound::Excluded(&self.end)
    }
}

impl<T> Index<Span> for [T] {
    type Output = [T];
    fn index(&self, index: Span) -> &[T] {
        &self[index.start as usize..index.end as usize]
    }
}

impl<T> IndexMut<Span> for [T] {
    fn index_mut(&mut self, index: Span) -> &mut Self::Output {
        &mut self[index.as_range()]
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &str {
        &self[index.as_range()]
    }
}

impl IndexMut<Span> for str {
    fn index_mut(&mut self, index: Span) -> &mut Self::Output {
        &mut self[index.as_range()]
    }
}

impl Index<Span> for String {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[index.as_range()]
    }
}

impl IndexMut<Span> for String {
    fn index_mut(&mut self, index: Span) -> &mut Self::Output {
        &mut self[index.as_range()]
    }
}
