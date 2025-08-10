use std::num::NonZero;

/// A trait for representing compact, copyable handles (IDs) for
/// recursive/[Pool](crate::pool::Pool)-allocated types or any other index-based
/// data structure (e.g., [Interner](crate::interner::Interner), [SymbolTable](crate::symbol_table::SymbolTable)).
///
/// Inspired by:
/// - [Flattening ASTs (and Other Compiler Data Structures)](https://www.cs.cornell.edu/~asampson/blog/flattening.html)
/// - [Handles are the better pointers](https://floooh.github.io/2018/06/17/handles-vs-pointers.html)
pub trait Handle: Copy + Ord {
    /// Compile-time reflection so [Span](crate::span::Span)'s `Debug` impl can be nice.
    const NAME: &'static str;
    fn from_usize(n: usize) -> Self;
    fn to_usize(self) -> usize;
}

macro_rules! impl_handle_for_nums {
    ($($ty:ty),*) => {$(
        impl Handle for $ty {
            const NAME: &'static str = stringify!($ty);

            #[inline]
            fn from_usize(idx: usize) -> Self {
                idx as $ty
            }

            #[inline]
            fn to_usize(self) -> usize {
                self as usize
            }
        }

        impl Handle for NonZero<$ty> {
            const NAME: &'static str = concat!("NonZero<", stringify!($ty), ">");

            #[inline]
            fn from_usize(idx: usize) -> Self {
                unsafe { NonZero::new_unchecked((idx + 1) as $ty) }
            }

            #[inline]
            fn to_usize(self) -> usize {
                (self.get() - 1) as usize
            }
        }
    )*};
}

impl_handle_for_nums!(u8, u16, u32, u64, usize);

macro_rules! impl_handle {
    (
        $(
            $(#[$attr:meta])*
            $vis:vis struct $name:ident($type:ty);
        )*
    ) => {
        $(
            $(#[$attr])*
            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
            $vis struct $name($type);

            impl $crate::handle::Handle for $name {
                const NAME: &'static str = stringify!($name);

                #[inline(always)]
                fn from_usize(n: usize) -> $name {
                    $name($crate::handle::Handle::from_usize(n))
                }

                #[inline(always)]
                fn to_usize(self) -> usize {
                    $crate::handle::Handle::to_usize(self.0)
                }
            }
        )*
    };
}

pub(crate) use impl_handle;
