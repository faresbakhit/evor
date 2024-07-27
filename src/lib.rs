pub mod ast;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

pub mod interner {
    pub use string_interner::symbol::SymbolU32 as Symbol;
    pub use string_interner::DefaultStringInterner as Interner;
}
