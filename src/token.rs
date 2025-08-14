use crate::handle::impl_handle;
use crate::span::Span;
use std::fmt;
use std::num::NonZeroU32;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind, self.span)
    }
}

impl Token {
    pub(crate) fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum TokenKind {
    /// A keyword
    Kw(Keyword),
    /// `an_id3ntifier`
    ///
    /// For usage, see [Lexer::ident](crate::lexer::Lexer::ident)
    Ident(IdentId),
    /// `0xff`, 0o777, 31, `b'a'`, `'f'`,
    /// `'\x0A'`, `'\u{1F436}'`, `'🥊'`
    Num(u32),
    /// `"\n\tThis is was\r\n\twritten by a \"Human\" 🤖\n"`
    ///
    /// For usage, see  [Lexer::str](crate::lexer::Lexer::str)
    Str,
    /// A symbol
    Sym(Symbol),
    /// End of input
    Eof,
    /// Bad token
    Bad(BadTokenKind),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            Kw(kw) => kw.fmt(f),
            Ident(_) => f.write_str("identifier"),
            Num(_) => f.write_str("integer literal"),
            Str => f.write_str("string literal"),
            Sym(sym) => sym.fmt(f),
            Eof => f.write_str("end of file"),
            Bad(bad) => bad.fmt(f),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Symbol {
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `=`
    Equal,
    /// `!`
    Not,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `^`
    Caret,
    /// `&`
    And,
    /// `|`
    Bar,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `~`
    Tilde,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `;`
    Semicolon,
    /// `<<`
    ShiftLeft,
    /// `>>`
    ShiftRight,
    /// `&&`
    AndAnd,
    /// `||`
    BarBar,
    /// `<=`
    LessEqual,
    /// `>=`
    GreaterEqual,
    /// `==`
    EqualEqual,
    /// `!=`
    NotEqual,
    /// `+=`
    PlusEqual,
    /// `-=`
    MinusEqual,
    /// `*=`
    StarEqual,
    /// `/=`
    SlashEqual,
    /// `%=`
    PercentEqual,
    /// `^=`
    CaretEqual,
    /// `&=`
    AndEqual,
    /// `|=`
    BarEqual,
    /// `->`
    Arrow,
    /// `<<=`
    ShiftLeftEqual,
    /// `>>=`
    ShiftRightEqual,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Symbol::*;
        let s = match self {
            Less => "`<`",
            Greater => "`>`",
            Equal => "`=`",
            Not => "`!`",
            Plus => "`+`",
            Minus => "`-`",
            Star => "`*`",
            Slash => "`/`",
            Percent => "`%`",
            Caret => "`^`",
            And => "`&`",
            Bar => "`|`",
            OpenParen => "`(`",
            CloseParen => "`)`",
            OpenBrace => "`{`",
            CloseBrace => "`}`",
            OpenBracket => "`[`",
            CloseBracket => "`]`",
            Tilde => "`~`",
            Colon => "`:`",
            Comma => "`,`",
            Dot => "`.`",
            Semicolon => "`;`",
            ShiftLeft => "`<<`",
            ShiftRight => "`>>`",
            AndAnd => "`&&`",
            BarBar => "`||`",
            LessEqual => "`<=`",
            GreaterEqual => "`>=`",
            EqualEqual => "`==`",
            NotEqual => "`!=`",
            PlusEqual => "`+=`",
            MinusEqual => "`-=`",
            StarEqual => "`*=`",
            SlashEqual => "`/=`",
            PercentEqual => "`%=`",
            CaretEqual => "`^=`",
            AndEqual => "`&=`",
            BarEqual => "`|=`",
            Arrow => "`->`",
            ShiftLeftEqual => "`<<=`",
            ShiftRightEqual => "`>>=`",
        };
        f.write_str(s)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub enum Keyword {
    /// `bool`
    Bool,
    /// `break`
    Break,
    /// `continue`
    Continue,
    /// `else`
    Else,
    /// `false`
    False,
    /// `for`
    For,
    /// `i32`
    I32,
    /// `if`
    If,
    /// `return`
    Return,
    /// `struct`
    Struct,
    /// `true`
    True,
    /// `u8`
    U8,
    /// `_`
    Underscore,
    /// `var`
    Var,
    /// `void`
    Void,
    /// `while`
    While,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Keyword::*;
        let s = match self {
            Bool => "keyword `bool`",
            Break => "keyword `break`",
            Continue => "keyword `continue`",
            Else => "keyword `else`",
            False => "keyword `false`",
            For => "keyword `for`",
            I32 => "keyword `i32`",
            If => "keyword `if`",
            Return => "keyword `return`",
            Struct => "keyword `struct`",
            True => "keyword `true`",
            U8 => "keyword `u8`",
            Underscore => "keyword `_`",
            Var => "keyword `var`",
            Void => "keyword `void`",
            While => "keyword `while`",
        };
        f.write_str(s)
    }
}

const _: () = assert!(std::mem::size_of::<Keyword>() == 1);

impl Keyword {
    pub const COUNT: usize = 16;

    pub const VALUES: [Self; Self::COUNT] = [
        Self::Bool,
        Self::Break,
        Self::Continue,
        Self::Else,
        Self::False,
        Self::For,
        Self::I32,
        Self::If,
        Self::Return,
        Self::Struct,
        Self::True,
        Self::U8,
        Self::Underscore,
        Self::Var,
        Self::Void,
        Self::While,
    ];

    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Bool => "bool",
            Self::Break => "break",
            Self::Continue => "continue",
            Self::Else => "else",
            Self::False => "false",
            Self::For => "for",
            Self::I32 => "i32",
            Self::If => "if",
            Self::Return => "return",
            Self::Struct => "struct",
            Self::True => "true",
            Self::U8 => "u8",
            Self::Underscore => "_",
            Self::Var => "var",
            Self::Void => "void",
            Self::While => "while",
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub enum BadTokenKind {
    NumNoDigits,
    NumOverflow,
    NumPrefixedUnknown,
    NumPrefixedZero,
    StrUnterminated,
    CharEmpty,
    CharTooLong,
    EscapeUnknown,
    HexEscapeInvalidDigit,
    UniEscapeInvalidDigit,
    UniEscapeNoBraces,
    UniEscapeNoDigits,
    UniEscapeNotScalar,
    Unknown,
}

impl fmt::Display for BadTokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BadTokenKind::*;
        let s = match self {
            NumNoDigits => "integer literal without digits",
            NumOverflow => "integer literal out of allowed range",
            NumPrefixedUnknown => "unknown integer literal prefix",
            NumPrefixedZero => "leading zero integer literal (use 0o.. for octal literals)",
            StrUnterminated => "unterminated string literal",
            CharEmpty => "empty character literal",
            CharTooLong => "character literal with too many characters",
            EscapeUnknown => "unknown escape character",
            HexEscapeInvalidDigit => "invalid digit in hex escape",
            UniEscapeInvalidDigit => "invalid digit in unicode escape",
            UniEscapeNoBraces => "unicode escape without braces (use as `\\u{...}`)",
            UniEscapeNoDigits => "unicode escape without digits",
            UniEscapeNotScalar => "unicode escape character out of allowed ranges",
            Unknown => "unknown token",
        };
        f.write_str(s)
    }
}

impl_handle! { pub struct IdentId(NonZeroU32); }
