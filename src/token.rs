use crate::span::Span;

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub enum TokenKind {
    /// "<"
    Less,
    /// ">"
    Greater,
    /// "="
    Equal,
    /// "!"
    Not,
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "%"
    Percent,
    /// "^"
    Caret,
    /// "&"
    And,
    /// "|"
    Bar,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "~"
    Tilde,
    /// ":"
    Colon,
    /// ","
    Comma,
    /// ";"
    Semicolon,

    /// "::"
    DoubleColon,
    /// "&&"
    AndAnd,
    /// "||"
    BarBar,
    /// "<<"
    ShiftLeft,
    /// ">>"
    ShiftRight,
    /// "<="
    LessEqual,
    /// ">="
    GreaterEqual,
    /// "=="
    EqualEqual,
    /// "!="
    NotEqual,
    /// "+="
    PlusEqual,
    /// "-="
    MinusEqual,
    /// "*="
    StarEqual,
    /// "/="
    SlashEqual,
    /// "%="
    PercentEqual,
    /// "^="
    CaretEqual,
    /// "&="
    AndEqual,
    /// "|="
    BarEqual,
    /// "->"
    Arrow,

    /// "<<="
    ShiftLeftEqual,
    /// ">>="
    ShiftRightEqual,

    /// "an_id3ntifier"
    Ident,
    /// "0b101", "50", "0o764", "0x1388"
    Int(Base),
    /// "0b", "0o", "0x"
    IntNoDigits(Base),
    /// "3.11e1", "14E+2", "3.0", "271.8e-2"
    Float,
    /// "314.1e-", "55e"
    FloatNoExpo,
    /// "'\x08'", "'\x16'", "'('", "'8'"
    Char,
    /// ""\n\tThis is was\r\n\twritten by a \"Human\" 🤖\n""
    String,

    // Unknwon token.
    Unknown,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash, Debug)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}
