use crate::span::Span;
use crate::token::Base;
use crate::token::Token;
use crate::token::TokenKind::*;

macro_rules! token {
    [$kind:expr; $start:expr => $end:expr] => {
        Token {
            kind: $kind,
            span: Span::new($start as u32, $end as u32),
        }
    };

    [$kind:expr; $len:expr, $end:expr] => {
        token![$kind; $end - $len => $end]
    };
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Lexer<'source> {
    buf: &'source [u8],
    pos: usize,
}

impl<'source> Lexer<'source> {
    pub const fn new(buf: &'source [u8]) -> Self {
        Self { buf, pos: 0 }
    }
}

impl Lexer<'_> {
    fn number(&mut self) -> Token {
        let mut len = count(&self.buf[self.pos..], is_digit);

        let parse_expo = match self.buf.get(len) {
            Some(b'.') => {
                len += 1;
                len += count(&self.buf[self.pos + len..], is_digit);
                matches!(self.buf.get(self.pos + len), Some(b'e') | Some(b'E'))
            }
            Some(b'E') | Some(b'e') => true,
            _ => {
                self.pos += len;
                return token![Integer(Base::Decimal); len, self.pos];
            }
        };

        if parse_expo {
            len += 1;
            match self.buf.get(self.pos + len) {
                Some(b'+') | Some(b'-') => {
                    len += 1;
                    match self.buf.get(self.pos + len) {
                        Some(byte) if byte.is_ascii_digit() => (),
                        _ => {
                            self.pos += len;
                            return token![FloatNoExpo; len, self.pos];
                        }
                    }
                }
                Some(byte) if byte.is_ascii_digit() => (),
                _ => {
                    self.pos += len;
                    return token![FloatNoExpo; len, self.pos];
                }
            }
            len += count(&self.buf[self.pos + len..], is_digit);
        }

        self.pos += len;
        token![Float; len, self.pos]
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! peek_eq {
            ($index:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
                match self.buf.get(self.pos + $index) {
                    Some($pattern) $(if $guard)? => true,
                    _ => false
                }
            };
        }

        let byte = loop {
            match self.buf.get(self.pos)? {
                b'#' => self.pos += count(&self.buf[self.pos..], |&x| x != b'\n'),
                byte if byte.is_ascii_whitespace() => {
                    self.pos += count(&self.buf[self.pos..], u8::is_ascii_whitespace);
                }
                byte => break byte,
            }
        };

        let (len, kind) = match byte {
            b'<' if peek_eq!(1, b'<') && peek_eq!(2, b'=') => (3, ShiftLeftEqual),
            b'>' if peek_eq!(1, b'>') && peek_eq!(2, b'=') => (3, ShiftRightEqual),
            b':' if peek_eq!(1, b':') => (2, DoubleColon),
            b'<' if peek_eq!(1, b'<') => (2, ShiftLeft),
            b'>' if peek_eq!(1, b'>') => (2, ShiftRight),
            b'>' if peek_eq!(1, b'=') => (2, GreaterEqual),
            b'<' if peek_eq!(1, b'=') => (2, LessEqual),
            b'=' if peek_eq!(1, b'=') => (2, EqualEqual),
            b'!' if peek_eq!(1, b'=') => (2, NotEqual),
            b'+' if peek_eq!(1, b'=') => (2, PlusEqual),
            b'-' if peek_eq!(1, b'=') => (2, MinusEqual),
            b'*' if peek_eq!(1, b'=') => (2, StarEqual),
            b'/' if peek_eq!(1, b'=') => (2, SlashEqual),
            b'%' if peek_eq!(1, b'=') => (2, PercentEqual),
            b'^' if peek_eq!(1, b'=') => (2, CaretEqual),
            b'&' if peek_eq!(1, b'=') => (2, AmperEqual),
            b'|' if peek_eq!(1, b'=') => (2, BarEqual),
            b'-' if peek_eq!(1, b'>') => (2, Arrow),
            b'<' => (1, Less),
            b'>' => (1, Greater),
            b'=' => (1, Equal),
            b'!' => (1, Not),
            b'+' => (1, Plus),
            b'-' => (1, Minus),
            b'*' => (1, Star),
            b'/' => (1, Slash),
            b'%' => (1, Percent),
            b'^' => (1, Caret),
            b'&' => (1, Amper),
            b'|' => (1, Bar),
            b'(' => (1, OpenParen),
            b')' => (1, CloseParen),
            b'{' => (1, OpenBrace),
            b'}' => (1, CloseBrace),
            b'~' => (1, Tilde),
            b':' => (1, Colon),
            b',' => (1, Comma),
            b';' => (1, Semicolon),
            byte if byte.is_ascii_alphabetic() || *byte == b'_' => {
                let chunk = &self.buf[self.pos..];
                let len = count(chunk, |&byte| byte.is_ascii_alphanumeric() || byte == b'_');
                let ident = &chunk[..len];

                let kind = match ident {
                    b"fn" => Fn,
                    b"var" => Var,
                    b"for" => For,
                    b"if" => If,
                    b"else" => Else,
                    b"break" => Break,
                    b"continue" => Continue,
                    b"defer" => Defer,
                    b"return" => Return,
                    b"true" => True,
                    b"false" => False,
                    _ => Identifier,
                };

                (len, kind)
            }
            byte if byte.is_ascii_digit() => {
                let start = self.pos;

                let base = match byte {
                    _ if peek_eq!(1, b'b' | b'B') => {
                        self.pos += 2;
                        self.pos += count(&self.buf[self.pos..], is_binary_digit);
                        Base::Binary
                    }
                    _ if peek_eq!(1, b'o' | b'O') => {
                        self.pos += 2;
                        self.pos += count(&self.buf[self.pos..], is_octal_digit);
                        Base::Octal
                    }
                    _ if peek_eq!(1, b'x' | b'X') => {
                        self.pos += 2;
                        self.pos = count(&self.buf[self.pos..], is_hexadecimal_digit);
                        Base::Hexadecimal
                    }
                    _ => return Some(self.number()),
                };

                let kind = if start + 2 == self.pos {
                    IntegerNoDigits(base)
                } else {
                    Integer(base)
                };

                return Some(token![kind; start => self.pos]);
            }
            _ => (1, Unknown),
        };

        self.pos += len;
        Some(token![kind; len, self.pos])
    }
}

// Utility Functions
// =================

const fn is_digit(byte: &u8) -> bool {
    byte.is_ascii_digit() || *byte == b'_'
}

const fn is_binary_digit(byte: &u8) -> bool {
    matches!(byte, b'0' | b'1' | b'_')
}

const fn is_octal_digit(byte: &u8) -> bool {
    matches!(byte, b'0'..=b'7' | b'_')
}

const fn is_hexadecimal_digit(byte: &u8) -> bool {
    matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')
}

fn count(slice: &[u8], mut predicate: impl FnMut(&u8) -> bool) -> usize {
    slice
        .iter()
        .position(|b| !predicate(b))
        .unwrap_or(slice.len())
}
