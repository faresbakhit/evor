use std::str::Chars;

use crate::span::Span;
use crate::token::Base;
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenKind::*;

#[derive(Debug, Clone)]
pub struct Lexer<'source> {
    chars: Chars<'source>,
    rem: usize,
    pos: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(buf: &'source str) -> Self {
        Self {
            chars: buf.chars(),
            rem: buf.len(),
            pos: 0,
        }
    }
}

impl Lexer<'_> {
    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn first(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next()
    }

    fn second(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next()
    }

    fn third(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next()
    }

    fn second_eq(&self, ch: char) -> bool {
        self.second() == Some(ch)
    }

    fn third_eq(&self, ch: char) -> bool {
        self.third() == Some(ch)
    }

    fn bump_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.first() {
            if !predicate(c) {
                break;
            }
            self.bump();
        }
    }

    fn bumped(&self) -> usize {
        self.rem - self.chars.as_str().len()
    }

    fn reset(&mut self) {
        let len = self.chars.as_str().len();
        self.pos += self.rem - len;
        self.rem = len
    }
}

impl Lexer<'_> {
    fn next_number(&mut self) -> TokenKind {
        self.bump_while(is_digit_continue);

        let parse_expo = match self.first() {
            Some('.') => {
                self.bump();
                self.bump_while(is_digit_continue);
                matches!(self.first(), Some('e' | 'E'))
            }
            Some('E') | Some('e') => true,
            _ => return Integer(Base::Decimal),
        };

        if parse_expo {
            self.bump();
            match self.first() {
                Some('+' | '-') => {
                    self.bump();
                    match self.first() {
                        Some(c) if is_digit_continue(c) => self.bump_while(is_digit_continue),
                        _ => return FloatNoExpo,
                    }
                }
                Some(c) if is_digit_continue(c) => self.bump_while(is_digit_continue),
                _ => return FloatNoExpo,
            }
        }

        Float
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! bump {
            [$n:expr, $kind:expr] => {
                {
                    for _ in 0..$n { self.bump(); }
                    ($n, $kind)
                }
            };
        }

        let c = loop {
            match self.first()? {
                '#' => self.bump_while(|c| c != '\n'),
                c if c.is_whitespace() => self.bump_while(|c| c.is_whitespace()),
                c => break c,
            }
        };

        self.reset();

        let (len, kind) = match c {
            '<' if self.second_eq('<') && self.third_eq('=') => bump![3, ShiftLeftEqual],
            '>' if self.second_eq('>') && self.third_eq('=') => bump![3, ShiftRightEqual],
            ':' if self.second_eq(':') => bump![2, DoubleColon],
            '<' if self.second_eq('<') => bump![2, ShiftLeft],
            '>' if self.second_eq('>') => bump![2, ShiftRight],
            '>' if self.second_eq('=') => bump![2, GreaterEqual],
            '<' if self.second_eq('=') => bump![2, LessEqual],
            '=' if self.second_eq('=') => bump![2, EqualEqual],
            '!' if self.second_eq('=') => bump![2, NotEqual],
            '+' if self.second_eq('=') => bump![2, PlusEqual],
            '-' if self.second_eq('=') => bump![2, MinusEqual],
            '*' if self.second_eq('=') => bump![2, StarEqual],
            '/' if self.second_eq('=') => bump![2, SlashEqual],
            '%' if self.second_eq('=') => bump![2, PercentEqual],
            '^' if self.second_eq('=') => bump![2, CaretEqual],
            '&' if self.second_eq('=') => bump![2, AmperEqual],
            '|' if self.second_eq('=') => bump![2, BarEqual],
            '-' if self.second_eq('>') => bump![2, Arrow],
            '<' => bump![1, Less],
            '>' => bump![1, Greater],
            '=' => bump![1, Equal],
            '!' => bump![1, Not],
            '+' => bump![1, Plus],
            '-' => bump![1, Minus],
            '*' => bump![1, Star],
            '/' => bump![1, Slash],
            '%' => bump![1, Percent],
            '^' => bump![1, Caret],
            '&' => bump![1, Amper],
            '|' => bump![1, Bar],
            '(' => bump![1, OpenParen],
            ')' => bump![1, CloseParen],
            '{' => bump![1, OpenBrace],
            '}' => bump![1, CloseBrace],
            '~' => bump![1, Tilde],
            ':' => bump![1, Colon],
            ',' => bump![1, Comma],
            ';' => bump![1, Semicolon],
            c if is_ident_start(c) => {
                self.bump_while(is_ident_continue);
                (self.bumped(), Identifier)
            }
            c if is_digit_start(c) => {
                let mut kind = match self.second() {
                    Some('b' | 'B') => {
                        self.bump();
                        self.bump();
                        self.bump_while(is_binary_digit);
                        Integer(Base::Binary)
                    }
                    Some('o' | 'O') => {
                        self.bump();
                        self.bump();
                        self.bump_while(is_octal_digit);
                        Integer(Base::Octal)
                    }
                    Some('x' | 'X') => {
                        self.bump();
                        self.bump();
                        self.bump_while(is_hexadecimal_digit);
                        Integer(Base::Hexadecimal)
                    }
                    _ => self.next_number(),
                };

                let bumped = self.bumped();

                if let Integer(base) = kind {
                    if bumped == 2 {
                        kind = IntegerNoDigits(base);
                    } else {
                        kind = Integer(base);
                    }
                };

                (bumped, kind)
            }
            _ => bump![1, Unknown],
        };

        let span = Span::new(self.pos as u32, (self.pos + len) as u32);
        let token = Token { kind, span };
        Some(token)
    }
}

// Utility Functions
// =================

const fn is_digit_start(ch: char) -> bool {
    ch.is_ascii_digit()
}

const fn is_digit_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_digit()
}

const fn is_binary_digit(ch: char) -> bool {
    matches!(ch, '0' | '1' | '_')
}

const fn is_octal_digit(ch: char) -> bool {
    matches!(ch, '0'..='7' | '_')
}

const fn is_hexadecimal_digit(ch: char) -> bool {
    matches!(ch, '0'..='9' | 'A'..='Z' | 'a'..='z')
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || unicode_xid::UnicodeXID::is_xid_start(ch)
}

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || unicode_xid::UnicodeXID::is_xid_continue(ch)
}
