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

    fn flush(&mut self) {
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
            Some('E' | 'e') => true,
            _ => return Int(Base::Decimal),
        };

        if parse_expo {
            self.bump();
            match self.first() {
                Some('+' | '-') => {
                    self.bump();
                    if !self.first().is_some_and(is_digit_continue) {
                        return FloatNoExpo;
                    }
                    self.bump_while(is_digit_continue);
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
            [$n:expr; $kind:expr] => {
                {
                    for _ in 0..$n { self.bump(); }
                    ($n, $kind)
                }
            };
        }

        let c = loop {
            match self.first()? {
                '#' => {
                    self.bump_while(|c| c != '\n');
                    self.bump(); // '\n'
                }
                c if c.is_whitespace() => {
                    self.bump_while(|c| c.is_whitespace());
                }
                c => break c,
            }
        };

        self.flush();

        let (len, kind) = match (c, self.second(), self.third()) {
            ('<', Some('<'), Some('=')) => bump![3; ShiftLeftEqual],
            ('>', Some('>'), Some('=')) => bump![3; ShiftRightEqual],
            (':', Some(':'), _) => bump![2; DoubleColon],
            ('&', Some('&'), _) => bump![2; AndAnd],
            ('|', Some('|'), _) => bump![2; BarBar],
            ('<', Some('<'), _) => bump![2; ShiftLeft],
            ('>', Some('>'), _) => bump![2; ShiftRight],
            ('>', Some('='), _) => bump![2; GreaterEqual],
            ('<', Some('='), _) => bump![2; LessEqual],
            ('=', Some('='), _) => bump![2; EqualEqual],
            ('!', Some('='), _) => bump![2; NotEqual],
            ('+', Some('='), _) => bump![2; PlusEqual],
            ('-', Some('='), _) => bump![2; MinusEqual],
            ('*', Some('='), _) => bump![2; StarEqual],
            ('/', Some('='), _) => bump![2; SlashEqual],
            ('%', Some('='), _) => bump![2; PercentEqual],
            ('^', Some('='), _) => bump![2; CaretEqual],
            ('&', Some('='), _) => bump![2; AndEqual],
            ('|', Some('='), _) => bump![2; BarEqual],
            ('-', Some('>'), _) => bump![2; Arrow],
            ('<', _, _) => bump![1; Less],
            ('>', _, _) => bump![1; Greater],
            ('=', _, _) => bump![1; Equal],
            ('!', _, _) => bump![1; Not],
            ('+', _, _) => bump![1; Plus],
            ('-', _, _) => bump![1; Minus],
            ('*', _, _) => bump![1; Star],
            ('/', _, _) => bump![1; Slash],
            ('%', _, _) => bump![1; Percent],
            ('^', _, _) => bump![1; Caret],
            ('&', _, _) => bump![1; And],
            ('|', _, _) => bump![1; Bar],
            ('(', _, _) => bump![1; OpenParen],
            (')', _, _) => bump![1; CloseParen],
            ('{', _, _) => bump![1; OpenBrace],
            ('}', _, _) => bump![1; CloseBrace],
            ('~', _, _) => bump![1; Tilde],
            (':', _, _) => bump![1; Colon],
            (',', _, _) => bump![1; Comma],
            (';', _, _) => bump![1; Semicolon],
            (c, _, _) if is_ident_start(c) => {
                self.bump_while(is_ident_continue);
                (self.bumped(), Ident)
            }
            (c, _, _) if is_digit_start(c) => 'number: {
                if c != '0' {
                    let kind = self.next_number();
                    let bumped = self.bumped();
                    break 'number (bumped, kind);
                }

                let base = match self.second() {
                    Some('B' | 'b') => {
                        self.bump();
                        self.bump();
                        self.bump_while(is_binary_digit);
                        Base::Binary
                    }
                    Some('O' | 'o') => {
                        self.bump();
                        self.bump();
                        self.bump_while(is_octal_digit);
                        Base::Octal
                    }
                    Some('X' | 'x') => {
                        self.bump();
                        self.bump();
                        self.bump_while(is_hexadecimal_digit);
                        Base::Hexadecimal
                    }
                    _ => {
                        let kind = self.next_number();
                        let bumped = self.bumped();
                        break 'number (bumped, kind);
                    }
                };

                let bumped = self.bumped();

                let kind = if bumped == 2 {
                    Int(base)
                } else {
                    IntNoDigits(base)
                };

                (bumped, kind)
            }
            _ => bump![1; Unknown],
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
