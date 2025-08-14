use crate::handle::Handle;
use crate::interner::StringInterner;
use crate::span::Span;
use crate::token::BadTokenKind::*;
use crate::token::Symbol::*;
use crate::token::TokenKind::*;
use crate::token::*;
use std::mem;
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    rem: usize,
    pos: usize,
    peeked: Option<Token>,
    idents: StringInterner<IdentId>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut idents = StringInterner::with_capacity(1024);
        for kw in Keyword::VALUES {
            idents.intern(kw.as_str());
        }
        Self {
            src,
            chars: src.chars(),
            rem: src.len(),
            pos: 0,
            peeked: None,
            idents,
        }
    }

    pub fn idents(self) -> StringInterner<IdentId> {
        self.idents
    }

    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }

        unsafe { self.peeked.as_ref().unwrap_unchecked() }
    }

    #[inline]
    pub fn next_is(&mut self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    #[inline]
    pub fn next_if(&mut self, kind: TokenKind) -> Option<Token> {
        self.next_is(kind).then(|| self.next())
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Token {
        match self.peeked.take() {
            Some(tok) => return tok,
            None => (),
        }
        macro_rules! bump {
            [$n:expr; $kind:expr] => {
                {
                    for _ in 0..$n { self.bump(); }
                    $kind
                }
            };
        }
        let c = loop {
            match (self.first(), self.second()) {
                ('/', '/') => {
                    self.bump_while(|c| c != '\n');
                    self.bump(); // '\n'
                }
                ('/', '*') => {
                    let mut depth = 1usize;
                    self.bump(); // '/'
                    self.bump(); // '*'
                    while let Some(c) = self.bump() {
                        match c {
                            '/' if self.first() == '*' => {
                                self.bump();
                                depth += 1;
                            }
                            '*' if self.first() == '/' => {
                                self.bump();
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => (),
                        }
                    }
                }
                (c, _) if c.is_whitespace() => {
                    self.bump_while(|c| c.is_whitespace());
                }
                _ if self.is_eof() => {
                    self.flush();
                    return Token::new(Eof, Span::new(self.pos, self.pos));
                }
                (c, _) => {
                    self.flush();
                    break c;
                }
            }
        };

        let kind = match (c, self.second(), self.third()) {
            ('<', '<', '=') => bump![3; Sym(ShiftLeftEqual)],
            ('>', '>', '=') => bump![3; Sym(ShiftLeftEqual)],
            ('<', '<', _) => bump![2; Sym(ShiftLeft)],
            ('>', '>', _) => bump![2; Sym(ShiftRight)],
            ('&', '&', _) => bump![2; Sym(AndAnd)],
            ('|', '|', _) => bump![2; Sym(BarBar)],
            ('<', '=', _) => bump![2; Sym(LessEqual)],
            ('>', '=', _) => bump![2; Sym(GreaterEqual)],
            ('=', '=', _) => bump![2; Sym(EqualEqual)],
            ('!', '=', _) => bump![2; Sym(NotEqual)],
            ('+', '=', _) => bump![2; Sym(PlusEqual)],
            ('-', '=', _) => bump![2; Sym(MinusEqual)],
            ('*', '=', _) => bump![2; Sym(StarEqual)],
            ('/', '=', _) => bump![2; Sym(SlashEqual)],
            ('%', '=', _) => bump![2; Sym(PercentEqual)],
            ('^', '=', _) => bump![2; Sym(CaretEqual)],
            ('&', '=', _) => bump![2; Sym(AndEqual)],
            ('|', '=', _) => bump![2; Sym(BarEqual)],
            ('-', '>', _) => bump![2; Sym(Arrow)],
            ('<', _, _) => bump![1; Sym(Less)],
            ('>', _, _) => bump![1; Sym(Greater)],
            ('=', _, _) => bump![1; Sym(Equal)],
            ('!', _, _) => bump![1; Sym(Not)],
            ('+', _, _) => bump![1; Sym(Plus)],
            ('-', _, _) => bump![1; Sym(Minus)],
            ('*', _, _) => bump![1; Sym(Star)],
            ('/', _, _) => bump![1; Sym(Slash)],
            ('%', _, _) => bump![1; Sym(Percent)],
            ('^', _, _) => bump![1; Sym(Caret)],
            ('&', _, _) => bump![1; Sym(And)],
            ('|', _, _) => bump![1; Sym(Bar)],
            ('(', _, _) => bump![1; Sym(OpenParen)],
            (')', _, _) => bump![1; Sym(CloseParen)],
            ('{', _, _) => bump![1; Sym(OpenBrace)],
            ('}', _, _) => bump![1; Sym(CloseBrace)],
            ('[', _, _) => bump![1; Sym(OpenBracket)],
            (']', _, _) => bump![1; Sym(CloseBracket)],
            ('~', _, _) => bump![1; Sym(Tilde)],
            (':', _, _) => bump![1; Sym(Colon)],
            (',', _, _) => bump![1; Sym(Comma)],
            ('.', _, _) => bump![1; Sym(Dot)],
            (';', _, _) => bump![1; Sym(Semicolon)],
            ('0'..='9', c2, _) => {
                let mut radix = 10;
                if c == '0' {
                    match c2 {
                        'b' => {
                            radix = 2;
                            self.bump();
                            self.bump();
                        }
                        'o' => {
                            radix = 8;
                            self.bump();
                            self.bump();
                        }
                        'x' => {
                            radix = 16;
                            self.bump();
                            self.bump();
                        }
                        _ if c2.is_ascii_digit() => {
                            self.bump_while(|c| c.is_ascii_digit());
                            return Token::new(Bad(NumPrefixedZero), self.bumped_span());
                        }
                        _ if c2.is_ascii_alphabetic() => {
                            self.bump();
                            self.bump();
                            return Token::new(Bad(NumPrefixedUnknown), self.bumped_span());
                        }
                        _ => {
                            self.bump();
                            return Token::new(Num(0), self.bumped_span());
                        }
                    }
                }
                let mut has_digits = false;
                let mut value = 0u32;
                loop {
                    let c = self.first();
                    if c == '_' {
                        self.bump();
                        continue;
                    }
                    match char::to_digit(c, radix) {
                        Some(digit) => {
                            has_digits = true;
                            self.bump();
                            value =
                                match value.checked_mul(radix).and_then(|x| x.checked_add(digit)) {
                                    Some(value) => value,
                                    None => {
                                        self.bump_while(|c| char::is_digit(c, radix));
                                        return Token::new(Bad(NumOverflow), self.bumped_span());
                                    }
                                };
                        }
                        None => break,
                    }
                    if self.is_eof() {
                        break;
                    }
                }
                if !has_digits {
                    Bad(NumNoDigits)
                } else {
                    Num(value)
                }
            }
            (c, _, _) if is_ident_start(c) => {
                let s = self.chars.as_str();
                self.bump();
                self.bump_while(is_ident_continue);
                let ident = &s[..self.bumped()];
                let ident_id = self.idents.intern(ident);
                if ident_id.to_usize() < Keyword::COUNT {
                    // SAFETY: See `new`
                    let keyword = ident_id.to_usize() as u8;
                    unsafe { Kw(mem::transmute::<u8, Keyword>(keyword)) }
                } else {
                    Ident(ident_id)
                }
            }
            ('"', _, _) => {
                self.bump();
                loop {
                    match self.bump() {
                        Some('"') => break Str,
                        Some('\\') if self.first() == '"' => {
                            self.bump();
                        }
                        None => break Bad(StrUnterminated),
                        _ => (),
                    }
                }
            }
            ('\'', mut c, _) => {
                self.bump();
                self.bump();

                if c == '\\' && self.first() != '\'' {
                    c = match Self::escape_character(&mut self.chars) {
                        Ok(c) => c,
                        Err(bad) => {
                            let span = self.bumped_span();
                            self.bump_while(|c| c != '\'' && c != '\n');
                            self.bump();
                            return Token::new(Bad(bad), span);
                        }
                    };
                }
                match self.first() {
                    '\'' => {
                        self.bump();
                        Num(c as u32)
                    }
                    _ if c == '\'' => Bad(CharEmpty),
                    _ => {
                        self.bump_while(|c| c != '\'' && c != '\n');
                        self.bump();
                        let span = self.bumped_span();
                        return Token::new(Bad(CharTooLong), span);
                    }
                }
            }
            ('∧', _, _) => bump![1; Sym(AndAnd)],
            ('∨', _, _) => bump![1; Sym(BarBar)],
            ('≤', _, _) => bump![1; Sym(LessEqual)],
            ('≥', _, _) => bump![1; Sym(GreaterEqual)],
            ('≠', _, _) => bump![1; Sym(NotEqual)],
            ('→', _, _) => bump![1; Sym(Arrow)],
            _ => bump![1; Bad(Unknown)],
        };

        Token::new(kind, self.bumped_span())
    }

    pub fn str(&self, span: Span) -> Result<String, BadTokenKind> {
        let mut string = String::with_capacity(24);
        let mut chars = self.src[span.start() + 1..span.end() - 1].chars();

        loop {
            match chars.next() {
                Some('\\') => string.push(Self::escape_character(&mut chars)?),
                Some(c) => string.push(c),
                None => break Ok(string),
            }
        }
    }

    fn escape_character(chars: &mut Chars<'_>) -> Result<char, BadTokenKind> {
        match chars.next() {
            Some('\'') => Ok('\''),
            Some('"') => Ok('\"'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('0') => Ok('\0'),
            Some('x') => {
                if let Some(a) = chars.next()
                    && let Some(a) = char::to_digit(a, 16)
                    && let Some(b) = chars.next()
                    && let Some(b) = char::to_digit(b, 16)
                {
                    // SAFETY: 0 <= a,b <= 15
                    Ok(unsafe { char::from_u32_unchecked(a * 16 + b) })
                } else {
                    Err(HexEscapeInvalidDigit)
                }
            }
            Some('u') => {
                if chars.next() != Some('{') {
                    return Err(UniEscapeNoBraces);
                }
                let mut has_digits = false;
                let mut value = 0;
                loop {
                    match chars.next() {
                        Some('_') => continue,
                        Some('}') => {
                            if !has_digits {
                                break Err(UniEscapeNoDigits);
                            }
                            break match char::from_u32(value) {
                                Some(c) => Ok(c),
                                None => Err(UniEscapeNotScalar),
                            };
                        }
                        Some(c) => match char::to_digit(c, 16) {
                            Some(digit) => {
                                has_digits = true;
                                value *= 16;
                                value += digit;
                                if value > 0x10FFFF {
                                    break Err(UniEscapeNotScalar);
                                }
                            }
                            None => break Err(UniEscapeInvalidDigit),
                        },
                        None => break Err(UniEscapeNoBraces),
                    }
                }
            }
            _ => Err(EscapeUnknown),
        }
    }
}

const EOF_CHAR: char = '\0';

impl<'a> Lexer<'a> {
    #[inline]
    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    #[inline]
    fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    #[inline]
    fn second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    #[inline]
    fn third(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    #[inline]
    fn bump_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    #[inline]
    fn bumped(&self) -> usize {
        self.rem - self.chars.as_str().len()
    }

    #[inline]
    fn bumped_span(&self) -> Span {
        Span::new(self.pos, self.pos + self.bumped())
    }

    #[inline]
    fn flush(&mut self) {
        let len = self.chars.as_str().len();
        self.pos += self.rem - len;
        self.rem = len
    }
}

#[inline]
fn is_ident_start(ch: char) -> bool {
    ch == '_' || unicode_ident::is_xid_start(ch)
}

#[inline]
fn is_ident_continue(ch: char) -> bool {
    ch == '_' || unicode_ident::is_xid_continue(ch)
}
