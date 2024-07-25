use crate::ast::*;
use crate::lexer::Lexer;
use crate::span::Span;
use crate::token::TokenKind;
use ErrorKind::*;

pub struct Parser<'source> {
    pub buf: &'source str,
    tokens: Lexer<'source>,
    ctx_span: Span,
}

impl<'source> Parser<'source> {
    pub fn new(buf: &'source str) -> Parser<'source> {
        Parser {
            buf,
            tokens: Lexer::new(buf),
            ctx_span: Span::default(),
        }
    }
}

impl Parser<'_> {
    pub fn parse_identifier(&mut self) -> Result<Identifier> {
        let token = match self.tokens.next() {
            Some(token) => token,
            None => return Err(Error::new(Expected(TokenKind::Identifier), self.ctx_span)),
        };
        match token.kind {
            TokenKind::Identifier => {
                todo!()
            }
            found => Err(Error::new(
                ExpectedFound(TokenKind::Identifier, found),
                token.span,
            )),
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}

impl Error {
    const fn new(kind: ErrorKind, span: Span) -> Error {
        Error { kind, span }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
enum ErrorKind {
    Expected(TokenKind),
    ExpectedFound(TokenKind, TokenKind),
}
