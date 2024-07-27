use crate::ast::*;
use crate::lexer::Lexer;
use crate::span::Span;
use crate::token::TokenKind;
use core::iter::Peekable;
use string_interner::DefaultStringInterner as Interner;

#[derive(Debug, Clone)]
pub struct Parser<'source> {
    buf: &'source str,
    tokens: Peekable<Lexer<'source>>,
    interner: Interner,
    ctx_span: Span,
}

impl<'source> Parser<'source> {
    pub fn new(buf: &'source str) -> Parser<'source> {
        Parser {
            buf,
            tokens: Lexer::new(buf).peekable(),
            interner: Interner::new(),
            ctx_span: Span::default(),
        }
    }
}

impl Parser<'_> {
    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_expr_with_bp(0)
    }

    fn parse_expr_with_bp(&mut self, min_bp: u8) -> Result<Expr> {
        macro_rules! lit {
            [$kind:expr; $span:expr] => {
                {
                    let kind = $kind;
                    let ss = &self.buf[$span];
                    let sym = self.interner.get_or_intern(ss);
                    let lit = Lit { kind, sym, span: $span };
                    let kind = ExprKind::Lit(lit);
                    let span = $span;
                    Expr { kind, span }
                }
            };
        }

        let token = match self.tokens.next() {
            Some(token) => token,
            None => return Error::new(ERROR_EXPECTED_EXPR, self.ctx_span),
        };

        let mut lhs = match token.kind {
            TokenKind::Ident => {
                let ss = &self.buf[token.span];
                let sym = self.interner.get_or_intern(ss);
                let kind = ExprKind::Ident(sym);
                let span = token.span;
                Expr { kind, span }
            }
            TokenKind::Char => lit![LitKind::Char; token.span],
            TokenKind::Float => lit![LitKind::Float; token.span],
            TokenKind::Int(base) => lit![LitKind::Int(base); token.span],
            TokenKind::String => lit![LitKind::String; token.span],
            TokenKind::OpenParen => {
                let mut lhs = self.parse_expr_with_bp(0)?;
                lhs.span.start = token.span.start;
                match self.tokens.next() {
                    Some(close_token) if close_token.kind == TokenKind::CloseParen => {
                        lhs.span.end = close_token.span.end;
                        lhs
                    }
                    _ => return Error::new(ERROR_EXPECTED_CLOSE_PAREN, lhs.span),
                }
            }
            _ => return Error::new(ERROR_EXPECTED_EXPR, self.ctx_span),
        };

        loop {
            let op = match self.tokens.peek().and_then(InfixOp::from_token) {
                Some(op) => op,
                None => break Ok(lhs),
            };

            let (lbp, rbp) = op.binding_power();

            if lbp < min_bp {
                break Ok(lhs);
            }
            self.tokens.next();

            let rhs = self.parse_expr_with_bp(rbp)?;
            let span = Span::new(lhs.span.start, rhs.span.end);
            let kind = ExprKind::Infix(op, Box::new(lhs), Box::new(rhs));
            lhs = Expr { kind, span }
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Error {
    err: &'static str,
    span: Span,
}

impl Error {
    const fn new<T>(err: &'static str, span: Span) -> Result<T> {
        Err(Error { err, span })
    }
}

const ERROR_EXPECTED_CLOSE_PAREN: &str = "expected closing parenthesis ')'";
const ERROR_EXPECTED_EXPR: &str = "expected expression";
