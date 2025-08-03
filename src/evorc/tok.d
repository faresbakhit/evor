/**
 * Defines lexical tokens of the Evor language ([`TokType`]), a data
 * structure that holds a token and its lexeme/location in source
 * code ([`Tok`]), and a function that transforms a source code
 * string or range of unicode characters into a range of [`Tok`]s
 * ([`tokenize`]).
 *
 * FE1: Lexical analysis.
 */

module evorc.tok;

public import evorc.span : Span;
public import std.bigint : BigInt;

import evorc.span;
import std.ascii;
import std.functional;
import std.range.primitives;
import std.sumtype;
import std.traits;
import std.typecons;
import std.uni;
import std.utf;

enum Sym
{
    @("<") less,
    @(">") greater,
    @("=") equal,
    @("!") not,
    @("+") plus,
    @("-") minus,
    @("*") star,
    @("/") slash,
    @("%") percent,
    @("^") caret,
    @("&") and,
    @("|") bar,
    @("(") openParen,
    @(")") closeParen,
    @("{") openBrace,
    @("}") closeBrace,
    @("[") openBracket,
    @("]") closeBracket,
    @("~") tilde,
    @(":") colon,
    @(",") comma,
    @(".") dot,
    @(";") semicolon,

    @("<<") shiftLeft,
    @(">>") shiftRight,
    @("&&", "∧") andAnd,
    @("||", "∨") barBar,
    @("<=", "≤") lessEqual,
    @(">=", "≥") greaterEqual,
    @("==") equalEqual,
    @("!=", "≠") notEqual,
    @("+=") plusEqual,
    @("-=") minusEqual,
    @("*=") starEqual,
    @("/=") slashEqual,
    @("%=") percentEqual,
    @("^=") caretEqual,
    @("&=") andEqual,
    @("|=") barEqual,
    @("->", "→") arrow,

    @("<<=") shiftLeftEqual,
    @(">>=") shiftRightEqual,

    @("break") break_,
    @("continue") continue_,
    @("else") else_,
    @("for") for_,
    @("if") if_,
    @("return") return_,
    @("while") while_,
}

Sym sym(string repr)()
{
    static foreach (sym; EnumMembers!Sym)
    {
        static foreach (symRepr; getUDAs!(sym, string))
        {
            static if (repr == symRepr)
            {
                return sym;
            }
        }
    }
}

alias Bool = Tuple!(bool, "value");
alias Int = Tuple!(BigInt, "value");
alias Str = Tuple!(string, "value");
alias Char = Tuple!(dchar, "value");
alias Ident = Tuple!(string, "name");
alias Err = Tuple!(string, "message");
alias Eof = Tuple!();

alias TokType = SumType!(
    Sym,
    Bool,
    Int,
    Str,
    Char,
    Ident,
    Err,
    Eof,
);

struct Tok
{
    Span span; // also called a lexeme
    TokType type;

    bool contains(T)(auto ref T value)
    {
        import evorc.utils.sumtype : contains;
        return contains(this.type, value);
    }

    static import std.sumtype;

    template has(T)
    {
        alias has = partial!(std.sumtype.has!T, this.type);
    }

    template get(T)
    {
        alias get = partial!(std.sumtype.get!T, this.type);
    }
}

auto tokenize(S)(S src)
if (isSomeString!S || (isRandomAccessRange!S && hasSlicing!S && hasLength!S && isSomeChar!(ElementType!S)))
{
    return Lexer!S(src);
}

unittest
{
    static void nextIs(TokType, Range)(ref Range toks, Span span, TokType value)
    {
        import std.sumtype : has, get;
        static bool sameSlice(string a, string b) => a.ptr == b.ptr && a.length == b.length;
        assert(sameSlice(toks.front.span, span));
        assert(toks.front.type.has!TokType);
        assert(toks.front.type.get!TokType == value);
        toks.popFront;
    }

    {
        string s = "int س = 42;";
        auto toks = s.tokenize;
        nextIs(toks, s[0..3], Ident("int"));
        nextIs(toks, s[4..6], Ident("س"));
        nextIs(toks, s[7..8], sym!"=");
        nextIs(toks, s[9..11], Int(BigInt(42)));
        nextIs(toks, s[11..12], sym!";");
        nextIs(toks, s[12..12], Eof());
    }

    {
        string s = "1-2";
        auto toks = s.tokenize;
        nextIs(toks, s[0..1], Int(BigInt(1)));
        nextIs(toks, s[1..2], sym!"-");
        nextIs(toks, s[2..3], Int(BigInt(2)));
    }

    {
        string s = "1--2";
        auto toks = s.tokenize;
        nextIs(toks, s[0..1], Int(BigInt(1)));
        nextIs(toks, s[1..2], sym!"-");
        nextIs(toks, s[2..4], Int(BigInt(-2)));
    }
}

template isTokRange(R)
{
    enum isTokRange = isForwardRange!(R, Tok);
}

unittest
{
    auto tok(T)(T value) => Tok("", TokType(value));
    auto tokArray = [tok(Bool(true)), tok(Eof())];
    assert(isTokRange!(typeof(tokArray)));
    assert(isTokRange!(Lexer!string));
}

private struct Lexer(S) {
    private S src;
    private size_t idx;
    private Tok tok;

    this(string src)
    {
        this.src = src;
        popFront();
    }

    bool empty() const
    {
        return idx >= src.length + 2;
    }

    Tok front() const
    {
        return tok;
    }

    Lexer save() const
    {
        return this;
    }

    void popFront() 
    {
        import std.uni : isWhite;
        skipWhile!isWhite();
        if (idx >= src.length + 1)
        {
            idx++;
            return;
        }
        if (idx >= src.length)
        {
            tok = Tok(src[src.length..src.length], TokType(Eof()));
            idx++;
            return;
        };
        Sym symbol;
        size_t start = idx;
        dchar ch0 = decode!(Yes.useReplacementDchar)(src, idx);
        // Assumption: All symbolic tokens of multiple codepoints are ASCII
        char ch1 = idx < src.length ? src[idx] : '\0';
        char ch2 = idx + 1 < src.length ? src[idx + 1] : '\0';
        switch (ch0)
        {
        case '<':
            switch (ch1)
            {
            case '<':
                symbol = (ch2 == '=') ? Sym.shiftLeftEqual : Sym.shiftLeft;
                idx += (ch2 == '=') + 1;
                break;
            case '=':
                symbol = Sym.lessEqual;
                idx++;
                break;
            default:
                symbol = Sym.less;
            }
            goto ret;
        case '>':
            switch (ch1)
            {
            case '=':
                symbol = Sym.greaterEqual;
                idx++;
                break;
            case '>':
                symbol = (ch2 == '=') ? Sym.shiftRightEqual : Sym.shiftRight;
                idx += (ch2 == '=') + 1;
                break;
            default:
                symbol = Sym.greater;
            }
            goto ret;
        case '&':
            switch (ch1)
            {
            case '&':
                symbol = Sym.andAnd;
                idx++;
                break;
            case '=':
                symbol = Sym.andEqual;
                idx++;
                break;
            default:
                symbol = Sym.and;
            }
            goto ret;
        case '-':
            switch (ch1)
            {
            case '=':
                symbol = Sym.minusEqual;
                idx++;
                break;
            case '>':
                symbol = Sym.arrow;
                idx++;
                break;
            default:
                symbol = Sym.minus;
            }
            goto ret;
        case '/':
            switch (ch1) {
            case '*': // Block comment
                idx++;
                size_t commentStart = idx;
                while (idx + 1 < src.length) {
                    if (src[idx] == '*' && src[idx + 1] == '/') {
                        string comment = src[commentStart..idx];
                        idx += 2;
                        popFront;
                        return;
                    }
                    idx++;
                }
                // Unterminated block comment
                idx = src.length;
                popFront;
                return;
            case '/': // Line comment
                idx++;
                size_t commentStart = idx;
                while (idx < src.length && src[idx] != '\n') {
                    idx++;
                }
                popFront;
                return;
            case '=':
                symbol = Sym.slashEqual;
                idx++;
                break;
            default:
                symbol = Sym.slash;
                break;
            }
            goto ret;
        case '|':
            switch (ch1)
            {
            case '=':
                symbol = Sym.barEqual;
                idx++;
                break;
            case '|':
                symbol = Sym.barBar;
                idx++;
                break;
            default:
                symbol = Sym.bar;
            }
            goto ret;
        case '!':
            symbol = (ch1 == '=') ? Sym.notEqual : Sym.not;
            idx += (ch1 == '=');
            goto ret;
        case '%':
            symbol = (ch1 == '=') ? Sym.percentEqual : Sym.percent;
            idx += (ch1 == '=');
            goto ret;
        case '*':
            symbol = (ch1 == '=') ? Sym.starEqual : Sym.star;
            idx += (ch1 == '=');
            goto ret;
        case '+':
            symbol = (ch1 == '=') ? Sym.plusEqual : Sym.plus;
            idx += (ch1 == '=');
            goto ret;
        case '=':
            symbol = (ch1 == '=') ? Sym.equalEqual : Sym.equal;
            idx += (ch1 == '=');
            goto ret;
        case '^':
            symbol = (ch1 == '=') ? Sym.caretEqual : Sym.caret;
            idx += (ch1 == '=');
            goto ret;
        case '(':
            symbol = Sym.openParen;
            goto ret;
        case ')':
            symbol = Sym.closeParen;
            goto ret;
        case ',':
            symbol = Sym.comma;
            goto ret;
        case ':':
            symbol = Sym.colon;
            goto ret;
        case '.':
            symbol = Sym.dot;
            goto ret;
        case ';':
            symbol = Sym.semicolon;
            goto ret;
        case '[':
            symbol = Sym.openBracket;
            goto ret;
        case ']':
            symbol = Sym.closeBracket;
            goto ret;
        case '{':
            symbol = Sym.openBrace;
            goto ret;
        case '}':
            symbol = Sym.closeBrace;
            goto ret;
        case '~':
            symbol = Sym.tilde;
            goto ret;
        case '→':
            symbol = Sym.arrow;
            goto ret;
        case '∧':
            symbol = Sym.andAnd;
            goto ret;
        case '∨':
            symbol = Sym.barBar;
            goto ret;
        case '≤':
            symbol = Sym.lessEqual;
            goto ret;
        case '≥':
            symbol = Sym.greaterEqual;
            goto ret;
        case '≠':
            symbol = Sym.notEqual;
            goto ret;
        default:
            break;
        ret:
            tok = Tok(src[start..idx], TokType(symbol));
            return;
        }
        if (isDigit(ch0))
        {
        intTok:
            uint radix = 10;
            if (ch0 == '0')
            {
                switch (ch1)
                {
                case 'b': case 'B':
                    if (ch2 == '0' || ch2 == '1') {
                        idx += 2;
                        radix = 2;
                    }
                    break;
                case 'x': case 'X':
                    if (isHexDigit(ch2)) {
                        idx += 2;
                        radix = 16;
                    }
                    break;
                default:
                    if (isOctalDigit(ch2)) {
                        idx += 1;
                        radix = 8;
                    }
                    break;
                }
            }
            // Shamelessly stolen from std.conv.parse
            immutable uint beyond = (radix < 10 ? '0' : 'a'-10) + radix;
            BigInt v;
            idx -= 1;
            do
            {
                uint c = src[idx];
                if (c == '_')
                {
                    ++idx;
                    continue;
                }
                if (c < '0')
                    break;
                if (radix < 10)
                {
                    if (c >= beyond)
                        break;
                }
                else
                {
                    if (c > '9')
                    {
                        c |= 0x20;//poorman's tolower
                        if (c < 'a' || c >= beyond)
                            break;
                        c -= 'a'-10-'0';
                    }
                }
                v *= radix;
                v += c - '0';
                ++idx;
            } while (idx < src.length);
            tok = Tok(src[start..idx], TokType(Int(v)));
            return;
        }
        if (isIdentStart(ch0))
        {
            skipWhile!isIdentContinue();
            string ident = src[start..idx];
            switch (ident)
            {
            case "break": tok = Tok(ident, TokType(Sym.break_)); break;
            case "continue": tok = Tok(ident, TokType(Sym.continue_)); break;
            case "else": tok = Tok(ident, TokType(Sym.else_)); break;
            case "for": tok = Tok(ident, TokType(Sym.for_)); break;
            case "if": tok = Tok(ident, TokType(Sym.if_)); break;
            case "return": tok = Tok(ident, TokType(Sym.return_)); break;
            case "while": tok = Tok(ident, TokType(Sym.while_)); break;
            case "true": tok = Tok(ident, TokType(Bool(true))); break;
            case "false": tok = Tok(ident, TokType(Bool(false))); break;
            default: tok = Tok(ident, TokType(Ident(ident))); break;
            }
            return;
        }
        if (ch0 == '"')
        {
            string s;
            while (idx < src.length)
            {
                size_t escapeStart = idx;
                dchar ch = decode(src, idx);
                switch (ch)
                {
                case '"':
                    tok = Tok(src[start..idx], TokType(Str(s)));
                    return;
                case '\\':
                    if (idx >= src.length) goto errTermStr;
                    ch = decode(src, idx);
                    ch = escapeCharacter(ch);
                    if (ch == dchar.max) return;
                    goto default;
                default:
                    s ~= ch;
                }
            }
        errTermStr:
            tok = Tok(src[start..idx], TokType(Err("unterminated string literal")));
            return;
        }
        if (ch0 == '\'')
        {
            dchar ch = decode(src, idx);
            switch (ch)
            {
            case '\'':
                tok = Tok(src[start..idx], TokType(Err("empty character literal")));
                return;
            case '\\':
                if (idx >= src.length) goto errTermChar;
                ch = decode(src, idx);
                ch = escapeCharacter(ch);
                if (ch == dchar.max) return;
                break;
            default:
                break;
            }
            if (idx < src.length && src[idx] == '\'')
            {
                tok = Tok(src[start..++idx], TokType(Char(ch)));
                return;
            }
        errTermChar:
            tok = Tok(src[start..idx], TokType(Err("unterminated character literal")));
            return;
        }
        tok = Tok(src[start..idx], TokType(Err("unknown token")));
    }

    private dchar escapeCharacter(dchar ch)
    {
        size_t start = idx - 2;
        switch (ch)
        {
        case '\'': return '\'';
        case '"':  return '"';
        case 'n':  return '\n';
        case 'r':  return '\r';
        case 't':  return '\t';
        case '\\': return '\\';
        case '0':  return '\0';
        case 'u':
            if (idx >= src.length || src[idx++] != '{')
            {
                tok = Tok(src[start..idx], TokType(Err("unicode escape must start with '{'")));
                return dchar.max;
            }
            if (idx >= src.length)
            {
            errTermUni:
                tok = Tok(src[start..idx], TokType(Err("unterminated unicode escape")));
                return dchar.max;
            }
            if (src[idx] == '}')
            {
                tok = Tok(src[start..++idx], TokType(Err("empty unicode escape sequence")));
                return dchar.max;
            }
            uint val;
            do
            {
                uint c = src[idx];
                if (c == '_')
                {
                    ++idx;
                    continue;
                }
                if (c < '0')
                    break;
                if (c > '9')
                {
                    c |= 0x20;//poorman's tolower
                    if (c < 'a' || c > 'f')
                        break;
                    c -= 'a'-10-'0';
                }
                val *= 16;
                val += c - '0';
                ++idx;
                if (val > 0x10FFFF)
                {
                    while (idx < src.length)
                    {
                        c = src[idx];
                        if (!(isHexDigit(c) || c == '_' || c == '}')) break;
                        idx++;
                    }
                    tok = Tok(src[start..idx], TokType(Err("unicode escape must be at most 10FFFF")));
                    return dchar.max;
                }
            } while (idx < src.length);
            if (idx >= src.length) goto errTermUni;
            uint c = src[idx++];
            if (!(isHexDigit(c) || c == '_' || c == '}'))
            {
                tok = Tok(src[idx-1..idx], TokType(Err("invalid character in unicode escape")));
                return dchar.max;
            }
            auto cha = cast(dchar)val;
            if (!isValidDchar(cha))
            {
                tok = Tok(src[start..idx], TokType(Err("invalid unicode character")));
                return dchar.max;
            }
            return cha;
        default:
            tok = Tok(src[start..idx], TokType(Err("unknown character escape")));
            return dchar.max;
        }
    }

    private void skipWhile(alias pred)()
    {
        size_t prevIdx = idx;
        while (prevIdx < src.length && pred(decode!(Yes.useReplacementDchar)(src, prevIdx))) {
            idx = prevIdx;
        }
    }

    private static bool isIdentStart(dchar ch)
    {
        // fast path
        if (ch <= 127) return asciiIdentStartSet[ch];
        return xidStartSet[ch];
    }

    private static bool isIdentContinue(dchar ch)
    {
        // fast path
        if (ch <= 127) return asciiIdentContSet[ch];
        return xidContSet[ch];
    }
}

private static immutable xidStartSet = toTrie!1(unicode.XID_Start);
private static immutable xidContSet = toTrie!1(unicode.XID_Continue);
private static immutable asciiIdentStartSet = genAsciiIdentSet(xidStartSet);
private static immutable asciiIdentContSet = genAsciiIdentSet(xidContSet);

private auto genAsciiIdentSet(T)(auto ref T xidSet)
{
    bool[128] lut;
    foreach (ch; 0..128)
        lut[ch] = xidSet[ch];
    lut['_'] = true;
    return lut;
}
