/**
 * Defines lexical tokens of the Evor language and handles the first
 * step of the compilation process: tokenization.
 */

module evorc.tok;

import evorc.span;
import std;

enum Sym
{
    /// "<"
    less,
    /// ">"
    greater,
    /// "="
    equal,
    /// "!"
    not,
    /// "+"
    plus,
    /// "-"
    minus,
    /// "*"
    star,
    /// "/"
    slash,
    /// "%"
    percent,
    /// "^"
    caret,
    /// "&"
    and,
    /// "|"
    bar,
    /// "("
    openParen,
    /// ")"
    closeParen,
    /// "{"
    openBrace,
    /// "}"
    closeBrace,
    /// "["
    openBracket,
    /// "]"
    closeBracket,
    /// "~"
    tilde,
    /// ":"
    colon,
    /// ","
    comma,
    /// "."
    dot,
    /// ";"
    semicolon,

    /// "<<"
    shiftLeft,
    /// ">>"
    shiftRight,
    /// "&&", "∧"
    andAnd,
    /// "||", "∨"
    barBar,
    /// "<=", "≤"
    lessEqual,
    /// ">=", "≥"
    greaterEqual,
    /// "=="
    equalEqual,
    /// "!=", "≠"
    notEqual,
    /// "+="
    plusEqual,
    /// "-="
    minusEqual,
    /// "*="
    starEqual,
    /// "/="
    slashEqual,
    /// "%="
    percentEqual,
    /// "^="
    caretEqual,
    /// "&="
    andEqual,
    /// "|="
    barEqual,
    /// "->", "→"
    arrow,

    /// "<<="
    shiftLeftEqual,
    /// ">>="
    shiftRightEqual,

    /// break
    break_,
    /// continue
    continue_,
    /// else
    else_,
    /// for
    for_,
    /// if
    if_,
    /// return
    return_,
    /// while
    while_,
}

Sym sym(string symbol)()
{
    with (Sym)
    {
    static if (symbol == "<") return less;
    else static if (symbol == ">") return greater;
    else static if (symbol == "=") return equal;
    else static if (symbol == "!") return not;
    else static if (symbol == "+") return plus;
    else static if (symbol == "-") return minus;
    else static if (symbol == "*") return star;
    else static if (symbol == "/") return slash;
    else static if (symbol == "%") return percent;
    else static if (symbol == "^") return caret;
    else static if (symbol == "&") return and;
    else static if (symbol == "|") return bar;
    else static if (symbol == "(") return openParen;
    else static if (symbol == ")") return closeParen;
    else static if (symbol == "{") return openBrace;
    else static if (symbol == "}") return closeBrace;
    else static if (symbol == "[") return openBracket;
    else static if (symbol == "]") return closeBracket;
    else static if (symbol == "~") return tilde;
    else static if (symbol == ":") return colon;
    else static if (symbol == ",") return comma;
    else static if (symbol == ".") return dot;
    else static if (symbol == ";") return semicolon;
    else static if (symbol == "<<") return shiftLeft;
    else static if (symbol == ">>") return shiftRight;
    else static if (symbol == "&&") return andAnd;
    else static if (symbol == "∧") return andAnd;
    else static if (symbol == "||") return barBar;
    else static if (symbol == "∨") return barBar;
    else static if (symbol == "<=") return lessEqual;
    else static if (symbol == "≤") return lessEqual;
    else static if (symbol == ">=") return greaterEqual;
    else static if (symbol == "≥") return greaterEqual;
    else static if (symbol == "==") return equalEqual;
    else static if (symbol == "!=") return notEqual;
    else static if (symbol == "+=") return plusEqual;
    else static if (symbol == "-=") return minusEqual;
    else static if (symbol == "*=") return starEqual;
    else static if (symbol == "/=") return slashEqual;
    else static if (symbol == "%=") return percentEqual;
    else static if (symbol == "^=") return caretEqual;
    else static if (symbol == "&=") return andEqual;
    else static if (symbol == "|=") return barEqual;
    else static if (symbol == "->") return arrow;
    else static if (symbol == "→") return arrow;
    else static if (symbol == "<<=") return shiftLeftEqual;
    else static if (symbol == ">>=") return shiftRightEqual;
    else static if (symbol == "break") return break_; 
    else static if (symbol == "continue") return continue_; 
    else static if (symbol == "else") return else_; 
    else static if (symbol == "for") return for_; 
    else static if (symbol == "if") return if_; 
    else static if (symbol == "return") return return_; 
    else static if (symbol == "while") return while_; 
    }
}

alias Bool = Tuple!(bool, "value");
alias Int = Tuple!(BigInt, "value");
alias Ident = Tuple!(string, "name");
alias Unknown = Tuple!(dchar, "tok");
alias Eof = Tuple!();

alias TokType = SumType!(
    Sym,
    Bool,
    Int,
    Ident,
    Unknown,
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

    template has(T)
    {
        bool has()
        {
            import std.sumtype : has;
            return has!T(this.type);
        }
    }

    template get(T)
    {
        auto ref T get()
        {
            import std.sumtype : get;
            return get!T(this.type);
        }
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
    string s = "int x = 42;";
    auto toks = s.tokenize;
    nextIs!Ident(toks, s[0..3], Ident("int"));
    nextIs!Ident(toks, s[4..5], Ident("x"));
    nextIs!Sym(toks, s[6..7], sym!"=");
    nextIs!Int(toks, s[8..10], Int(BigInt(42)));
    nextIs!Sym(toks, s[10..11], sym!";");
    nextIs!Eof(toks, s[11..11], Eof());
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
        tok = Tok(src[start..idx], TokType(Unknown(ch0)));
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

auto genAsciiIdentSet(T)(auto ref T xidSet)
{
    bool[128] lut;
    foreach (ch; 0..128)
        lut[ch] = xidSet[ch];
    lut['_'] = true;
    return lut;
}
