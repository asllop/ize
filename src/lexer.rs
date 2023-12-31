use crate::common::TokenPos;
use crate::err::IzeErr;
use alloc::{string::String, vec::Vec};
use core::fmt::Debug;
use core::str::FromStr;
use logos::{Lexer, Logos, Skip};

fn parse_callback<T>(lex: &mut Lexer<TokenKind>) -> T
where
    T: FromStr + Debug,
{
    lex.slice().parse().ok().unwrap()
}

fn newline_callback(lex: &mut Lexer<TokenKind>) -> Skip {
    lex.extras.line += 1;
    lex.extras.pos_last_eol = lex.span().end;
    Skip
}

#[derive(Default, Debug, Clone, Copy)]
pub struct LexExtras {
    line: usize,
    pos_last_eol: usize,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexExtras, skip r"[ \t]+", skip r"//.*")]
/// List of recognized tokens, requiered by [logos].
pub enum TokenKind {
    #[regex(r"\n", newline_callback)]
    Newline,

    #[token("(")]
    OpenParenth,
    #[token(")")]
    ClosingParenth,
    #[token("[")]
    OpenClause,
    #[token("]")]
    ClosingClause,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    LesserThan,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GtEqual,
    #[token("<=")]
    LtEqual,
    #[token("&")]
    And,
    #[token("&&")]
    TwoAnds,
    #[token("|")]
    Or,
    #[token("||")]
    TwoOrs,
    #[token("!")]
    Not,
    #[token("==")]
    TwoEquals,
    #[token("!=")]
    NotEqual,
    #[token(".")]
    Dot,
    #[token("...")]
    ThreeDots,
    #[token("as")]
    As,
    #[token("->")]
    Arrow,
    #[token("let")]
    Let,

    // Primaries
    #[regex("-?[0-9]+", parse_callback::<i64>)]
    IntegerLiteral(i64),
    //TODO: scientific notation: -9.09E-3, 9.09E+3
    #[regex(r#"-?[0-9]+\.[0-9]+"#, parse_callback::<f64>)]
    FloatLiteral(f64),
    #[regex("true", parse_callback::<bool>)]
    #[regex("false", parse_callback::<bool>)]
    BooleanLiteral(bool),
    #[token("none")]
    NoneLiteral,
    #[token("null")]
    NullLiteral,
    #[regex(r#""([^"\\]|\\"|\\)*""#, parse_callback::<String>)]
    StringLiteral(String),
    #[regex(r#"[\p{Alphabetic}_]([\p{Alphabetic}_0-9]+)?"#, parse_callback::<String>)]
    Identifier(String),

    // Types
    // TODO: do we really need this? We could just have Ident and check for types during the semantic analysis.
    #[token("Integer")]
    IntegerType,
    #[token("Float")]
    FloatType,
    #[token("Boolean")]
    BooleanType,
    #[token("String")]
    StringType,
    #[token("Null")]
    NullType,
    #[token("None")]
    NoneType,
    #[token("Map")]
    MapType,
    #[token("List")]
    ListType,
    #[token("Mux")]
    MuxType,
    #[token("Tuple")]
    TupleType,

    //TODO: can we scan complex types like Map[String, Integer] or List[Map[String, Integer]] ?

    // Commands
    #[token("model")]
    Model,
    #[token("transfer")]
    Transfer,
    #[token("pipe")]
    Pipe,
    #[token("run")]
    Run,
    #[token("import")]
    Import,

    // Decision
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("select")]
    Select,
    #[token("unwrap")]
    Unwrap,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: TokenPos,
}

impl Token {
    pub fn new(pos: TokenPos, kind: TokenKind) -> Self {
        Self { kind, pos }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, IzeErr> {
    let mut lex = TokenKind::lexer(input);
    let mut tokens = vec![];
    while let Some(r) = lex.next() {
        let line = lex.extras.line;
        let start_col = lex.span().start - lex.extras.pos_last_eol;
        let end_col = lex.span().end - lex.extras.pos_last_eol;
        let pos = TokenPos::new(line, start_col, end_col, lex.span().start);
        if let Ok(token_kind) = r {
            tokens.push(Token::new(pos, token_kind));
        } else {
            return Err(IzeErr::new("Bad token".into(), pos));
        }
    }
    Ok(tokens)
}
