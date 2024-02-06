//! # Lexer
//!
//! Lexical analyzer. Generate [Token](crate::lexer::Token)s from the raw source code.

use crate::{
    pos::RangePos,
    err::IzeErr
};
use alloc::{string::String, vec::Vec};
use core::{fmt::Debug, str::FromStr};
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
/// Extra data for the lexer.
pub struct LexExtras {
    line: usize,
    pos_last_eol: usize,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexExtras, skip r"[ \t]+", skip r"//.*")]
/// List of recognized tokens.
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
    #[regex("[0-9]+", parse_callback::<i64>)]
    IntegerLiteral(i64),
    #[regex(r#"[0-9]+\.[0-9]+"#, parse_callback::<f64>)]
    FloatLiteral(f64),
    #[regex("(true|false)", parse_callback::<bool>)]
    BooleanLiteral(bool),
    #[token("none")]
    NoneLiteral,
    #[token("null")]
    NullLiteral,
    //TODO: check valid formatting for escape sequences: \n \t \r \\ \" \0 \xXX (2 digits, up to 0x7F) \u{XXXX} (up to 6 digits)
    #[regex(r#""([^"\\]|\\"|\\)*""#, parse_callback::<String>)]
    StringLiteral(String),
    #[regex(r#"[\p{Alphabetic}_]([\p{Alphabetic}_0-9]+)?"#, parse_callback::<String>)]
    Identifier(String),

    // Types
    #[token("List")]
    List,
    #[token("Map")]
    Map,
    #[token("Mux")]
    Mux,
    #[token("Tuple")]
    Tuple,
    #[token("Traf")]
    Traf,

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
    #[token("const")]
    Const,

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

#[derive(Debug, PartialEq)]
/// Token type.
pub struct Token {
    /// Token kind.
    pub kind: TokenKind,
    /// Token position.
    pub pos: RangePos,
}

impl Token {
    /// New token from position and kind.
    pub fn new(pos: RangePos, kind: TokenKind) -> Self {
        Self { kind, pos }
    }
}

/// Tokenize. Convert string containing source code into a vector of [Token](crate::lexer::Token)s.
pub fn tokenize(input: &str) -> Result<Vec<Token>, IzeErr> {
    let mut lex = TokenKind::lexer(input);
    let mut tokens = vec![];
    while let Some(r) = lex.next() {
        let line = lex.extras.line;
        let start_col = lex.span().start - lex.extras.pos_last_eol;
        let end_col = lex.span().end - lex.extras.pos_last_eol;
        let pos = RangePos::inline_new(line, start_col, end_col, lex.span().start);
        if let Ok(token_kind) = r {
            tokens.push(Token::new(pos, token_kind));
        } else {
            return Err(IzeErr::new("Bad token".into(), pos));
        }
    }
    Ok(tokens)
}
