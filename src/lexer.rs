//! # IZE Lexer
//!
//! The lexer reads raw source code and converts it into a vector of [Token](crate::lexer::Token)s.

use crate::{IzeErr, Pos};
use alloc::string::String;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t]+")]
/// List of recognized tokens, requiered by [logos].
pub enum TokenKind {
    #[regex("//.*")]
    Comment,

    #[token("\n")]
    EOL,

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
    #[token("_")]
    Underscore,
    #[token("as")]
    As,
    #[token("->")]
    Arrow,
    #[token("let")]
    Let,

    // Literals
    #[regex("-?[0-9]+")]
    IntegerLiteral,
    //TODO: scientific notation: -9.09E-3, 9.09E+3
    #[regex(r#"-?[0-9]+\.[0-9]+"#)]
    FloatLiteral,
    #[token("true")]
    #[token("false")]
    BooleanLiteral,
    #[token("none")]
    NoneLiteral,
    #[token("null")]
    NullLiteral,
    #[regex(r#""([^"\\]|\\"|\\)*""#)]
    StringLiteral,

    // Types
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
    #[token("if?")]
    If,
    #[token("else?")]
    Else,
    #[token("select")]
    Select,
    #[token("unwrap")]
    Unwrap,

    #[regex(r#"[\p{Alphabetic}_]([\p{Alphabetic}_0-9]+)?"#)]
    Ident,
}

#[derive(Debug, PartialEq)]
/// A lexical element, the actual token. [Token] is just a wrapper that contains this and a position.
pub enum Lexeme {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ident(String),
    Particle(TokenKind),
    EOF,
    Nothing,
}

#[derive(Debug)]
/// A token is a lexical element and its position in the source code.
pub struct Token {
    pub lexeme: Lexeme,
    //TODO: rename to "start"
    pub pos: Pos,
    //TODO: add end position
}

impl Token {
    /// Create a new token.
    pub fn new(lexeme: Lexeme, pos: Pos) -> Self {
        Self { lexeme, pos }
    }
}

/// Lexer.
pub struct Lexer<'a> {
    current_code: &'a str,
    last_pos: Pos,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer with source code.
    pub fn new(code: &'a str) -> Self {
        Self {
            current_code: code,
            last_pos: Pos::new(0, 0),
        }
    }

    /// Position of last read token.
    pub fn pos(&self) -> Pos {
        self.last_pos
    }

    /// Scan next token from the source code.
    pub fn scan_token(&mut self) -> Result<Token, IzeErr> {
        if let Some((lexeme, lex_offs)) = TokenKind::lexer(self.current_code).spanned().next() {
            let fragment = &self.current_code[lex_offs.start..lex_offs.end];
            self.current_code = &self.current_code[lex_offs.end..];

            let col = lex_offs.start + self.last_pos.col;
            let mut next_pos = Pos::new(self.last_pos.row, col);

            if let Ok(lexeme) = lexeme {
                match lexeme {
                    TokenKind::Comment => {
                        let token = Token::new(Lexeme::Nothing, next_pos);
                        next_pos.col += lex_offs.end - lex_offs.start;
                        self.last_pos = next_pos;
                        Ok(token)
                    }
                    TokenKind::EOL => {
                        let token = Token::new(Lexeme::Nothing, next_pos);
                        next_pos.row += 1;
                        next_pos.col = 0;
                        self.last_pos = next_pos;
                        Ok(token)
                    }
                    TokenKind::IntegerLiteral => match str::parse(fragment) {
                        Ok(n) => {
                            let token = Token::new(Lexeme::Int(n), next_pos);
                            next_pos.col += lex_offs.end - lex_offs.start;
                            self.last_pos = next_pos;
                            Ok(token)
                        }
                        Err(err) => Err(IzeErr {
                            message: format!("{:?}", err),
                            pos: next_pos,
                        }),
                    },
                    TokenKind::FloatLiteral => match str::parse(fragment) {
                        Ok(n) => {
                            let token = Token::new(Lexeme::Float(n), next_pos);
                            next_pos.col += lex_offs.end - lex_offs.start;
                            self.last_pos = next_pos;
                            Ok(token)
                        }
                        Err(err) => Err(IzeErr {
                            message: format!("{:?}", err),
                            pos: next_pos,
                        }),
                    },
                    TokenKind::StringLiteral => {
                        let token = Token::new(Lexeme::String(fragment.into()), next_pos);
                        next_pos.col += lex_offs.end - lex_offs.start;
                        self.last_pos = next_pos;
                        Ok(token)
                    }
                    TokenKind::BooleanLiteral => {
                        let token = Token::new(Lexeme::Bool(fragment == "true"), next_pos);
                        next_pos.col += lex_offs.end - lex_offs.start;
                        self.last_pos = next_pos;
                        Ok(token)
                    }
                    TokenKind::Ident => {
                        let token = Token::new(Lexeme::Ident(fragment.into()), next_pos);
                        next_pos.col += lex_offs.end - lex_offs.start;
                        self.last_pos = next_pos;
                        Ok(token)
                    }
                    _ => {
                        let token = Token::new(Lexeme::Particle(lexeme), next_pos);
                        next_pos.col += lex_offs.end - lex_offs.start;
                        self.last_pos = next_pos;
                        Ok(token)
                    }
                }
            } else {
                return Err(IzeErr {
                    message: format!("Unrecognized lexeme: '{}'", fragment),
                    pos: next_pos,
                });
            }
        } else {
            // EOF
            let token = Token::new(Lexeme::EOF, self.last_pos);
            Ok(token)
        }
    }
}
