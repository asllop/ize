//! # IZE Lexer
//!
//! The lexer reads raw source code and converts it into a vector of [Token](crate::lexer::Token)s.

use crate::{common::BuildErr, IzeErr, Pos};
use alloc::{collections::VecDeque, string::String, vec::Vec};
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

#[derive(Debug)]
/// Stream of tokens returned by the lexer.
pub struct TokenStream {
    tokens: VecDeque<Token>,
}

impl TokenStream {
    /// Check if parser ended processing tokens.
    pub fn ended(&self) -> bool {
        self.tokens.is_empty()
    }

    /// Return position of next token in the stream.
    pub fn last_pos(&self) -> Pos {
        if !self.ended() {
            self.tokens[0].pos
        } else {
            Default::default()
        }
    }

    /// Consume token.
    pub fn consume_token(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    /// Check if token at offset is of given type.
    pub fn is_token(&self, token_kind: TokenKind, offset: usize) -> bool {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            match token.lexeme {
                Lexeme::Float(_) => token_kind == TokenKind::FloatLiteral,
                Lexeme::Int(_) => token_kind == TokenKind::IntegerLiteral,
                Lexeme::Bool(_) => token_kind == TokenKind::BooleanLiteral,
                Lexeme::String(_) => token_kind == TokenKind::StringLiteral,
                Lexeme::Ident(_) => token_kind == TokenKind::Ident,
                Lexeme::Particle(tt) => token_kind == tt,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Check if token is a literal.
    pub fn is_literal(&self, offset: usize) -> bool {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            match token.lexeme {
                Lexeme::Float(_)
                | Lexeme::Int(_)
                | Lexeme::Bool(_)
                | Lexeme::String(_)
                | Lexeme::Particle(TokenKind::NullLiteral)
                | Lexeme::Particle(TokenKind::NoneLiteral) => true,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Check if token is an identifier.
    pub fn is_ident(&self, offset: usize) -> bool {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            if let Lexeme::Ident(_) = token.lexeme {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Check for a list of tokens.
    pub fn check_tokens(&self, token_types: &[TokenKind], offset: usize) -> bool {
        // Check if token exist at the specified offset
        for t in token_types {
            if self.is_token(*t, offset) {
                return true;
            }
        }
        false
    }

    //TODO: extract, check empty, check token type, etc
}

impl From<Vec<Token>> for TokenStream {
    fn from(value: Vec<Token>) -> Self {
        Self {
            tokens: VecDeque::from(value),
        }
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
            last_pos: Pos::default(),
        }
    }

    /// Position of last read token.
    pub fn pos(&self) -> Pos {
        self.last_pos
    }

    /// Scan all tokens
    pub fn tokenize(&mut self) -> Result<TokenStream, IzeErr> {
        Ok(self.tokenize_vec()?.into())
    }

    //TODO: remove this and keep only "tokenize"
    /// Scan all tokens
    pub fn tokenize_vec(&mut self) -> Result<Vec<Token>, IzeErr> {
        let mut tokens = Vec::new();
        loop {
            let token = self.scan_token()?;
            match token.lexeme {
                Lexeme::EOF => break,
                Lexeme::Nothing => {}
                _ => tokens.push(token),
            }
        }
        Ok(tokens)
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
                        Err(err) => Result::ize_err(format!("{:?}", err), next_pos),
                    },
                    TokenKind::FloatLiteral => match str::parse(fragment) {
                        Ok(n) => {
                            let token = Token::new(Lexeme::Float(n), next_pos);
                            next_pos.col += lex_offs.end - lex_offs.start;
                            self.last_pos = next_pos;
                            Ok(token)
                        }
                        Err(err) => Result::ize_err(format!("{:?}", err), next_pos),
                    },
                    TokenKind::StringLiteral => {
                        let literal_str = fragment[1..fragment.len() - 1].into();
                        let token = Token::new(Lexeme::String(literal_str), next_pos);
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
                return Result::ize_err(format!("Unrecognized lexeme: '{}'", fragment), next_pos);
            }
        } else {
            // EOF
            let token = Token::new(Lexeme::EOF, self.last_pos);
            Ok(token)
        }
    }
}
