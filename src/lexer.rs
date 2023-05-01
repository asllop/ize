//! # IZE Lexer
//!
//! This module contains all the types and methods necessary to convert raw source code into tokens.
//!
//! The main public interface from this module is [Line](crate::lexer::Line):
//!
//! ```
//! use ize::lexer::Line;
//!
//! let line = match Line::scan_tokens(r#"const ME = "IZE Language""#, 0) {
//!     Ok(line) => line,
//!     Err(err) => panic!("Error: \"{}\" at offset {}", err.message, err.position),
//! };
//! ```

use alloc::{string::String, vec::Vec};
use core::fmt::Display;
use logos::Logos;
use regex::Regex;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t]+")]
/// Token types.
pub enum TokenType {
    // Flow and multipurpose
    #[token("match")]
    Match,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("as")]
    As,
    #[token("return")]
    Return,

    // Single, double and triple chars
    #[token("_")]
    Underscore,
    #[token("(")]
    OpenParenth,
    #[token(")")]
    ClosingParenth,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("-")]
    Minus,
    #[token("->")]
    Arrow,
    #[token("<")]
    OpenAngleBrack,
    #[token(">")]
    ClosingAngleBrack,
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
    Exclam,
    #[token("!=")]
    NotEqual,
    #[token("=")]
    Equal,
    #[token("==")]
    TwoEquals,
    #[token(".")]
    Dot,
    #[token("..")]
    TwoDots,
    #[token("...")]
    ThreeDots,

    // Literals
    #[regex("-?[0-9]+")]
    IntegerLiteral,
    //TODO: scientific notation: -9.09E-3, 9.09E+3
    #[regex(r#"-?[0-9]+\.[0-9]+"#)]
    FloatLiteral,
    #[regex(r#""([^"\\]|\\"|\\)*""#)]
    StringLiteral,
    #[regex(r#"r"([^"\\]|\\"|\\)*""#)]
    RegexLiteral,
    #[token("true")]
    #[token("false")]
    BooleanLiteral,

    // Types
    #[token("String")]
    StringType,
    #[token("Integer")]
    IntegerType,
    #[token("Float")]
    FloatType,
    #[token("Boolean")]
    BooleanType,
    #[token("Map")]
    MapType,
    #[token("Pair")]
    PairType,
    #[token("List")]
    ListType,
    #[token("Any")]
    AnyType,
    #[token("None")]
    NoneType,
    #[token("Null")]
    NullType,

    // Definition
    #[token("struct")]
    StructDefinition,
    #[token("map")]
    MapDefinition,
    #[token("list")]
    ListDefinition,
    #[token("string")]
    StringDefinition,
    #[token("boolean")]
    BooleanDefinition,
    #[token("integer")]
    IntegerDefinition,
    #[token("float")]
    FloatDefinition,
    #[token("dyn")]
    DynDefinition,
    #[token("transfer")]
    TransferDefinition,
    #[token("buffer")]
    BufferDefinition,
    #[token("pipeline")]
    PipelineDefinition,
    #[token("const")]
    ConstDefinition,

    // Comment
    #[token("//")]
    Comment,

    // Multichar
    #[regex(r#"#[\p{Alphabetic}_]([\p{Alphabetic}_0-9]+)?"#)]
    Macro,
    #[regex(r#"[\p{Alphabetic}_]([\p{Alphabetic}_0-9]+)?"#)]
    Identifier,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            TokenType::Match => write!(f, "match"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::As => write!(f, "as"),
            TokenType::Return => write!(f, "return"),
            TokenType::Underscore => write!(f, "_"),
            TokenType::OpenParenth => write!(f, "("),
            TokenType::ClosingParenth => write!(f, ")"),
            TokenType::Comma => write!(f, ","),
            TokenType::Colon => write!(f, ":"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Percent => write!(f, "%"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Arrow => write!(f, "->"),
            TokenType::OpenAngleBrack => write!(f, "<"),
            TokenType::ClosingAngleBrack => write!(f, ">"),
            TokenType::GtEqual => write!(f, ">="),
            TokenType::LtEqual => write!(f, "<="),
            TokenType::And => write!(f, "&"),
            TokenType::TwoAnds => write!(f, "&&"),
            TokenType::Or => write!(f, "|"),
            TokenType::TwoOrs => write!(f, "||"),
            TokenType::Exclam => write!(f, "!"),
            TokenType::NotEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::TwoEquals => write!(f, "=="),
            TokenType::Dot => write!(f, "."),
            TokenType::TwoDots => write!(f, ".."),
            TokenType::ThreeDots => write!(f, "..."),
            TokenType::IntegerLiteral => write!(f, "INT\\"),
            TokenType::FloatLiteral => write!(f, "FLT\\"),
            TokenType::StringLiteral => write!(f, "STR\\"),
            TokenType::RegexLiteral => write!(f, "REX\\"),
            TokenType::BooleanLiteral => write!(f, "BOL\\"),
            TokenType::StringType => write!(f, "StringType"),
            TokenType::IntegerType => write!(f, "IntegerType"),
            TokenType::FloatType => write!(f, "FloatType"),
            TokenType::BooleanType => write!(f, "BooleanType"),
            TokenType::MapType => write!(f, "MapType"),
            TokenType::PairType => write!(f, "PairType"),
            TokenType::ListType => write!(f, "ListType"),
            TokenType::AnyType => write!(f, "AnyType"),
            TokenType::NoneType => write!(f, "NoneType"),
            TokenType::NullType => write!(f, "NullType"),
            TokenType::StructDefinition => write!(f, "struct"),
            TokenType::MapDefinition => write!(f, "map"),
            TokenType::ListDefinition => write!(f, "list"),
            TokenType::StringDefinition => write!(f, "string"),
            TokenType::BooleanDefinition => write!(f, "boolean"),
            TokenType::IntegerDefinition => write!(f, "integer"),
            TokenType::FloatDefinition => write!(f, "float"),
            TokenType::DynDefinition => write!(f, "dyn"),
            TokenType::TransferDefinition => write!(f, "transfer"),
            TokenType::BufferDefinition => write!(f, "buffer"),
            TokenType::PipelineDefinition => write!(f, "pipeline"),
            TokenType::ConstDefinition => write!(f, "const"),
            TokenType::Comment => write!(f, "//"),
            TokenType::Macro => write!(f, "#\\"),
            TokenType::Identifier => write!(f, "ID\\"),
        }
    }
}

#[derive(Debug)]
/// Lexical analysis error.
pub struct LexError {
    /// Error message.
    pub message: String,
    /// Position in the line where the error was found.
    pub position: usize,
}

#[derive(Debug)]
/// Tokenized line of code.
pub struct Line {
    /// List of tokens in this line.
    pub tokens: Vec<Token>,
    /// Line position within the source code.
    pub position: Position,
}

impl Line {
    /// Create new empty Line.
    pub fn new(code: &str, line_num: usize) -> Self {
        Line {
            tokens: Vec::new(),
            position: Position {
                indentation: 0,
                indentation_type: IndentationType::None,
                line_num,
                length: code.len(),
            },
        }
    }

    /// Scan tokens in a line of code and generate either a Line or an error.
    pub fn scan_tokens(code: &str, line_num: usize) -> Result<Self, LexError> {
        let mut line = Line::new(code, line_num);

        // Extract line indentation
        match Self::find_indentation(code) {
            Ok(Some((i, t))) => {
                line.position.indentation = i;
                line.position.indentation_type = t;
            }
            Ok(None) => {
                // Empty line
                return Ok(line);
            }
            Err(err) => {
                return Err(err);
            }
        }

        let line_offset = line.position.indentation;
        let code = &code[line_offset..];

        // Start regular analysis part
        for (lexeme, pos) in TokenType::lexer(code).spanned() {
            let fragment = &code[pos.start..pos.end];
            let lex_pos = pos.start + line_offset;

            if let Ok(lexeme) = lexeme {
                match lexeme {
                    TokenType::Comment => {
                        break;
                    }
                    TokenType::StringLiteral => {
                        //TODO: check for valid escape sequences: \t \n \0 \xNN \r \\ \" \uNNNN
                        line.add_token(lexeme, fragment[1..(fragment.len() - 1)].into(), lex_pos);
                    }
                    TokenType::RegexLiteral => {
                        if let Ok(regex) = Regex::new(&fragment[2..(fragment.len() - 1)]) {
                            line.add_token(lexeme, regex.into(), lex_pos);
                        } else {
                            return Err(LexError {
                                message: format!("Malformed regular expression: '{}'", fragment),
                                position: lex_pos,
                            });
                        }
                    }
                    TokenType::IntegerLiteral => {
                        line.add_token(
                            lexeme,
                            // Unwrapping because Logos already checked that it is actually a well formatted integer
                            fragment.parse::<i64>().unwrap().into(),
                            lex_pos,
                        );
                    }
                    TokenType::FloatLiteral => {
                        line.add_token(
                            lexeme,
                            // Unwrapping because Logos already checked that it is actually a well formatted float
                            fragment.parse::<f64>().unwrap().into(),
                            lex_pos,
                        );
                    }
                    TokenType::BooleanLiteral => {
                        if fragment == "true" {
                            line.add_token(lexeme, true.into(), lex_pos);
                        } else {
                            line.add_token(lexeme, false.into(), lex_pos);
                        }
                    }
                    TokenType::Macro => {
                        line.add_token(lexeme, fragment[1..].into(), lex_pos);
                    }
                    TokenType::Identifier => {
                        line.add_token(lexeme, fragment.into(), lex_pos);
                    }
                    _ => {
                        line.add_token(lexeme, TokenData::None, lex_pos);
                    }
                }
            } else {
                //TODO: check for the most common mistakes: not closed string
                return Err(LexError {
                    message: format!("Unrecognized lexeme: '{}'", fragment),
                    position: lex_pos,
                });
            }
        }

        Ok(line)
    }

    fn add_token(&mut self, token_type: TokenType, data: TokenData, pos: usize) {
        self.tokens.push(Token {
            token_type,
            data,
            offset: pos,
        });
    }

    fn find_indentation(code: &str) -> Result<Option<(usize, IndentationType)>, LexError> {
        let mut indent_type = IndentationType::None;
        for (i, ch) in code.chars().enumerate() {
            match ch {
                ' ' | '\t' => {
                    if indent_type == IndentationType::None {
                        indent_type = ch.into();
                    } else if IndentationType::from(ch) != indent_type {
                        return Err(LexError {
                            message: "Mismatching indentation, mixed spaces and tabs".into(),
                            position: i,
                        });
                    }
                }
                _ => return Ok(Some((i, indent_type))),
            }
        }
        Ok(None)
    }
}

#[derive(Debug)]
/// Token object.
pub struct Token {
    /// Token type.
    pub token_type: TokenType,
    /// Token content.
    pub data: TokenData,
    /// Token position within the line of code.
    pub offset: usize,
}

#[derive(Debug)]
/// Line position properties.
pub struct Position {
    /// Line indentation.
    pub indentation: usize,
    /// Type of indentation symbol.
    pub indentation_type: IndentationType,
    /// Line number within the file.
    pub line_num: usize,
    /// Line length.
    pub length: usize,
}

#[derive(Debug, PartialEq)]
/// Indentation symbol type.
pub enum IndentationType {
    /// Indentation with spaces.
    Space,
    /// Indentation with tabs.
    Tab,
    /// No indentation.
    None,
}

impl From<char> for IndentationType {
    fn from(value: char) -> Self {
        match value {
            ' ' => Self::Space,
            '\r' => Self::Tab,
            _ => Self::None,
        }
    }
}

#[derive(Debug, Clone)]
/// Token data.
pub enum TokenData {
    String(String),
    Regex(Regex),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    None,
}

impl Display for TokenData {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            TokenData::String(s) => write!(f, "\"{}\"", s),
            TokenData::Regex(r) => write!(f, "\"{}\"", r),
            TokenData::Integer(i) => write!(f, "{}", i),
            TokenData::Float(fl) => write!(f, "{}", fl),
            TokenData::Boolean(b) => write!(f, "{}", b),
            TokenData::None => write!(f, ""),
        }
    }
}

impl From<i64> for TokenData {
    fn from(value: i64) -> Self {
        TokenData::Integer(value)
    }
}

impl From<f64> for TokenData {
    fn from(value: f64) -> Self {
        TokenData::Float(value)
    }
}

impl From<bool> for TokenData {
    fn from(value: bool) -> Self {
        TokenData::Boolean(value)
    }
}

impl From<String> for TokenData {
    fn from(value: String) -> Self {
        TokenData::String(value)
    }
}

impl From<&str> for TokenData {
    fn from(value: &str) -> Self {
        TokenData::String(value.into())
    }
}

impl From<Regex> for TokenData {
    fn from(value: Regex) -> Self {
        TokenData::Regex(value)
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Line;

    #[test]
    fn no_forbidden_chars() {
        assert!(Line::scan_tokens("const ME = \"IZE Language\"", 0).is_ok());
        assert!(Line::scan_tokens("const ME =\t\"IZE Language\"", 0).is_ok());

        assert!(Line::scan_tokens("const ME =\n\"IZE Language\"", 0).is_err());
        assert!(Line::scan_tokens("const ME =\r\"IZE Language\"", 0).is_err());
        assert!(Line::scan_tokens("const ME =\0\"IZE Language\"", 0).is_err());
        assert!(Line::scan_tokens("const ME =\x07\"IZE Language\"", 0).is_err());
    }
}
