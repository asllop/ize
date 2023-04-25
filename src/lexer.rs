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
//!     Err(err) => panic!("Error: \"{}\" at offset {}", err.message, err.position + 1),
//! };
//! ```

use alloc::{string::String, vec::Vec};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t]+")]
/// Patterns to scan valid lexemes.
enum Lexeme {
    // Flow and multipurpose
    #[token("match")]
    Match,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("as")]
    As,

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
    #[token("Regex")]
    RegexType,

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

        let code = &code[line.position.indentation..];

        // Start regular analysis part
        let lex = Lexeme::lexer(code);
        for (lexeme, pos) in lex.spanned() {
            let fragment = &code[pos.start..pos.end];

            if let Ok(lexeme) = lexeme {
                match lexeme {
                    Lexeme::Comment => {
                        break;
                    }
                    Lexeme::StringLiteral => {
                        //TODO: check for valid escape sequences: \t \n \0 \xNN \r \\ \" \uNNNN
                        line.add_token(TokenId::StringLiteral(fragment.into()), pos.start);
                    }
                    _ => {
                        // Convert from Lexeme to Token
                        if let Ok(token_id) = TokenId::try_from(lexeme) {
                            line.add_token(token_id, pos.start);
                        } else {
                            // handle conversion of literals, macros and identifiers
                            match lexeme {
                                Lexeme::IntegerLiteral => {
                                    line.add_token(
                                        TokenId::IntegerLiteral(fragment.parse().unwrap()),
                                        pos.start,
                                    );
                                }
                                Lexeme::FloatLiteral => {
                                    line.add_token(
                                        TokenId::FloatLiteral(fragment.parse().unwrap()),
                                        pos.start,
                                    );
                                }
                                Lexeme::BooleanLiteral => {
                                    if fragment == "true" {
                                        line.add_token(TokenId::BooleanLiteral(true), pos.start);
                                    } else {
                                        line.add_token(TokenId::BooleanLiteral(false), pos.start);
                                    }
                                }
                                Lexeme::Macro => {
                                    line.add_token(TokenId::Macro(fragment.into()), pos.start);
                                }
                                Lexeme::Identifier => {
                                    line.add_token(TokenId::Identifier(fragment.into()), pos.start);
                                }
                                _ => {
                                    return Err(LexError {
                                        message: format!(
                                            "Unexpected lexeme type {:?}: '{}'",
                                            lexeme, fragment
                                        ),
                                        position: pos.start,
                                    });
                                }
                            }
                        }
                    }
                }
            } else {
                return Err(LexError {
                    message: format!("Unrecognized lexeme: '{}'", fragment),
                    position: pos.start,
                });
            }
        }

        Ok(line)
    }

    fn add_token(&mut self, token_id: TokenId, pos: usize) {
        self.tokens.push(Token {
            id: token_id,
            offset: pos,
        });
    }

    fn find_indentation(code: &str) -> Result<Option<(usize, IndentationType)>, LexError> {
        let mut indent_type = IndentationType::None;
        let mut first_indent_char = '\0';
        for (i, ch) in code.chars().enumerate() {
            match ch {
                ' ' | '\t' => {
                    if first_indent_char == '\0' {
                        first_indent_char = ch;
                        indent_type = ch.into();
                    } else if ch != first_indent_char {
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
    /// Token identificator.
    pub id: TokenId,
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

#[derive(Debug)]
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

#[derive(Debug)]
/// Token identificator.
pub enum TokenId {
    // Object definition tokens.
    StructDefinition,
    MapDefinition,
    ListDefinition,
    StringDefinition,
    BooleanDefinition,
    IntegerDefinition,
    FloatDefinition,
    DynDefinition,
    TransferDefinition,
    BufferDefinition,
    PipelineDefinition,
    ConstDefinition,

    // Data type tokens.
    StringType,
    IntegerType,
    FloatType,
    BooleanType,
    MapType,
    PairType,
    ListType,
    AnyType,
    NoneType,
    NullType,
    RegexType,

    // Data literal tokens.
    StringLiteral(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),

    // Macro token.
    Macro(String),

    // Identifier token.
    Identifier(String),

    // The rest.
    Match,             // match
    If,                // if
    Else,              // else
    As,                // as
    Underscore,        // _
    OpenParenth,       // (
    ClosingParenth,    // )
    Comma,             // ,
    Colon,             // :
    Plus,              // +
    Star,              // *
    Slash,             // /
    Percent,           // %
    Minus,             // -
    Arrow,             // ->
    OpenAngleBrack,    // <
    ClosingAngleBrack, // >
    GtEqual,           // >=
    LtEqual,           // <=
    And,               // &
    TwoAnds,           // &&
    Or,                // |
    TwoOrs,            // ||
    Exclam,            // !
    NotEqual,          // !=
    Equal,             // =
    TwoEquals,         // ==
    Dot,               // .
    TwoDots,           // ..
    ThreeDots,         // ...
}

impl TryFrom<Lexeme> for TokenId {
    type Error = ();

    fn try_from(value: Lexeme) -> Result<Self, Self::Error> {
        match value {
            Lexeme::Match => Ok(Self::Match),
            Lexeme::If => Ok(Self::If),
            Lexeme::Else => Ok(Self::Else),
            Lexeme::As => Ok(Self::As),
            Lexeme::Underscore => Ok(Self::Underscore),
            Lexeme::OpenParenth => Ok(Self::OpenParenth),
            Lexeme::ClosingParenth => Ok(Self::ClosingParenth),
            Lexeme::Comma => Ok(Self::Comma),
            Lexeme::Colon => Ok(Self::Colon),
            Lexeme::Plus => Ok(Self::Plus),
            Lexeme::Star => Ok(Self::Star),
            Lexeme::Slash => Ok(Self::Slash),
            Lexeme::Percent => Ok(Self::Percent),
            Lexeme::Minus => Ok(Self::Minus),
            Lexeme::Arrow => Ok(Self::Arrow),
            Lexeme::OpenAngleBrack => Ok(Self::OpenAngleBrack),
            Lexeme::ClosingAngleBrack => Ok(Self::ClosingAngleBrack),
            Lexeme::GtEqual => Ok(Self::GtEqual),
            Lexeme::LtEqual => Ok(Self::LtEqual),
            Lexeme::And => Ok(Self::And),
            Lexeme::TwoAnds => Ok(Self::TwoAnds),
            Lexeme::Or => Ok(Self::Or),
            Lexeme::TwoOrs => Ok(Self::TwoOrs),
            Lexeme::Exclam => Ok(Self::Exclam),
            Lexeme::NotEqual => Ok(Self::NotEqual),
            Lexeme::Equal => Ok(Self::Equal),
            Lexeme::TwoEquals => Ok(Self::TwoEquals),
            Lexeme::Dot => Ok(Self::Dot),
            Lexeme::TwoDots => Ok(Self::TwoDots),
            Lexeme::ThreeDots => Ok(Self::ThreeDots),
            Lexeme::StringType => Ok(Self::StringType),
            Lexeme::IntegerType => Ok(Self::IntegerType),
            Lexeme::FloatType => Ok(Self::FloatType),
            Lexeme::BooleanType => Ok(Self::BooleanType),
            Lexeme::MapType => Ok(Self::MapType),
            Lexeme::PairType => Ok(Self::PairType),
            Lexeme::ListType => Ok(Self::ListType),
            Lexeme::AnyType => Ok(Self::AnyType),
            Lexeme::NoneType => Ok(Self::NoneType),
            Lexeme::NullType => Ok(Self::NullType),
            Lexeme::RegexType => Ok(Self::RegexType),
            Lexeme::StructDefinition => Ok(Self::StructDefinition),
            Lexeme::MapDefinition => Ok(Self::MapDefinition),
            Lexeme::ListDefinition => Ok(Self::ListDefinition),
            Lexeme::StringDefinition => Ok(Self::StringDefinition),
            Lexeme::BooleanDefinition => Ok(Self::BooleanDefinition),
            Lexeme::IntegerDefinition => Ok(Self::IntegerDefinition),
            Lexeme::FloatDefinition => Ok(Self::FloatDefinition),
            Lexeme::DynDefinition => Ok(Self::DynDefinition),
            Lexeme::TransferDefinition => Ok(Self::TransferDefinition),
            Lexeme::BufferDefinition => Ok(Self::BufferDefinition),
            Lexeme::PipelineDefinition => Ok(Self::PipelineDefinition),
            Lexeme::ConstDefinition => Ok(Self::ConstDefinition),
            _ => {
                // we have to handle literals, macros and identifiers conversion outside
                Err(())
            }
        }
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
