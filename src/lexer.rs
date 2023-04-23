use alloc::{
    string::String,
    vec::Vec,
};

#[derive(Debug)]
/// Lexical analysis error.
pub struct LexError {
    /// Error message.
    pub message: String,
    /// Position in the line where the error was found.
    pub position: usize,
}

#[derive(Debug)]
pub struct Line {
	pub tokens: Vec<Token>,
    /// Line position within the source code.
	pub position: Position,
}

impl Line {
    pub fn add_token(&mut self, token_id: TokenId, pos: usize) {
        self.tokens.push(Token {
            id: token_id,
            offset: pos,
        });
    }

    pub fn scan_tokens(code: &str, line_num: usize) -> Result<Self, LexError> {
        let mut line = Line {
            tokens: Vec::new(),
            position: Position {
                indentation: 0,
                indentation_type: IndentationType::None,
                line_num,
                length: code.len()
            },
        };

        // Get line indentation
        match Self::find_indentation(code) {
            Ok(Some((i, t))) => {
                line.position.indentation = i;
                line.position.indentation_type = t;
            },
            Ok(None) => {
                // Empty line
                return Ok(line);
            },
            Err(err) => {
                return Err(err)
            },
        }

        // Start looking for actual tokens
        let mut in_string = false;
        let mut in_symbol = false;
        let mut in_number = false;
        let mut current_pos = line.position.indentation;

        loop {
            let ch = Self::next_char(code, current_pos);
            current_pos += 1;

            if let Some(ch) = ch {
                match ch {
                    // Single char tokens
                    ':' => {
                        line.add_token(TokenId::Colon, current_pos);
                    },
                    ',' => {
                        line.add_token(TokenId::Comma, current_pos);
                    },
                    '(' => {
                        line.add_token(TokenId::OpenParenth, current_pos);
                    },
                    ')' => {
                        line.add_token(TokenId::ClosingParenth, current_pos);
                    },
                    '+' => {
                        line.add_token(TokenId::Plus, current_pos);
                    },
                    '-' => {
                        line.add_token(TokenId::Minus, current_pos);
                    },
                    '*' => {
                        line.add_token(TokenId::Star, current_pos);
                    },
                    '%' => {
                        line.add_token(TokenId::Percent, current_pos);
                    },
                    // Two char tokens
                    '/' => {
                        // it can be / or //
                        if let Some('/') = Self::next_char(code, current_pos) {
                            // It's a comment, ignore the rest
                            break;
                        }
                        else {
                            line.add_token(TokenId::Slash, current_pos);
                        }
                    },
                    '=' => {
                        // it can be = or ==
                        if let Some('=') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            line.add_token(TokenId::TwoEquals, current_pos);
                        }
                        else {
                            line.add_token(TokenId::Equal, current_pos);
                        }
                    },
                    '&' => {
                        // it can be & or &&
                        if let Some('&') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            line.add_token(TokenId::TwoAnds, current_pos);
                        }
                        else {
                            line.add_token(TokenId::And, current_pos);
                        }
                    },
                    '|' => {
                        // it can be | or ||
                        if let Some('|') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            line.add_token(TokenId::TwoOrs, current_pos);
                        }
                        else {
                            line.add_token(TokenId::Or, current_pos);
                        }
                    },
                    '!' => {
                        // it can be ! or !=
                        if let Some('=') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            line.add_token(TokenId::NotEqual, current_pos);
                        }
                        else {
                            line.add_token(TokenId::Exclam, current_pos);
                        }
                    },
                    '<' => {
                        // it can be < or <=
                        if let Some('=') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            line.add_token(TokenId::LtEqual, current_pos);
                        }
                        else {
                            line.add_token(TokenId::OpenAngleBrack, current_pos);
                        }
                    },
                    '>' => {
                        // it can be > or >=
                        if let Some('=') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            line.add_token(TokenId::GtEqual, current_pos);
                        }
                        else {
                            line.add_token(TokenId::ClosingAngleBrack, current_pos);
                        }
                    },
                    // Three char tokens
                    '.' => {
                        // it can be . or .. or ...
                        if let Some('.') = Self::next_char(code, current_pos) {
                            current_pos += 1;
                            if let Some('.') = Self::next_char(code, current_pos) {
                                current_pos += 1;
                                line.add_token(TokenId::ThreeDots, current_pos);
                            }
                            else {
                                line.add_token(TokenId::TwoDots, current_pos);
                            }
                        }
                        else {
                            line.add_token(TokenId::Dot, current_pos);
                        }
                    },
                    // Multi char tokens
                    ' ' | '\t' => {},
                    _ => {}
                }
            }
            else {
                // Reached the end
                break;
            }
        }

        Ok(line)
    }

    fn next_char(code: &str, current_pos: usize) -> Option<char> {
        code.chars().nth(current_pos)
    }

    fn find_indentation(code: &str) -> Result<Option<(usize, IndentationType)>, LexError> {
        let mut indent = IndentationType::None;
        for (i, ch) in code.chars().enumerate() {
            match ch {
                ' ' => {
                    if let IndentationType::None | IndentationType::Space = indent {
                        indent = IndentationType::Space;    
                    }
                    else {
                        return Err(LexError {
                            message: "Mismatching indentation, mixed spaces and tabs".into(),
                            position: i
                        })
                    }
                },
                '\t' => {
                    if let IndentationType::None | IndentationType::Tab = indent {
                        indent = IndentationType::Tab;    
                    }
                    else {
                        return Err(LexError {
                            message: "Mismatching indentation, mixed spaces and tabs".into(),
                            position: i
                        })
                    }
                },
                _ => {
                    return Ok(Some((i, indent)))
                }
            }
        }
        Ok(None)
    }
}

#[derive(Debug)]
pub struct Token {
	pub id: TokenId,
    /// Token position within the line of code.
	pub offset: usize,
}

#[derive(Debug)]
pub struct Position {
	pub indentation: usize,
    pub indentation_type: IndentationType,
	pub line_num: usize,
	pub length: usize,
}

#[derive(Debug)]
pub enum IndentationType {
	Space,
	Tab,
    None,
}

#[derive(Debug)]
pub enum TokenId {
	Definition(DefinitionToken),
	Type(TypeToken),
	Literal(LiteralToken),
    Macro(MacroToken),
	Identifier(String),
	Match,              // match
	If,                 // if
	Else,               // else
    As,                 // as
	OpenParenth,        // (
	ClosingParenth,     // )
    Comma,              // ,
    Colon,              // :
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Percent,            // %
	OpenAngleBrack,     // <
	ClosingAngleBrack,  // >
    GtEqual,            // >=
    LtEqual,            // <=
    And,                // &
    TwoAnds,            // &&
    Or,                 // |
    TwoOrs,             // ||
    Exclam,             // !
    NotEqual,           // !=
    Equal,              // =
    TwoEquals,          // ==
	Dot,                // .
	TwoDots,            // ..
	ThreeDots,          // ...
}

// ------- Token Variants -------

#[derive(Debug)]
pub enum DefinitionToken {
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
}

#[derive(Debug)]
pub enum TypeToken {
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
}

#[derive(Debug)]
pub enum LiteralToken {
    StringLiteral(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    RegexLiteral(String),   //TODO: use the Regex type from the regex crate
}

#[derive(Debug)]
pub enum MacroToken {
	ImportMacro,
	OnErrorMacro,
}
