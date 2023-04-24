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
        let mut current_token = Vec::<char>::new();
        let mut current_pos = line.position.indentation;

        loop {
            if let Some(ch) = Self::next_char(code, current_pos) {
                match ch {
                    // Ignore spaces and tabs
                    ' ' | '\t' => {},
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
                    '*' => {
                        line.add_token(TokenId::Star, current_pos);
                    },
                    '%' => {
                        line.add_token(TokenId::Percent, current_pos);
                    },
                    // Double char tokens
                    '/' => {
                        // it can be / or //
                        if let Some('/') = Self::next_char(code, current_pos + 1) {
                            // It's a comment, ignore the rest
                            break;
                        }
                        else {
                            line.add_token(TokenId::Slash, current_pos);
                        }
                    },
                    '-' => {
                        // it can be - or ->
                        if let Some('>') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::Return, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::Minus, current_pos);
                        }
                    },
                    '=' => {
                        // it can be = or ==
                        if let Some('=') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::TwoEquals, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::Equal, current_pos);
                        }
                    },
                    '&' => {
                        // it can be & or &&
                        if let Some('&') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::TwoAnds, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::And, current_pos);
                        }
                    },
                    '|' => {
                        // it can be | or ||
                        if let Some('|') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::TwoOrs, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::Or, current_pos);
                        }
                    },
                    '!' => {
                        // it can be ! or !=
                        if let Some('=') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::NotEqual, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::Exclam, current_pos);
                        }
                    },
                    '<' => {
                        // it can be < or <=
                        if let Some('=') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::LtEqual, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::OpenAngleBrack, current_pos);
                        }
                    },
                    '>' => {
                        // it can be > or >=
                        if let Some('=') = Self::next_char(code, current_pos + 1) {
                            line.add_token(TokenId::GtEqual, current_pos);
                            current_pos += 1;
                        }
                        else {
                            line.add_token(TokenId::ClosingAngleBrack, current_pos);
                        }
                    },
                    // Triple char tokens
                    '.' => {
                        // it can be . or .. or ...
                        if let Some('.') = Self::next_char(code, current_pos + 1) {
                            if let Some('.') = Self::next_char(code, current_pos + 2) {
                                line.add_token(TokenId::ThreeDots, current_pos);
                                current_pos += 2;
                            }
                            else {
                                line.add_token(TokenId::TwoDots, current_pos);
                                current_pos += 1;
                            }
                        }
                        else {
                            line.add_token(TokenId::Dot, current_pos);
                        }
                    },
                    // Multi char tokens
                    '\"' => {
                        if current_token.len() > 0 {
                            panic!("Started a String and current token is not empty")
                        }
                        let mut str_current_pos = current_pos + 1;
                        loop {
                            if let Some(ch) = Self::next_char(code, str_current_pos) {
                                match ch {
                                    '\"' => {
                                        line.add_token(TokenId::Literal(LiteralToken::StringLiteral(current_token.into_iter().collect())), current_pos);
                                        current_token = Vec::new();
                                        current_pos = str_current_pos;
                                        break;
                                    },
                                    '\\' => {
                                        // Escape sequence
                                        match Self::next_char(code, str_current_pos + 1) {
                                            Some('\"') => {
                                                current_token.push('\"');
                                            },
                                            Some('n') => {
                                                current_token.push('\n');
                                            },
                                            Some('t') => {
                                                current_token.push('\t');
                                            },
                                            Some('r') => {
                                                current_token.push('\r');
                                            },
                                            Some('\\') => {
                                                current_token.push('\\');
                                            },
                                            Some('0') => {
                                                current_token.push('\0');
                                            },
                                            _ => {
                                                return Err(LexError {
                                                    message: "Unrecognized escape sequence".into(),
                                                    position: str_current_pos
                                                })
                                            }
                                        }
                                        str_current_pos += 1;
                                    },
                                    _ => {
                                        current_token.push(ch);
                                    }
                                }
                            }
                            else {
                                // End of line
                                return Err(LexError {
                                    message: "Premature end of string literal".into(),
                                    position: current_pos
                                });
                            }
                            str_current_pos += 1;
                        }
                    },
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        if current_token.len() > 0 {
                            panic!("Started a Number and current token is not empty")
                        }
                        current_token.push(ch);
                        let mut num_current_pos = current_pos + 1;
                        loop {
                            if let Some(ch) = Self::next_char(code, num_current_pos) {
                                match ch {
                                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                                        current_token.push(ch);
                                    },
                                    '.' => {
                                        if let Some('.') = Self::next_char(code, num_current_pos + 1) {
                                            // Two dots, it's an integer that ends here
                                            let num_str: String = current_token.into_iter().collect();
                                            current_token = Vec::new();
                                            let token = Self::number_token(&num_str, current_pos)?;
                                            line.tokens.push(token);
                                            current_pos = num_current_pos - 1;
                                            break;
                                        }
                                        else {
                                            current_token.push(ch);
                                        }
                                    }
                                    _ => {
                                        // End of number
                                        let num_str: String = current_token.into_iter().collect();
                                        current_token = Vec::new();
                                        let token = Self::number_token(&num_str, current_pos)?;
                                        line.tokens.push(token);
                                        current_pos = num_current_pos - 1;
                                        break;
                                    }
                                }
                            }
                            else {
                                // End of line
                                let num_str: String = current_token.into_iter().collect();
                                current_token = Vec::new();
                                let token = Self::number_token(&num_str, current_pos)?;
                                line.tokens.push(token);
                                current_pos = num_current_pos - 1;
                                break;
                            }
                            num_current_pos += 1;
                        }
                    },
                    '#' => {
                        if current_token.len() > 0 {
                            panic!("Started a Macro and current token is not empty")
                        }
                        let mut macro_current_pos = current_pos + 1;
                        loop {
                            if let Some(ch) = Self::next_char(code, macro_current_pos) {
                                if ch.is_alphanumeric() || ch == '_' {
                                    current_token.push(ch);
                                }
                                else {
                                    // End of macro
                                    line.add_token(TokenId::Macro(current_token.into_iter().collect()), current_pos);
                                    current_token = Vec::new();
                                    current_pos = macro_current_pos - 1;
                                    break;
                                }
                            }
                            else {
                                // End of line
                                line.add_token(TokenId::Macro(current_token.into_iter().collect()), current_pos);
                                current_token = Vec::new();
                                current_pos = macro_current_pos - 1;
                                break;
                            }
                            macro_current_pos += 1;
                        }
                    },
                    _ => {
                        // Any other token: not single, double, triple, macro, string or number.
                        if current_token.len() > 0 {
                            panic!("Started a Identifier and current token is not empty")
                        }
                        current_token.push(ch);
                        let mut sym_current_pos = current_pos + 1;
                        loop {
                            if let Some(ch) = Self::next_char(code, sym_current_pos) {
                                if ch.is_alphanumeric() || ch == '_' {
                                    current_token.push(ch);
                                }
                                else {
                                    // End of symbol
                                    let symbol_str: String = current_token.into_iter().collect();
                                    current_token = Vec::new();
                                    line.tokens.push(Self::symbol_token(symbol_str, current_pos));
                                    current_pos = sym_current_pos - 1;
                                    break;
                                }
                            }
                            else {
                                // End of line
                                let symbol_str: String = current_token.into_iter().collect();
                                current_token = Vec::new();
                                line.tokens.push(Self::symbol_token(symbol_str, current_pos));
                                current_pos = sym_current_pos - 1;
                                break;
                            }
                            sym_current_pos += 1;
                        }
                    }
                }
            }
            else {
                // Reached the end line
                break;
            }

            current_pos += 1;
        }

        Ok(line)
    }

    fn next_char(code: &str, current_pos: usize) -> Option<char> {
        code.chars().nth(current_pos)
    }

    fn symbol_token(symbol: String, current_pos: usize) -> Token {
        let token_id = match symbol.as_str() {
            "match" => TokenId::Match,
            "if" => TokenId::If,
            "else" => TokenId::Else,
            "as" => TokenId::As,
            "_" => TokenId::Underscore,
            "true" => TokenId::Literal(LiteralToken::BooleanLiteral(true)),
            "false" => TokenId::Literal(LiteralToken::BooleanLiteral(true)),
            "string" => TokenId::Definition(DefinitionToken::StringDefinition),
            "struct" => TokenId::Definition(DefinitionToken::StructDefinition),
            "integer" => TokenId::Definition(DefinitionToken::IntegerDefinition),
            "float" => TokenId::Definition(DefinitionToken::FloatDefinition),
            "dyn" => TokenId::Definition(DefinitionToken::DynDefinition),
            "boolean" => TokenId::Definition(DefinitionToken::BooleanDefinition),
            "map" => TokenId::Definition(DefinitionToken::MapDefinition),
            "list" => TokenId::Definition(DefinitionToken::ListDefinition),
            "transfer" => TokenId::Definition(DefinitionToken::TransferDefinition),
            "buffer" => TokenId::Definition(DefinitionToken::BufferDefinition),
            "pipeline" => TokenId::Definition(DefinitionToken::PipelineDefinition),
            "const" => TokenId::Definition(DefinitionToken::ConstDefinition),
            "Any" => TokenId::Type(TypeToken::AnyType),
            "Boolean" => TokenId::Type(TypeToken::BooleanType),
            "Float" => TokenId::Type(TypeToken::FloatType),
            "Integer" => TokenId::Type(TypeToken::IntegerType),
            "List" => TokenId::Type(TypeToken::ListType),
            "Map" => TokenId::Type(TypeToken::MapType),
            "None" => TokenId::Type(TypeToken::NoneType),
            "Null" => TokenId::Type(TypeToken::NullType),
            "Pair" => TokenId::Type(TypeToken::PairType),
            "String" => TokenId::Type(TypeToken::StringType),
            _ => TokenId::Identifier(symbol)
        };
        Token {
            id: token_id,
            offset: current_pos
        }
    }

    fn number_token(symbol: &str, current_pos: usize) -> Result<Token, LexError> {
        if let Ok(n) = symbol.parse::<i64>() {
            Ok(Token {
                id: TokenId::Literal(LiteralToken::IntegerLiteral(n)),
                offset: current_pos
            })
        }
        else if let Ok(n) = symbol.parse::<f64>() {
            Ok(Token {
                id: TokenId::Literal(LiteralToken::FloatLiteral(n)),
                offset: current_pos
            })
        }
        else {
            Err(LexError {
                message: "Parsing number".into(),
                position: current_pos
            })
        }
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
    Macro(String),
	Identifier(String),
	Match,              // match
	If,                 // if
	Else,               // else
    As,                 // as
    Underscore,         // _
	OpenParenth,        // (
	ClosingParenth,     // )
    Comma,              // ,
    Colon,              // :
    Plus,               // +
    Star,               // *
    Slash,              // /
    Percent,            // %
    Minus,              // -
    Return,             // ->
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