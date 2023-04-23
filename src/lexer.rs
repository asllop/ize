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

pub struct Line {
	pub tokens: Vec<Token>,
    /// Line position within the source code.
	pub position: Position,
}

impl Line {
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
        match Self::find_indentation(code) {
            Ok(Some((i, t))) => {
                line.position.indentation = i;
                line.position.indentation_type = t;
                Ok(line)
            },
            Ok(None) => {
                // Empty line
                Ok(line)
            },
            Err(err) => {
                Err(err)
            },
        }

        //TODO: scan tokens
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
                            message: "Mismatching indentation characters".into(),
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
                            message: "Mismatching indentation characters".into(),
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

    fn _find_next_token(_code: &str, _position: usize) {
        todo!("Find next token  in the code stream")
    }
}

pub struct Token {
	pub id: TokenId,
    /// Token position within the line of code.
	pub offset: usize,
}

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

pub enum TokenId {
	Definition(DefinitionToken),
	Type(TypeToken),
	Literal(LiteralToken),
    Macro(MacroToken),
	Identifier(String),
    Underscore,
	Match,
	If,
	Else,
    As,
	OpenParenth,
	ClosingParenth,
	OpenAngleBrack,
	ClosingAngleBrack,
	Comma,
	Dot,
	TwoDots,
	ThreeDots,
	Colon,
    Minus,
    Plus,
    Slash,
    Percent,
    Star,
    And,
    TwoAnds,
    Or,
    TwoOrs,
    Exclam,
    Equal,
    TwoEquals,
    NotEqual,
    GtEqual,
    LtEqual,
    // PlusEqual,
    // MinusEqual,
    // StarEqual,
    // SlashEqual,
    // PercentEqual,
}

// ------- Token Variants -------

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

pub enum LiteralToken {
    StringLiteral(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    RegexLiteral(String),   //TODO: use the Regex type from the regex crate
}

pub enum MacroToken {
	ImportMacro,
	OnErrorMacro,
}
