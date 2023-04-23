use alloc::{
    string::String,
    vec::Vec,
};
use core::str::FromStr;

#[derive(Debug)]
/// Lexical analysis error.
pub struct LexError {
    /// Error message.
    pub message: String,
    /// Position in the line where the error was found.
    pub position: usize,
}

impl FromStr for Line {
    type Err = LexError;

    fn from_str(_code: &str) -> Result<Self, Self::Err> {
        //TODO: Implement token scanner
        Ok(Line {
            tokens: Vec::new(),
            position: Position {
                indentation: 0,
                indentation_type: IndentationType::Tab,
                line_num: 0,
                length: 0
            },
        })
    }
}

pub struct Line {
	pub tokens: Vec<Token>,
    /// Line position within the source code.
	pub position: Position,
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

pub enum IndentationType {
	Space,
	Tab,
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
