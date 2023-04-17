#[derive(Debug)]
/// Lexical analysis error.
pub struct LexError {
    /// Error message.
    pub message: String,
    /// Position in the line where the error was found.
    pub position: usize,
}

// /// Lexical analysis trait.
// pub trait Scan {
//     /// Scan one line of code and generate tokens.
//     fn scan_tokens(&self) -> Result<Line, LexError>;
// }

// impl Scan for &str {
//     fn scan_tokens(&self) -> Result<Line, LexError> {
//         todo!("Implement scan_tokens for &str")
//     }
// }

// Tokenize one line of code.
impl TryFrom<&str> for Line {
    type Error = LexError;

    fn try_from(_code: &str) -> Result<Self, Self::Error> {
        //TODO: Implement token scanner
        Ok(Line {
            tokens: Vec::new(),
            position: Position { indentation: 0, indent_type: IndentType::Tab, line_num: 0, length: 0 },
        })
    }
}

pub struct Line {
	pub tokens: Vec<Token>,
	pub position: Position,
}

pub struct Token {
	pub identity: TokenIdent,
	pub offset: usize,
}

pub struct Position {
	pub indentation: usize,
    pub indent_type: IndentType,
	pub line_num: usize,
	pub length: usize,
}

pub enum IndentType {
	Space,
	Tab,
}

pub enum TokenIdent {
	Definition(DefinitionToken),
	Type(TypeToken),
	Literal(LiteralToken),
    Macro(MacroToken),
	Identifier(String),
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
