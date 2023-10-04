//! # IZE Abstract Syntax Tree
//!
//! The AST models the code structure. This crate contains a collection of types to build and represent an AST.

use crate::{common::BuildErr, lexer::TokenKind, IzeErr, Pos};
use alloc::{boxed::Box, string::String, vec::Vec};
use rustc_hash::FxHashMap;

type ModuleName = String;
type SymbolName = String;

#[derive(Debug)]
/// Abstract Syntax Tree. Represents a parsed IZE file.
pub struct Ast {
    /// List of commands.
    pub commands: Vec<Command>,
    /// Imported modules: other ASTs, each one associated with a module name.
    pub imports: FxHashMap<ModuleName, Ast>,
    /// Source file.
    pub source: ImportPath,
    /// Symbol table, symbols defined in the current source file.
    pub symbols: FxHashMap<SymbolName, SymType>,
}

#[derive(Debug)]
/// Symbol type, used in the symbol table.
pub enum SymType {
    Model,
    Pipe,
    Transfer,
}

#[derive(Debug)]
/// Unary operation set.
pub enum UnaryOp {
    Negate,
    Minus,
}

#[derive(Debug)]
/// Binary operation set.
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    LazyAnd,
    LazyOr,
    Equal,
    NotEqual,
    LesserThan,
    GreaterThan,
    GtEqual,
    LtEqual,
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = IzeErr;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(BinaryOp::Add),
            TokenKind::Minus => Ok(BinaryOp::Sub),
            TokenKind::Star => Ok(BinaryOp::Mul),
            TokenKind::Slash => Ok(BinaryOp::Div),
            TokenKind::Percent => Ok(BinaryOp::Mod),
            TokenKind::LesserThan => Ok(BinaryOp::LesserThan),
            TokenKind::GreaterThan => Ok(BinaryOp::GreaterThan),
            TokenKind::GtEqual => Ok(BinaryOp::GtEqual),
            TokenKind::LtEqual => Ok(BinaryOp::LtEqual),
            TokenKind::And => Ok(BinaryOp::And),
            TokenKind::TwoAnds => Ok(BinaryOp::LazyAnd),
            TokenKind::Or => Ok(BinaryOp::Or),
            TokenKind::TwoOrs => Ok(BinaryOp::LazyOr),
            TokenKind::TwoEquals => Ok(BinaryOp::Equal),
            TokenKind::NotEqual => Ok(BinaryOp::NotEqual),
            _ => Result::ize_err(
                "Invalid TokenKind to BinaryOp conversion".into(),
                Pos::default(),
            ),
        }
    }
}

//TODO: create a Vec<Expr>, and use indexes to this vec instead of Box<Expr> to reduce allocations.
//TODO: maybe an alternative allocator could improve this.

#[derive(Debug)]
/// Expression set.
pub enum ExprSet {
    Literal(Literal),
    Type(Type),
    Identifier(String),
    Group {
        expr: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left_expr: Box<Expr>,
        right_expr: Box<Expr>,
    },
    IfElse {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Chain {
        chain: Vec<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
    },
    Dot {
        compos: Vec<Expr>,
    },
    Select {
        expr: Box<Expr>,
        alias: String,
        arms: Vec<Arm>,
    },
    Unwrap {
        expr: Box<Expr>,
        alias: String,
        arms: Vec<Arm>,
    },
}

#[derive(Debug)]
/// Select or Unwrap arm.
pub struct Arm {
    pub value: Box<Expr>,
    pub action: Box<Expr>,
    //TODO: Pos
}

#[derive(Debug)]
/// Literal.
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
    Null,
}

#[derive(Debug)]
/// Language expression.
pub struct Expr {
    pub expr: ExprSet,
    //TODO: rename to "start"
    pub pos: Pos,
    //TODO: add end position
}

impl Expr {
    pub fn new(expr: ExprSet, pos: Pos) -> Self {
        Self { expr, pos }
    }
}

pub type FieldName = String;

#[derive(Debug)]
/// Language command (Import, Model, Transfer and Pipe).
pub struct Command {
    pub command: CommandSet,
    //TODO: rename to "start"
    pub pos: Pos,
    //TODO: add end position
}

impl Command {
    pub fn new(command: CommandSet, pos: Pos) -> Self {
        Self { command, pos }
    }
}

#[derive(Debug)]
/// Command set.
pub enum CommandSet {
    Import(Import),
    Model(Model),
    Transfer(Transfer),
    Pipe(Pipe),
}

#[derive(Debug)]
/// Import command.
pub struct Import {
    pub packages: Vec<Package>,
}

#[derive(Debug)]
/// Import package.
pub struct Package {
    pub path: ImportPath,
    pub alias: Option<String>,
    //TODO: Pos
}

impl Package {
    pub fn mod_name(self) -> Result<String, &'static str> {
        if let Some(alias) = self.alias {
            Ok(alias)
        } else {
            if let ImportPath::Dot(mut dot_path) = self.path {
                if let Some(last_compo) = dot_path.path.pop() {
                    Ok(last_compo)
                } else {
                    // This should never happen, the parser enforces a non empty dot path.
                    Err("Dot paths can't be empty")
                }
            } else {
                // This should never happen, the parser enforces all absolute paths to have an alias.
                Err("Absolute paths must define an alias enforced by the parser")
            }
        }
    }
}

#[derive(Debug, Clone)]
/// Import path.
pub enum ImportPath {
    Dot(DotPath),
    File(String),
}

#[derive(Debug, Clone)]
/// Dot path.
pub struct DotPath {
    pub path: Vec<String>,
}

#[derive(Debug)]
/// Model command.
pub struct Model {
    pub name: String,
    pub model_type: ModelType,
}

#[derive(Debug)]
/// Model type.
pub enum ModelType {
    Struct(StructModel),
    Alias(Type),
}

#[derive(Debug)]
/// Struct model.
pub struct StructModel {
    pub fields: FxHashMap<FieldName, ModelField>,
    pub field_order: Vec<FieldName>,
}

#[derive(Debug)]
/// Model field.
pub struct ModelField {
    pub rename: Option<String>,
    pub field_type: FieldType,
    //TODO: Pos
}

#[derive(Debug)]
/// Model field type.
pub enum FieldType {
    Remain,
    Type(Type),
}

#[derive(Debug)]
/// Transfer command.
pub struct Transfer {
    pub name: String,
    pub input_type: Type,
    pub output_type: Type,
    pub def: TransferDef,
}

#[derive(Debug)]
/// Transfer definition.
pub enum TransferDef {
    Expr(Expr),
    Fields(TransferFields),
}

#[derive(Debug)]
/// Transfer fields definition.
pub struct TransferFields {
    pub fields: FxHashMap<FieldName, Expr>,
}

#[derive(Debug)]
/// Pipe command.
pub struct Pipe {
    pub run: bool,
    pub name: String,
    pub items: Vec<PipeItem>,
}

#[derive(Debug)]
/// Pipe item.
pub struct PipeItem {
    pub name: String,
    pub pipe_struct: Option<PipeStruct>,
    //TODO: Pos
}

#[derive(Debug)]
/// Pipe item struct.
pub struct PipeStruct {
    pub fields: FxHashMap<FieldName, PipeVal>,
}

#[derive(Debug)]
/// Pipe val.
pub enum PipeVal {
    Literal(Literal),
    Identifier(String),
    Struct(PipeStruct),
}

#[derive(Debug)]
/// Type.
pub struct Type {
    pub id: TypeId,
    pub inner: Vec<Type>,
}

#[derive(Debug)]
/// Type Id.
pub enum TypeId {
    Custom(String),
    String,
    Integer,
    Float,
    Boolean,
    List,
    Map,
    Mux,
    Tuple,
    None,
    Null,
    Any,
}

impl TryFrom<TokenKind> for TypeId {
    type Error = IzeErr;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::IntegerType => Ok(Self::Integer),
            TokenKind::FloatType => Ok(Self::Float),
            TokenKind::BooleanType => Ok(Self::Boolean),
            TokenKind::StringType => Ok(Self::String),
            TokenKind::NullType => Ok(Self::Null),
            TokenKind::NoneType => Ok(Self::None),
            TokenKind::MapType => Ok(Self::Map),
            TokenKind::ListType => Ok(Self::List),
            TokenKind::MuxType => Ok(Self::Mux),
            TokenKind::TupleType => Ok(Self::Tuple),
            _ => Result::ize_err(
                "Invalid TokenKind to TypeId conversion".into(),
                Pos::default(),
            ),
        }
    }
}
