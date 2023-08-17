//! # IZE Abstract Syntax Tree
//!
//! This module contains all the types and methods necessary to generate an AST.

use crate::{
    common::Pos,
    lexer::{Lexeme, Lexer},
    IzeErr,
};
use alloc::{boxed::Box, string::String, vec::Vec};
use rustc_hash::FxHashMap;

#[derive(Debug)]
/// Abstract Syntax Tree.
pub struct Ast {
    pub commands: Vec<Command>,
}

impl<'a> Ast {
    pub fn build(code: &'a str) -> Result<Self, IzeErr> {
        let mut lexer = Lexer::new(code);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.scan_token()?;
            match token.lexeme {
                Lexeme::EOF => break,
                Lexeme::Nothing => {}
                _ => tokens.push(token),
            }
        }
        todo!("Parse and build AST")
    }
}

#[derive(Debug)]
/// Unary operation set.
pub enum Unary {
    Negate,
    Minus,
}

#[derive(Debug)]
/// Binary operation set.
pub enum Binary {
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

//TODO: create a Vec<Expr>, and use indexes to this vec instead of Box<Expr> to reduce allocations.
//TODO: maybe an alternative allocator could improve this.

#[derive(Debug)]
/// Expression set.
pub enum ExprSet {
    Literal(Literal),
    Identifier(String),
    Group {
        expr: Box<Expr>,
    },
    UnaryOp {
        op: Unary,
        expr: Box<Expr>,
    },
    BinaryOp {
        op: Binary,
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
    pub pos: Pos,
}

impl Expr {
    pub fn new(expr: ExprSet, pos: Pos) -> Self {
        Self { expr, pos }
    }
}

type FieldName = String;

#[derive(Debug)]
/// Language command (Import, Model, Transfer and Pipe).
pub struct Command {
    pub command: CommandSet,
    pub pos: Pos,
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
    pub alias: String,
}

#[derive(Debug)]
/// Import path.
pub enum ImportPath {
    Dot(DotPath),
    File(String),
}

#[derive(Debug)]
/// Dot path.
pub struct DotPath {
    pub path: Vec<String>,
}

#[derive(Debug)]
/// Model command.
pub enum Model {
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
    pub is_remain: bool,
    pub actual_name: String,
    pub field_type: Type,
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
    pub name: String,
    pub inner: Vec<Type>,
}
