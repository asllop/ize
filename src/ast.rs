//! # IZE Abstract Syntax Tree
//!
//! This module contains all the types and methods necessary to generate an AST.

use alloc::{boxed::Box, string::String, vec::Vec};
use rustc_hash::FxHashMap;
use crate::common::Pos;

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

//TODO: create a Vec<Expr>, and use indexes to this vec instead of Box<Expr> to reduce allocations.
//TODO: maybe an alternative allocator could improve this.

#[derive(Debug)]
/// Expression set.
pub enum ExprSet {
    //TODO: chain
    //TODO: let
    //TODO: select
    //TODO: unwrap
    //TODO: dot
    Literal(Literal),
    Identifier(String),
    Group {
        expr: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    BinaryOp {
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
        func: String,
        args: Vec<Expr>,
    },
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
/// Expression.
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
/// Command.
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
/// Import.
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
    File(String)
}

#[derive(Debug)]
/// Dot path.
pub struct DotPath {
    pub path: Vec<String>
}

#[derive(Debug)]
/// Model.
pub enum Model {
    Struct(StructModel),
    Alias(Type)
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
/// Transfer.
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
/// Pipe.
pub struct Pipe {
    pub run: bool,
    pub name: String,
    pub items: Vec<PipeItem>,
}

#[derive(Debug)]
/// Pipe item.
pub struct PipeItem {
    pub name: String,
    pub pipe_struct: Option<PipeStruct>
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
    Struct(PipeStruct)
}

#[derive(Debug)]
/// Type.
pub struct Type {
    pub name: String,
    pub content: Vec<Type>
}