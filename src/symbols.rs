//! # Symbols
//! 
//! Types to generate a symbol table and an intermediate AST out from the parser AST.
//! 

use crate::{ast::{AstNode, Command, Expression}, lexer::{Token, TokenPos}};
use alloc::{string::String, vec::Vec};
use rustc_hash::FxHashMap;

pub type Identifier = String;
pub type Alias = String;

#[derive(Default, Debug, Clone, Copy, PartialEq)]
/// A position in the code.
pub struct Pos {
    /// Line.
    pub line: usize,
    /// Column.
    pub col: usize,
    /// Absolute position of line-col from start of file.
    pub seek: usize,
}

impl Pos {
    /// Build Pos from the start of a TokenPos
    pub fn start(value: TokenPos) -> Self {
        Self { 
            line: value.line,
            col: value.start_col,
            seek: value.seek,
        }
    }

    /// Build Pos from the end of a TokenPos
    pub fn end(value: TokenPos) -> Self {
        Self { 
            line: value.line,
            col: value.end_col,
            seek: (value.end_col - value.start_col) + value.seek,
        }
    }

    pub fn to_tokenpos(&self) -> TokenPos {
        TokenPos::new(self.line, self.col, self.col, self.seek)
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
/// A position range in the code (start-end).
pub struct RangePos {
    /// Start position.
    pub start: Pos,
    /// End position.
    pub end: Pos,
}

impl RangePos {
    /// Create ne range position.
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }
}

impl From<&Expression> for RangePos {
    fn from(value: &Expression) -> Self {
        Self::new(Pos::start(value.start_pos), Pos::end(value.end_pos))
    }
}

impl From<&Command> for RangePos {
    fn from(value: &Command) -> Self {
        Self::new(Pos::start(value.start_pos), Pos::end(value.end_pos))
    }
}

impl From<&Token> for RangePos {
    fn from(value: &Token) -> Self {
        Self::new(Pos::start(value.pos), Pos::end(value.pos))
    }
}

impl From<&[AstNode]> for RangePos {
    fn from(value: &[AstNode]) -> Self {
        if value.len() > 0 {
            Self::new(
                Pos::start(value.first().unwrap().start_pos()),
                Pos::end(value.last().unwrap().end_pos())
            )
        } else {
            Self::default()
        }
    }
}

/// Table of symbols present in the AST.
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub symbols: FxHashMap<Identifier, Symbol>,
}

impl SymbolTable {
    pub fn id_exists(&self, identifier: &str) -> bool {
        self.symbols.contains_key(identifier)
    }

    pub fn id_is_model(&self, identifier: &str) -> bool {
        match self.symbols.get(identifier) {
            Some(Symbol { kind: SymbolKind::Model { .. }, .. }) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
/// Symbol.
pub struct Symbol {
    /// Symbol kind.
    pub kind: SymbolKind,
    /// Symbol position in the code.
    pub pos: RangePos,
}

impl Symbol {
    pub fn new(kind: SymbolKind, pos: RangePos) -> Self {
        Self { kind, pos }
    }
}

/// Symbol information and metadata.
#[derive(Debug)]
pub enum SymbolKind {
    Model {
        /// Model body.
        body: ModelBody,
        /// Model body position in the code.
        body_pos: RangePos,
    },
    //TODO: transfers, with environments and scopes for variables (let symbols)
    Transfer,
    //TODO: pipe
    Pipe,
    Const {
        /// Literal value.
        value_type: LiteralType,
        /// Literal value position in the code.
        value_pos: RangePos,
    },
}

/// Types of literals. Used by const symbols.
#[derive(Debug)]
pub enum LiteralType {
    String,
    Integer,
    Float,
    Boolean,
    None,
    Null,
}

/// Model content.
#[derive(Debug)]
pub enum ModelBody {
    Alias(Type),
    Struct(FxHashMap<Identifier, (Option<Alias>, Type)>),
}

/// Representation of a type.
#[derive(Debug, Default)]
pub struct Type {
    /// Type name.
    pub name: String,
    /// Subtypes.
    pub subtypes: Vec<Type>,
    /// Type position in the code.
    pub pos: RangePos,
}