//! # Abstract Syntax Tree
//!
//! Types and method to construct and store an AST.

use alloc::{boxed::Box, string::String, vec::Vec};
use rustc_hash::FxHashMap;

use crate::{
    err::IzeErr,
    lexer::{Token, TokenKind},
    pos::{Pos, RangePos},
};

#[derive(Debug)]
/// Abstract Syntax Tree. Code representation after parsing and import execution.
pub struct Ast {
    /// Source code file path.
    pub file_path: String,
    /// Parsed commands, without imports.
    pub commands: Vec<Command>,
    /// Imported modules and required symbols.
    pub imports: Vec<Ast>,
    /// Table of imported symbols.
    /// Link each imported symbol to a position in the "imports" vector, and a symbol rename.
    pub imported_symbols: FxHashMap<String, ImportRef>,
}

impl Ast {
    /// New Ast.
    pub fn new(
        file_path: String,
        commands: Vec<Command>,
        imports: Vec<Ast>,
        imported_symbols: FxHashMap<String, ImportRef>,
    ) -> Self {
        Self {
            file_path,
            commands,
            imports,
            imported_symbols,
        }
    }
}

#[derive(Debug)]
/// Import ref, used as value in the table of imported symbols. Links to the AST, rename symbol, and command position in the code.
pub struct ImportRef {
    /// Position in the AST (Ast.imports) vector.
    pub ast_index: usize,
    /// Symbol rename.
    pub rename: Option<String>,
    /// Import command position.
    pub pos: RangePos,
}

impl ImportRef {
    /// New ImportRef.
    pub fn new(ast_index: usize, rename: Option<String>, pos: RangePos) -> Self {
        Self {
            ast_index,
            rename,
            pos,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Expression type.
pub struct Expression {
    /// Expression kind.
    pub kind: ExpressionKind,
    /// Expression position.
    pub pos: RangePos,
}

impl Expression {
    /// New If-Else expression.
    pub fn new_ifelse(
        cond: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
        start_pos: Pos,
    ) -> Self {
        let end_pos = else_expr.pos.end;
        Self {
            kind: ExpressionKind::IfElse {
                cond,
                if_expr,
                else_expr,
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Let expression.
    pub fn new_let(ident: Identifier, expr: Box<Expression>, start_pos: Pos) -> Self {
        let end_pos = expr.pos.end;
        Self {
            kind: ExpressionKind::Let { ident, expr },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Select/Unwrap expression.
    pub fn new_select_unwrap(
        op: SelectUnwrapOp,
        expr: Box<Expression>,
        alias: Option<Identifier>,
        arms: Vec<Expression>,
        pos: RangePos,
    ) -> Self {
        Self {
            kind: ExpressionKind::SelectUnwrap {
                op,
                expr,
                alias,
                arms,
            },
            pos,
        }
    }

    /// New Arm expression.
    pub fn new_arm(left: Box<Expression>, right: Box<Expression>) -> Self {
        let start_pos = left.pos.start;
        let end_pos = right.pos.end;
        Self {
            kind: ExpressionKind::Arm { left, right },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Pair expression.
    pub fn new_pair(left: Box<Expression>, right: Box<Expression>) -> Self {
        let start_pos = left.pos.start;
        let end_pos = right.pos.end;
        Self {
            kind: ExpressionKind::Pair {
                left,
                alias: None,
                right,
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Pair expression with alias.
    pub fn new_pair_with_alias(
        left: Box<Expression>,
        alias: Identifier,
        right: Box<Expression>,
    ) -> Self {
        let start_pos = left.pos.start;
        let end_pos = right.pos.end;
        Self {
            kind: ExpressionKind::Pair {
                left,
                alias: Some(alias),
                right,
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Binary expression.
    pub fn new_binary(op: BinaryOp, left: Box<Expression>, right: Box<Expression>) -> Self {
        let start_pos = left.pos.start;
        let end_pos = right.pos.end;

        Self {
            kind: ExpressionKind::Binary { op, left, right },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Unary expression.
    pub fn new_unary(op: UnaryOp, expr: Box<Expression>, start_pos: Pos) -> Self {
        let end_pos = expr.pos.end;
        Self {
            kind: ExpressionKind::Unary { op, expr },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Chain expression.
    pub fn new_chain(chain: Vec<Expression>) -> Self {
        let start_pos = chain.first().unwrap().pos.start;
        let end_pos = chain.last().unwrap().pos.end;
        Self {
            kind: ExpressionKind::Chain(chain),
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Group expression.
    pub fn new_group(expr: Box<Expression>, pos: RangePos) -> Self {
        Self {
            kind: ExpressionKind::Group(expr),
            pos,
        }
    }

    /// New Group expression.
    pub fn new_call(ident: Identifier, args: Vec<Expression>, pos: RangePos) -> Self {
        Self {
            kind: ExpressionKind::Call { ident, args },
            pos,
        }
    }

    /// New Dot expression.
    pub fn new_dot(dots: Vec<Expression>) -> Self {
        let start_pos = dots.first().unwrap().pos.start;
        let end_pos = dots.last().unwrap().pos.end;
        Self {
            kind: ExpressionKind::Dot(dots),
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Type expression.
    pub fn new_type(ident: Identifier, subtypes: Vec<Expression>, pos: RangePos) -> Self {
        Self {
            kind: ExpressionKind::Type { ident, subtypes },
            pos,
        }
    }

    /// New Primary with int literal.
    pub fn new_primary(primary: Primary, pos: RangePos) -> Self {
        Self {
            kind: ExpressionKind::Primary(primary),
            pos,
        }
    }

    /// New PipeBody expression.
    pub fn new_pipe_body(body: Vec<Expression>, pos: RangePos) -> Self {
        Self {
            kind: ExpressionKind::PipeBody(body),
            pos,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Expression kind.
pub enum ExpressionKind {
    /// Primary expression (literals and identifiers).
    Primary(Primary),
    /// Chain expression.
    Chain(Vec<Expression>),
    /// Group expression.
    Group(Box<Expression>),
    /// If-Else expression.
    IfElse {
        cond: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    /// Binary expression.
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Unary expression.
    Unary { op: UnaryOp, expr: Box<Expression> },
    /// Let expression.
    Let {
        ident: Identifier,
        expr: Box<Expression>,
    },
    /// Call expression.
    Call {
        ident: Identifier,
        args: Vec<Expression>,
    },
    /// Dot expression.
    Dot(Vec<Expression>),
    /// Type expression.
    Type {
        ident: Identifier,
        /// Only expressions with kind == ExpressionKind::Type.
        subtypes: Vec<Expression>,
    },
    /// Select/Unwrap expression.
    SelectUnwrap {
        op: SelectUnwrapOp,
        expr: Box<Expression>,
        alias: Option<Identifier>,
        /// Only expressions with kind == ExpressionKind::Arm.
        arms: Vec<Expression>,
    },
    /// Arm expression for Select/Unwrap.
    Arm {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Pair expression.
    Pair {
        left: Box<Expression>,
        /// Identifier here is a string literal, not a primary identifier. Only used for models.
        alias: Option<Identifier>,
        right: Box<Expression>,
    },
    /// Pipe body expression. Only used by commands Run and Pipe.
    PipeBody(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
/// Identifier. Usually a primary-identifier but can also be used to represent a string literal.
pub struct Identifier {
    /// Identifier string.
    pub id: String,
    /// Identifier position.
    pub pos: RangePos,
}

impl Identifier {
    /// New Identifier.
    pub fn new(id: String, pos: RangePos) -> Self {
        Self { id, pos }
    }
}

impl TryFrom<Token> for Identifier {
    type Error = IzeErr;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        if let TokenKind::Identifier(id) = value.kind {
            Ok(Identifier { id, pos: value.pos })
        } else {
            Err(IzeErr::new("Token must be an identifier".into(), value.pos))
        }
    }
}

#[derive(Debug, PartialEq)]
/// Primary expression.
pub enum Primary {
    /// Identifier primary.
    Identifier(String),
    /// Literal primary.
    Literal(Literal),
}

impl TryFrom<Token> for Primary {
    type Error = IzeErr;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::IntegerLiteral(i) => Ok(Primary::Literal(Literal::Integer(i))),
            TokenKind::FloatLiteral(f) => Ok(Primary::Literal(Literal::Float(f))),
            TokenKind::BooleanLiteral(b) => Ok(Primary::Literal(Literal::Boolean(b))),
            TokenKind::NoneLiteral => Ok(Primary::Literal(Literal::None)),
            TokenKind::NullLiteral => Ok(Primary::Literal(Literal::Null)),
            TokenKind::StringLiteral(s) => Ok(Primary::Literal(Literal::String(s))),
            TokenKind::Identifier(id) => Ok(Primary::Identifier(id)),
            TokenKind::ThreeDots => Ok(Primary::Identifier("...".into())),
            _ => Err(IzeErr::new(
                "Token must be an identifier or a literal to build a primary expression".into(),
                value.pos,
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
/// Literal.
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    None,
}

impl TryFrom<Token> for Literal {
    type Error = IzeErr;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::IntegerLiteral(i) => Ok(Literal::Integer(i)),
            TokenKind::FloatLiteral(f) => Ok(Literal::Float(f)),
            TokenKind::BooleanLiteral(b) => Ok(Literal::Boolean(b)),
            TokenKind::NoneLiteral => Ok(Literal::None),
            TokenKind::NullLiteral => Ok(Literal::Null),
            TokenKind::StringLiteral(s) => Ok(Literal::String(s)),
            _ => Err(IzeErr::new(
                "Token must be a literal to build a Literal object".into(),
                value.pos,
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
/// Binary operation
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    LesserThan,
    GreaterThan,
    GtEqual,
    LtEqual,
    And,
    TwoAnds,
    Or,
    TwoOrs,
    TwoEquals,
    NotEqual,
}

impl TryFrom<Token> for BinaryOp {
    type Error = IzeErr;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Plus => Ok(BinaryOp::Plus),
            TokenKind::Minus => Ok(BinaryOp::Minus),
            TokenKind::Star => Ok(BinaryOp::Star),
            TokenKind::Slash => Ok(BinaryOp::Slash),
            TokenKind::Percent => Ok(BinaryOp::Percent),
            TokenKind::LesserThan => Ok(BinaryOp::LesserThan),
            TokenKind::GreaterThan => Ok(BinaryOp::GreaterThan),
            TokenKind::GtEqual => Ok(BinaryOp::GtEqual),
            TokenKind::LtEqual => Ok(BinaryOp::LtEqual),
            TokenKind::And => Ok(BinaryOp::And),
            TokenKind::TwoAnds => Ok(BinaryOp::TwoAnds),
            TokenKind::Or => Ok(BinaryOp::Or),
            TokenKind::TwoOrs => Ok(BinaryOp::TwoOrs),
            TokenKind::TwoEquals => Ok(BinaryOp::TwoEquals),
            TokenKind::NotEqual => Ok(BinaryOp::NotEqual),
            _ => Err(IzeErr::new(
                "Token must be a binary operator".into(),
                value.pos,
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
/// Unary operation
pub enum UnaryOp {
    Minus,
    Not,
}

impl TryFrom<Token> for UnaryOp {
    type Error = IzeErr;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Minus => Ok(UnaryOp::Minus),
            TokenKind::Not => Ok(UnaryOp::Not),
            _ => Err(IzeErr::new(
                "Token must be a unary operator".into(),
                value.pos,
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
/// Select/Unwrap operation
pub enum SelectUnwrapOp {
    Select,
    Unwrap,
}

impl TryFrom<Token> for SelectUnwrapOp {
    type Error = IzeErr;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Unwrap => Ok(SelectUnwrapOp::Unwrap),
            TokenKind::Select => Ok(SelectUnwrapOp::Select),
            _ => Err(IzeErr::new(
                "Token must be Select or Unwrap".into(),
                value.pos,
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
/// Command type.
pub struct Command {
    /// Command kind.
    pub kind: CommandKind,
    /// Command position.
    pub pos: RangePos,
}

impl Command {
    /// New transfer command with expression.
    pub fn new_transfer_with_expr(
        ident: Identifier,
        params: Vec<Expression>,
        return_type: Expression,
        body: Expression,
        pos: RangePos,
    ) -> Self {
        Self {
            kind: CommandKind::Transfer {
                ident,
                params,
                return_type,
                body: TransferBody::Expression(body),
            },
            pos,
        }
    }

    /// New transfer command with struct.
    pub fn new_transfer_with_struct(
        ident: Identifier,
        params: Vec<Expression>,
        return_type: Expression,
        body: Vec<Expression>,
        pos: RangePos,
    ) -> Self {
        Self {
            kind: CommandKind::Transfer {
                ident,
                params,
                return_type,
                body: TransferBody::Struct(body),
            },
            pos,
        }
    }

    /// New model command with type.
    pub fn new_model_with_type(ident: Identifier, body: Expression, pos: RangePos) -> Self {
        Self {
            kind: CommandKind::Model {
                ident,
                body: ModelBody::Type(body),
            },
            pos,
        }
    }

    /// New model command with struct.
    pub fn new_model_with_struct(ident: Identifier, body: Vec<Expression>, pos: RangePos) -> Self {
        Self {
            kind: CommandKind::Model {
                ident,
                body: ModelBody::Struct(body),
            },
            pos,
        }
    }

    /// New const command.
    pub fn new_const(ident: Identifier, value: Literal, pos: RangePos) -> Self {
        Self {
            kind: CommandKind::Const { ident, value },
            pos,
        }
    }

    /// New pipe command.
    pub fn new_pipe(ident: Identifier, pipe_body: Expression, start_pos: Pos) -> Self {
        let end_pos = pipe_body.pos.end;
        Self {
            kind: CommandKind::Pipe { ident, pipe_body },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New run command.
    pub fn new_run_with_body(pipe_body: Expression, start_pos: Pos) -> Self {
        let end_pos = pipe_body.pos.end;
        Self {
            kind: CommandKind::Run(RunBody::Pipe(pipe_body)),
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New run command.
    pub fn new_run_with_ident(ident: Identifier, start_pos: Pos) -> Self {
        let end_pos = ident.pos.end;
        Self {
            kind: CommandKind::Run(RunBody::Identifier(ident)),
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New import command.
    pub fn new_import(symbols: Vec<ImportSymbol>, path: Expression, pos: RangePos) -> Self {
        Self {
            kind: CommandKind::Import { symbols, path },
            pos,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Expression kind.
pub enum CommandKind {
    /// Import command.
    Import {
        /// Vector of imported symbols with an optional rename for each.
        symbols: Vec<ImportSymbol>,
        /// Import path. Must be an expression with kind == ExpressionKind::Dot.
        path: Expression,
    },
    /// Transfer command.
    Transfer {
        /// Transfer name.
        ident: Identifier,
        /// Parameters. Only expressions with kind == ExpressionKind::Pair
        params: Vec<Expression>,
        /// Return type. Must be an expression with kind == ExpressionKind::Type.
        return_type: Expression,
        // Transfer body.
        body: TransferBody,
    },
    /// Model command.
    Model {
        /// Model name.
        ident: Identifier,
        /// Model body.
        body: ModelBody,
    },
    /// Pipe command.
    Pipe {
        /// Pipe name
        ident: Identifier,
        /// Pipe body expression. Must be an expression with kind == ExpressionKind::PipeBody.
        pipe_body: Expression,
    },
    /// Run command.
    Run(RunBody),
    /// Const command
    Const {
        /// Const name.
        ident: Identifier,
        /// Value literal.
        value: Literal,
    },
}

#[derive(Debug, PartialEq)]
/// Imported symbol.
pub struct ImportSymbol {
    /// Imported symbol.
    pub symbol: Identifier,
    /// Symbol rename.
    pub rename: Option<Identifier>,
}

impl ImportSymbol {
    /// New imported symbol.
    pub fn new(symbol: Identifier, rename: Option<Identifier>) -> Self {
        Self { symbol, rename }
    }
}

#[derive(Debug, PartialEq)]
/// Transfer body.
pub enum TransferBody {
    /// Any expression.
    Expression(Expression),
    /// Vector of Pair expressions.
    Struct(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
/// Model body.
pub enum ModelBody {
    /// Type expression.
    Type(Expression),
    /// Vector of Pair expressions.
    Struct(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
/// Run body.
pub enum RunBody {
    /// Pipe identifier.
    Identifier(Identifier),
    /// Pipe body. Must be an expression with kind == ExpressionKind::PipeBody.
    Pipe(Expression),
}
