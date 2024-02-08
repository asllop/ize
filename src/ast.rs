//! # Abstract Syntax Tree
//!
//! Types and method to construct and store an AST. Composed of four elements:
//!
//! - [Token](crate::lexer::Token): A token in the source code, like a "(" or a variable name.
//! - [Expression](crate::ast::Expression): An expression, like an if-else block, or a math operation.
//! - [Command](crate::ast::Command): A command, like a model, or a transfer.
//! - [AstNode](crate::ast::AstNode): An AST is essentially a group of linked AST nodes. This type encapsulates the other three.

use alloc::{boxed::Box, string::String, vec::Vec};

use crate::{
    err::IzeErr,
    lexer::{Token, TokenKind},
    pos::{Pos, RangePos},
};

#[derive(Debug, PartialEq)]
/// AST node type. It can contain tokens, expressions, commands or vectors of other nodes.
pub enum AstNode {
    /// Token node.
    Token(Token),
    /// Expression node. TODO: unbox
    Expression(Box<Expression>),
    /// Command node. TODO: unbox
    Command(Box<Command>),
    /// Vector of nodes.
    Vec(Vec<AstNode>),
}

impl AstNode {
    /// Convert node into a token variant.
    pub fn token(self) -> Option<Token> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    /// Convert node into an expression variant.
    pub fn expr(self) -> Option<Box<Expression>> {
        if let Self::Expression(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

    /// Convert node into a vector variant.
    pub fn vec(self) -> Option<Vec<AstNode>> {
        if let Self::Vec(vec) = self {
            Some(vec)
        } else {
            None
        }
    }

    /// Convert node into a token variant ref.
    pub fn token_ref(&self) -> Option<&Token> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    /// Convert node into an expression variant ref.
    pub fn expr_ref(&self) -> Option<&Box<Expression>> {
        if let Self::Expression(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

    /// Convert node into a vector variant ref.
    pub fn vec_ref(&self) -> Option<&Vec<AstNode>> {
        if let Self::Vec(vec) = self {
            Some(vec)
        } else {
            None
        }
    }

    /// Starting position of node.
    pub fn start_pos(&self) -> Pos {
        match self {
            AstNode::Token(t) => t.pos.start,
            AstNode::Expression(e) => e.pos.start,
            AstNode::Command(c) => c.pos.start,
            AstNode::Vec(v) => {
                if v.len() > 0 {
                    v[0].start_pos()
                } else {
                    Default::default()
                }
            }
        }
    }

    /// Ending position of node.
    pub fn end_pos(&self) -> Pos {
        match self {
            AstNode::Token(t) => t.pos.start,
            AstNode::Expression(e) => e.pos.end,
            AstNode::Command(c) => c.pos.end,
            AstNode::Vec(v) => {
                if v.len() > 0 {
                    v[v.len() - 1].end_pos()
                } else {
                    Default::default()
                }
            }
        }
    }
}

impl From<Token> for AstNode {
    fn from(value: Token) -> Self {
        Self::Token(value)
    }
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        Self::Expression(Box::new(value))
    }
}

impl From<Box<Expression>> for AstNode {
    fn from(value: Box<Expression>) -> Self {
        Self::Expression(value)
    }
}

impl From<Command> for AstNode {
    fn from(value: Command) -> Self {
        Self::Command(Box::new(value))
    }
}

impl From<Vec<AstNode>> for AstNode {
    fn from(value: Vec<AstNode>) -> Self {
        Self::Vec(value)
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
        cond_expr: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
        start_pos: Pos,
    ) -> Self {
        let end_pos = else_expr.pos.end;
        Self {
            kind: ExpressionKind::IfElse {
                cond_expr,
                if_expr,
                else_expr,
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Let expression.
    pub fn new_let(ident: String, expr: Box<Expression>, start_pos: Pos) -> Self {
        let end_pos = expr.pos.end;
        Self {
            kind: ExpressionKind::Let { ident, expr },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Select/Unwrap expression.
    pub fn new_select_unwrap(
        op: Token,
        expr: Box<Expression>,
        alias_token: Option<Token>,
        arms: Vec<AstNode>,
        end_pos: Pos,
    ) -> Self {
        let start_pos = op.pos.start;
        Self {
            kind: ExpressionKind::SelectUnwrap {
                op_token: op.into(),
                expr: expr.into(),
                alias_token: if let Some(alias) = alias_token {
                    Some(alias.into())
                } else {
                    None
                },
                arms_vec: arms.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Arm expression.
    pub fn new_arm(left_expr: Box<Expression>, right_expr: Box<Expression>) -> Self {
        let start_pos = left_expr.pos.start;
        let end_pos = right_expr.pos.end;
        Self {
            kind: ExpressionKind::Arm {
                left_expr: left_expr.into(),
                right_expr: right_expr.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Pair expression.
    pub fn new_pair(left_expr: Box<Expression>, right_expr: Box<Expression>) -> Self {
        let start_pos = left_expr.pos.start;
        let end_pos = right_expr.pos.end;
        Self {
            kind: ExpressionKind::Pair {
                left_expr: left_expr.into(),
                alias_token: None,
                right_expr: right_expr.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Pair expression with alias.
    pub fn new_pair_with_alias(
        left_expr: Box<Expression>,
        alias: Token,
        right_expr: Box<Expression>,
    ) -> Self {
        let start_pos = left_expr.pos.start;
        let end_pos = right_expr.pos.end;
        Self {
            kind: ExpressionKind::Pair {
                left_expr: left_expr.into(),
                alias_token: Some(alias.into()),
                right_expr: right_expr.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New Binary expression.
    pub fn new_binary(
        op: BinaryOp,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
    ) -> Self {
        let start_pos = left_expr.pos.start;
        let end_pos = right_expr.pos.end;

        Self {
            kind: ExpressionKind::Binary {
                op,
                left_expr,
                right_expr,
            },
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
    pub fn new_call(ident: String, args: Vec<Expression>, pos: RangePos) -> Self {
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
    pub fn new_type(ident: String, subtypes: Vec<Expression>, pos: RangePos) -> Self {
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
    pub fn new_pipe_body(body: Vec<AstNode>, pos: RangePos) -> Self {
        Self {
            kind: ExpressionKind::PipeBody {
                pipe_vec: AstNode::Vec(body),
            },
            pos,
        }
    }

    /// New Path expression.
    pub fn new_path(module_expr: Box<Expression>) -> Self {
        let pos = module_expr.pos;
        Self {
            kind: ExpressionKind::Path {
                module_expr: module_expr.into(),
                alias_token: None,
            },
            pos,
        }
    }

    /// New Path expression with alias.
    pub fn new_path_with_alias(module_expr: Box<Expression>, alias: Token) -> Self {
        let start_pos = module_expr.pos.start;
        let end_pos = alias.pos.end;
        Self {
            kind: ExpressionKind::Path {
                module_expr: module_expr.into(),
                alias_token: Some(alias.into()),
            },
            pos: RangePos::new(start_pos, end_pos),
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
        cond_expr: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    /// Binary expression.
    Binary {
        op: BinaryOp,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
    },
    /// Unary expression.
    Unary { op: UnaryOp, expr: Box<Expression> },
    /// Let expression.
    Let {
        ident: String,
        expr: Box<Expression>,
    },
    /// Call expression.
    Call {
        ident: String,
        args: Vec<Expression>,
    },
    /// Dot expression.
    Dot(Vec<Expression>),
    /// Type expression.
    Type {
        ident: String,
        subtypes: Vec<Expression>,
    },
    /// Select/Unwrap expression.
    SelectUnwrap {
        op_token: AstNode,
        expr: AstNode,
        alias_token: Option<AstNode>,
        arms_vec: AstNode,
    },
    /// Arm expression for Select/Unwrap.
    Arm {
        left_expr: AstNode,
        right_expr: AstNode,
    },
    /// Pair expression.
    Pair {
        left_expr: AstNode,
        alias_token: Option<AstNode>,
        right_expr: AstNode,
    },
    /// Pipe body expression. Only used by commands Run and Pipe.
    PipeBody {
        /// Vector of expressions.
        pipe_vec: AstNode,
    },
    /// Path expression. Only used by the import command.
    Path {
        /// Module path, either a Dot or a Primary (identifier) expression.
        module_expr: AstNode,
        /// Module alias.
        alias_token: Option<AstNode>,
    },
}

/// Primary expression.
#[derive(Debug, PartialEq)]
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

/// Literal.
#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    None,
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
/// Command type.
pub struct Command {
    /// Command kind.
    pub kind: CommandKind,
    /// Command position.
    pub pos: RangePos,
}

impl Command {
    /// New transfer command.
    pub fn new_transfer(
        ident: Token,
        params: Vec<AstNode>,
        ret_type: Box<Expression>,
        body: AstNode,
        pos: RangePos,
    ) -> Self {
        Self {
            kind: CommandKind::Transfer {
                ident_token: ident.into(),
                param_vec: params.into(),
                return_type: ret_type.into(),
                body,
            },
            pos,
        }
    }

    /// New model command.
    pub fn new_model(ident: Token, body: AstNode, pos: RangePos) -> Self {
        Self {
            kind: CommandKind::Model {
                ident_token: ident.into(),
                body,
            },
            pos,
        }
    }

    /// New const command.
    pub fn new_const(ident: Token, value: Token, start_pos: Pos) -> Self {
        let end_pos = value.pos.end;
        Self {
            kind: CommandKind::Const {
                ident_token: ident.into(),
                value_token: value.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New pipe command.
    pub fn new_pipe(ident: Token, pipe_body: Box<Expression>, start_pos: Pos) -> Self {
        let end_pos = pipe_body.pos.end;
        Self {
            kind: CommandKind::Pipe {
                ident_token: ident.into(),
                pipe_body_expr: pipe_body.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New run command.
    pub fn new_run_with_body(pipe_body: Box<Expression>, start_pos: Pos) -> Self {
        let end_pos = pipe_body.pos.end;
        Self {
            kind: CommandKind::Run {
                pipe: pipe_body.into(),
            },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New run command.
    pub fn new_run_with_ident(ident: Token, start_pos: Pos) -> Self {
        let end_pos = ident.pos.end;
        Self {
            kind: CommandKind::Run { pipe: ident.into() },
            pos: RangePos::new(start_pos, end_pos),
        }
    }

    /// New import command.
    pub fn new_import(path_vec: Vec<AstNode>, pos: RangePos) -> Self {
        Self {
            kind: CommandKind::Import {
                path_vec: path_vec.into(),
            },
            pos,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Expression kind.
pub enum CommandKind {
    /// Import command.
    Import {
        /// Vector of Path expressions.
        path_vec: AstNode,
    },
    /// Transfer command.
    Transfer {
        /// Transfer name.
        ident_token: AstNode,
        /// Parameters, vector of Pair expressions.
        param_vec: AstNode,
        /// Return type.
        return_type: AstNode,
        // Transfer body. Either an expression or a vector of Pair expressions.
        body: AstNode,
    },
    /// Model command.
    Model {
        /// Model name.
        ident_token: AstNode,
        /// Model body. Eather an alias (type/primary expression) or a struct (vector of Pair expressions).
        body: AstNode,
    },
    /// Pipe command.
    Pipe {
        /// Pipe name
        ident_token: AstNode,
        /// Pipe body expression.
        pipe_body_expr: AstNode,
    },
    /// Run command.
    Run {
        /// Pipe. Either an identifier token or a pipe body expression.
        pipe: AstNode,
    },
    /// Const command
    Const {
        /// Const name.
        ident_token: AstNode,
        /// Value literal.
        value_token: AstNode,
    },
}
