//! # Abstract Syntax Tree
//!
//! Types and method to construct and store an AST. Composed of four elements:
//!
//! - [Token](crate::lexer::Token): A token in the source code, like a "(" or a variable name.
//! - [Expression](crate::ast::Expression): An expression, like an if-else block, or a math operation.
//! - [Command]: A command, like a model, or a transfer.
//! - [AstNode](crate::ast::AstNode): An AST is essentially a group of linked AST nodes. This type encapsulates the other three.

use alloc::{boxed::Box, vec::Vec};

use crate::lexer::{Token, TokenPos};

#[derive(Debug, PartialEq)]
/// AST node type. It can contain tokens, expressions, commands or vectors of other nodes.
pub enum AstNode {
    /// Token node.
    Token(Token),
    //TODO: Command/Statement variant
    /// Expression node.
    Expression(Box<Expression>),
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
    /// Expression starting position.
    pub start_pos: TokenPos,
    /// Expression ending position.
    pub end_pos: TokenPos,
}

impl Expression {
    /// New If-Else expression.
    pub fn new_ifelse(
        cond: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
        start_pos: TokenPos,
    ) -> Self {
        let end_pos = else_expr.end_pos;
        Self {
            kind: ExpressionKind::IfElse {
                cond_expr: cond.into(),
                if_expr: if_expr.into(),
                else_expr: else_expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    /// New Let expression.
    pub fn new_let(ident: Token, expr: Box<Expression>, start_pos: TokenPos) -> Self {
        let end_pos = expr.end_pos;
        Self {
            kind: ExpressionKind::Let {
                ident_token: ident.into(),
                expr: expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    /// New Binary expression.
    pub fn new_binary(op: Token, left_expr: Box<Expression>, right_expr: Box<Expression>) -> Self {
        let start_pos = left_expr.start_pos;
        let end_pos = right_expr.end_pos;
        Self {
            kind: ExpressionKind::Binary {
                op_token: op.into(),
                left_expr: left_expr.into(),
                right_expr: right_expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    /// New Unary expression.
    pub fn new_unary(op: Token, expr: Box<Expression>) -> Self {
        let start_pos = op.pos;
        let end_pos = expr.end_pos;
        Self {
            kind: ExpressionKind::Unary {
                op_token: op.into(),
                expr: expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    /// New Chain expression.
    pub fn new_chain(chain: Vec<AstNode>) -> Self {
        let start_pos = chain.first().unwrap().expr_ref().unwrap().start_pos;
        let end_pos = chain.last().unwrap().expr_ref().unwrap().end_pos;
        Self {
            kind: ExpressionKind::Chain {
                expr_vec: AstNode::Vec(chain),
            },
            start_pos,
            end_pos,
        }
    }

    /// New Group expression.
    pub fn new_group(expr: Box<Expression>, start_pos: TokenPos, end_pos: TokenPos) -> Self {
        Self {
            kind: ExpressionKind::Group {
                expr: AstNode::Expression(expr),
            },
            start_pos,
            end_pos,
        }
    }

    /// New Primary expression.
    pub fn new_primary(token: Token) -> Self {
        let start_pos = token.pos;
        let end_pos = token.pos;
        Self {
            kind: ExpressionKind::Primary {
                expr: AstNode::Token(token),
            },
            start_pos,
            end_pos,
        }
    }
}

#[derive(Debug, PartialEq)]
/// Expression kind.
pub enum ExpressionKind {
    /// Primary expression.
    Primary { expr: AstNode },
    /// Chain expression.
    Chain { expr_vec: AstNode },
    /// Group expression.
    Group { expr: AstNode },
    /// If-Else expression.
    IfElse {
        cond_expr: AstNode,
        if_expr: AstNode,
        else_expr: AstNode,
    },
    /// Binary expression.
    Binary {
        op_token: AstNode,
        left_expr: AstNode,
        right_expr: AstNode,
    },
    /// Unary expression.
    Unary { op_token: AstNode, expr: AstNode },
    /// Let expression.
    Let { ident_token: AstNode, expr: AstNode },
}
