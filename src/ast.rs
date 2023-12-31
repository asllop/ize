use alloc::{boxed::Box, vec::Vec};

use crate::{common::TokenPos, lexer::Token};

#[derive(Debug)]
pub struct Expression {
    pub expr: Expr,
    pub start_pos: TokenPos,
    pub end_pos: TokenPos,
}

impl Expression {
    pub fn new_ifelse(
        cond: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
        start_pos: TokenPos,
    ) -> Self {
        let end_pos = else_expr.end_pos;
        Self {
            expr: Expr::IfElse {
                cond_expr: cond.into(),
                if_expr: if_expr.into(),
                else_expr: else_expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    pub fn new_let(ident: Token, expr: Box<Expression>, start_pos: TokenPos) -> Self {
        let end_pos = expr.end_pos;
        Self {
            expr: Expr::Let {
                ident_token: ident.into(),
                expr: expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    pub fn new_binary(op: Token, left_expr: Box<Expression>, right_expr: Box<Expression>) -> Self {
        let start_pos = left_expr.start_pos;
        let end_pos = right_expr.end_pos;
        Self {
            expr: Expr::Binary {
                op_token: op.into(),
                left_expr: left_expr.into(),
                right_expr: right_expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    pub fn new_chain(chain: Vec<AstNode>) -> Self {
        let start_pos = chain.first().unwrap().expr_ref().unwrap().start_pos;
        let end_pos = chain.last().unwrap().expr_ref().unwrap().end_pos;
        Self {
            expr: Expr::Chain {
                expr_vec: AstNode::Vec(chain),
            },
            start_pos,
            end_pos,
        }
    }

    pub fn new_group(expr: Box<Expression>, start_pos: TokenPos, end_pos: TokenPos) -> Self {
        Self {
            expr: Expr::Group {
                expr: AstNode::Expression(expr),
            },
            start_pos,
            end_pos,
        }
    }

    pub fn new_primary(token: Token) -> Self {
        let start_pos = token.pos;
        let end_pos = token.pos;
        Self {
            expr: Expr::Primary {
                expr: AstNode::Token(token),
            },
            start_pos,
            end_pos,
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Primary {
        expr: AstNode,
    },
    Chain {
        expr_vec: AstNode,
    },
    Group {
        expr: AstNode,
    },
    IfElse {
        cond_expr: AstNode,
        if_expr: AstNode,
        else_expr: AstNode,
    },
    Binary {
        op_token: AstNode,
        left_expr: AstNode,
        right_expr: AstNode,
    },
    Let {
        ident_token: AstNode,
        expr: AstNode,
    },
}

#[derive(Debug)]
// AST node type. It can contain tokens, expressions, commands or vectors of other nodes.
pub enum AstNode {
    Token(Token),
    //TODO: Command/Statement variant
    Expression(Box<Expression>),
    Vec(Vec<AstNode>),
}

impl AstNode {
    pub fn token(self) -> Option<Token> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn expr(self) -> Option<Box<Expression>> {
        if let Self::Expression(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

    pub fn vec(self) -> Option<Vec<AstNode>> {
        if let Self::Vec(vec) = self {
            Some(vec)
        } else {
            None
        }
    }

    pub fn token_ref(&self) -> Option<&Token> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn expr_ref(&self) -> Option<&Box<Expression>> {
        if let Self::Expression(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

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
