//! # IZE Parser
//!
//! This module contains all the types and methods necessary to parse commands and expressions and generate an AST.

//TODO: create a struct for expression precedence, so we can parse asking to recursively parse a higher precedence thing without
// having to hardcode the precedence in the parser.

use crate::{
    ast::{BinaryOp, Command, Expr, ExprSet, Literal},
    lexer::{Lexeme, Token, TokenKind},
    IzeErr, Pos,
};
use alloc::{boxed::Box, string::String};
use alloc::{collections::VecDeque, vec::Vec};

trait FromToken {
    fn into_parts(self) -> Result<(Lexeme, Pos), IzeErr>;
    fn into_particle(self) -> Result<(TokenKind, Pos), IzeErr>;
    fn into_ident(self) -> Result<(String, Pos), IzeErr>;
    fn into_literal(self) -> Result<(Literal, Pos), IzeErr>;
}

impl FromToken for Option<Token> {
    fn into_parts(self) -> Result<(Lexeme, Pos), IzeErr> {
        if let Some(token) = self {
            Ok((token.lexeme, token.pos))
        } else {
            Err(IzeErr {
                message: "Token is None".into(),
                pos: Pos::default(),
            })
        }
    }

    fn into_particle(self) -> Result<(TokenKind, Pos), IzeErr> {
        let (lexeme, pos) = self.into_parts()?;
        if let Lexeme::Particle(t) = lexeme {
            Ok((t, pos))
        } else {
            Err(IzeErr {
                message: "Expected a particle".into(),
                pos: Pos::default(),
            })
        }
    }

    fn into_ident(self) -> Result<(String, Pos), IzeErr> {
        let (lexeme, pos) = self.into_parts()?;
        if let Lexeme::Ident(s) = lexeme {
            Ok((s, pos))
        } else {
            Err(IzeErr {
                message: "Expected an identifier".into(),
                pos: Pos::default(),
            })
        }
    }

    fn into_literal(self) -> Result<(Literal, Pos), IzeErr> {
        let (lexeme, pos) = self.into_parts()?;
        match lexeme {
            Lexeme::Float(f) => Ok((Literal::Float(f), pos)),
            Lexeme::Int(i) => Ok((Literal::Integer(i), pos)),
            Lexeme::Bool(b) => Ok((Literal::Boolean(b), pos)),
            Lexeme::String(s) => Ok((Literal::String(s), pos)),
            Lexeme::Particle(TokenKind::NullLiteral) => Ok((Literal::Null, pos)),
            Lexeme::Particle(TokenKind::NoneLiteral) => Ok((Literal::None, pos)),
            _ => Err(IzeErr {
                message: "Expected a literal".into(),
                pos: Pos::default(),
            }),
        }
    }
}

#[derive(Clone, Copy)]
enum ExprType {
    Expr,
    Select,
    Unwrap,
    IfElse,
    Chain,
    Equality,
    Comparison,
    Logic,
    Term,
    Factor,
    Unary,
    Dot,
    Call,
    Let,
    Group,
    Primary,
}

pub struct Parser {
    pub tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: VecDeque::from(tokens),
        }
    }

    /// Parse program.
    pub fn parse(&mut self) -> Result<Vec<Command>, IzeErr> {
        //TODO: parse all commands and return a vector
        todo!()
    }

    pub fn ended(&self) -> bool {
        self.tokens.len() == 0
    }

    /// Parse a single expression.
    pub fn expression(&mut self) -> Result<Expr, IzeErr> {
        Self::next_expr(ExprType::Expr)(self)
    }

    fn equality_expr(&mut self) -> Result<Expr, IzeErr> {
        self.binary_expr(
            &[TokenKind::TwoEquals, TokenKind::NotEqual],
            ExprType::Equality,
        )
    }

    fn comparison_expr(&mut self) -> Result<Expr, IzeErr> {
        self.binary_expr(
            &[
                TokenKind::GreaterThan,
                TokenKind::LesserThan,
                TokenKind::GtEqual,
                TokenKind::LtEqual,
                TokenKind::TwoAnds,
                TokenKind::TwoOrs,
            ],
            ExprType::Comparison,
        )
    }

    fn primary(&mut self) -> Result<Expr, IzeErr> {
        // Number literal
        if self.is_literal(0)? {
            let (lit, pos) = self.token().into_literal()?;
            let expr = Expr::new(ExprSet::Literal(lit), pos);
            return Ok(expr);
        }
        // Identifier
        if self.is_token(TokenKind::Ident, 0)? {
            let (id, pos) = self.token().into_ident()?;
            let expr = Expr::new(ExprSet::Identifier(id), pos);
            return Ok(expr);
        }

        //TODO: parse group

        // If we are here, something is badly formed
        if let Some(next_token) = self.token() {
            //TODO: check the next token and see if we can provide a more specific error message
            Err(IzeErr {
                message: format!(
                    "Couldn't parse a valid expression. Last token: {:?}",
                    next_token.lexeme
                )
                .into(),
                pos: next_token.pos,
            })
        } else {
            Err(IzeErr {
                message: "Couldn't parse a valid expression at end".into(),
                pos: Pos { row: 0, col: 0 },
            })
        }
    }

    /// Return the next expression parser in precedence order
    fn next_expr(curr_expr: ExprType) -> fn(&mut Parser) -> Result<Expr, IzeErr> {
        // From lower precedence (Expr) to higher precedence (Primary).
        match curr_expr {
            ExprType::Expr => Self::equality_expr,
            ExprType::Select => todo!(),
            ExprType::Unwrap => todo!(),
            ExprType::IfElse => todo!(),
            ExprType::Chain => todo!(),
            ExprType::Equality => Self::comparison_expr,
            ExprType::Comparison => Self::primary,
            ExprType::Logic => todo!(),
            ExprType::Term => todo!(),
            ExprType::Factor => todo!(),
            ExprType::Unary => todo!(),
            ExprType::Dot => todo!(),
            ExprType::Call => todo!(),
            ExprType::Let => todo!(),
            ExprType::Group => todo!(),
            ExprType::Primary => panic!("Primary parser has the highest precedence"),
        }
    }

    fn binary_expr(
        &mut self,
        op_tokens: &[TokenKind],
        curr_expr: ExprType,
    ) -> Result<Expr, IzeErr> {
        let mut expr = Self::next_expr(curr_expr)(self)?;
        while self.check_tokens(op_tokens, 0)? {
            let (op, op_pos) = self.token().into_particle()?;
            let op = match op {
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
                _ => Err(IzeErr {
                    message: "Token is not a binary operator".into(),
                    pos: op_pos,
                }),
            }?;
            let right = Self::next_expr(curr_expr)(self)?;
            let pos = expr.pos.clone();
            expr = Expr::new(
                ExprSet::Binary {
                    op,
                    left_expr: Box::new(expr),
                    right_expr: Box::new(right),
                },
                pos,
            )
        }
        Ok(expr)
    }

    fn token(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    /// Check for a particular token.
    fn is_token(&mut self, token_type: TokenKind, offset: usize) -> Result<bool, IzeErr> {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            Ok(match token.lexeme {
                Lexeme::Float(_) => token_type == TokenKind::FloatLiteral,
                Lexeme::Int(_) => token_type == TokenKind::IntegerLiteral,
                Lexeme::Bool(_) => token_type == TokenKind::BooleanLiteral,
                Lexeme::String(_) => token_type == TokenKind::StringLiteral,
                Lexeme::Ident(_) => token_type == TokenKind::Ident,
                Lexeme::Particle(tt) => token_type == tt,
                _ => false,
            })
        } else {
            Ok(false)
        }
    }

    /// Check for a list of tokens.
    fn check_tokens(&mut self, token_types: &[TokenKind], offset: usize) -> Result<bool, IzeErr> {
        // Check if token exist at the specified offset
        for t in token_types {
            if self.is_token(t.clone(), offset)? {
                return Ok(true);
            }
        }
        return Ok(false);
    }

    /// Check if token is a literal.
    fn is_literal(&mut self, offset: usize) -> Result<bool, IzeErr> {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            Ok(match token.lexeme {
                Lexeme::Float(_)
                | Lexeme::Int(_)
                | Lexeme::Bool(_)
                | Lexeme::String(_)
                | Lexeme::Particle(TokenKind::NullLiteral)
                | Lexeme::Particle(TokenKind::NoneLiteral) => true,
                _ => false,
            })
        } else {
            Ok(false)
        }
    }
}
