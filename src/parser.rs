//! # IZE Parser
//!
//! This module contains all the types and methods necessary to parse commands and expressions and generate an AST.

use crate::{
    ast::{BinaryOp, Command, Expr, ExprSet, Literal, UnaryOp},
    lexer::{Lexeme, Token, TokenKind},
    IzeErr, Pos,
};
use alloc::{boxed::Box, collections::VecDeque, string::String, vec::Vec};

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

    fn ifelse_expr(&mut self) -> Result<Expr, IzeErr> {
        let expr = Self::next_expr(ExprType::IfElse)(self)?;
        if self.is_token(TokenKind::If, 0) {
            let (_, if_pos) = self.consume_token().into_particle()?; // consume "if?"
            let then_expr = self.ifelse_expr()?;
            if self.is_token(TokenKind::Else, 0) {
                self.consume_token().into_particle()?; // consume "else?"
                let else_expr = self.ifelse_expr()?;
                Ok(Expr::new(
                    ExprSet::IfElse {
                        condition: Box::new(expr),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    },
                    if_pos,
                ))
            } else {
                Err(IzeErr {
                    message: "If without else".into(),
                    pos: if_pos,
                })
            }
        } else {
            Ok(expr)
        }
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

    fn logic_expr(&mut self) -> Result<Expr, IzeErr> {
        self.binary_expr(&[TokenKind::And, TokenKind::Or], ExprType::Logic)
    }

    fn term_expr(&mut self) -> Result<Expr, IzeErr> {
        self.binary_expr(&[TokenKind::Plus, TokenKind::Minus], ExprType::Term)
    }

    fn factor_expr(&mut self) -> Result<Expr, IzeErr> {
        self.binary_expr(
            &[TokenKind::Star, TokenKind::Slash, TokenKind::Percent],
            ExprType::Factor,
        )
    }

    fn unary_expr(&mut self) -> Result<Expr, IzeErr> {
        if self.is_token(TokenKind::Not, 0) || self.is_token(TokenKind::Minus, 0) {
            let (op, op_pos) = self.consume_token().into_particle()?;
            let op = match op {
                TokenKind::Not => Ok(UnaryOp::Negate),
                TokenKind::Minus => Ok(UnaryOp::Minus),
                _ => Err(IzeErr {
                    message: "Token is not a unary operator".into(),
                    pos: op_pos,
                }),
            }?;
            let right = self.unary_expr()?;
            let pos = right.pos.clone();
            return Ok(Expr::new(
                ExprSet::Unary {
                    op,
                    expr: Box::new(right),
                },
                pos,
            ));
        }
        Self::next_expr(ExprType::Unary)(self)
    }

    fn group_expr(&mut self) -> Result<Expr, IzeErr> {
        if self.is_token(TokenKind::OpenParenth, 0) {
            self.consume_token().into_particle()?; // consume "("
            let expr = self.expression()?;
            if !self.is_token(TokenKind::ClosingParenth, 0) {
                return Err(IzeErr {
                    message: "Group without closing parenthesis".into(),
                    pos: expr.pos,
                });
            }
            self.consume_token().into_particle()?; // consume ")"
            let pos = expr.pos.clone();
            return Ok(Expr::new(
                ExprSet::Group {
                    expr: Box::new(expr),
                },
                pos,
            ));
        } else {
            Self::next_expr(ExprType::Group)(self)
        }
    }

    fn primary_expr(&mut self) -> Result<Expr, IzeErr> {
        // Literal
        if self.is_literal(0)? {
            let (lit, pos) = self.consume_token().into_literal()?;
            let expr = Expr::new(ExprSet::Literal(lit), pos);
            return Ok(expr);
        }
        // Identifier
        if self.is_token(TokenKind::Ident, 0) {
            let (id, pos) = self.consume_token().into_ident()?;
            let expr = Expr::new(ExprSet::Identifier(id), pos);
            return Ok(expr);
        }

        // If we are here, something is badly formed
        if let Some(next_token) = self.consume_token() {
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
            ExprType::Expr => Self::ifelse_expr,
            ExprType::Select => todo!(),
            ExprType::Unwrap => todo!(),
            ExprType::IfElse => Self::equality_expr,
            ExprType::Chain => todo!(),
            ExprType::Equality => Self::comparison_expr,
            ExprType::Comparison => Self::logic_expr,
            ExprType::Logic => Self::term_expr,
            ExprType::Term => Self::factor_expr,
            ExprType::Factor => Self::unary_expr,
            ExprType::Unary => Self::group_expr,
            ExprType::Dot => todo!(),
            ExprType::Call => todo!(),
            ExprType::Let => todo!(),
            ExprType::Group => Self::primary_expr,
        }
    }

    fn binary_expr(
        &mut self,
        op_tokens: &[TokenKind],
        curr_expr: ExprType,
    ) -> Result<Expr, IzeErr> {
        let mut expr = Self::next_expr(curr_expr)(self)?;
        while self.check_tokens(op_tokens, 0)? {
            let (op, op_pos) = self.consume_token().into_particle()?;
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

    fn consume_token(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    /// Check for a particular token.
    fn is_token(&mut self, token_type: TokenKind, offset: usize) -> bool {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            match token.lexeme {
                Lexeme::Float(_) => token_type == TokenKind::FloatLiteral,
                Lexeme::Int(_) => token_type == TokenKind::IntegerLiteral,
                Lexeme::Bool(_) => token_type == TokenKind::BooleanLiteral,
                Lexeme::String(_) => token_type == TokenKind::StringLiteral,
                Lexeme::Ident(_) => token_type == TokenKind::Ident,
                Lexeme::Particle(tt) => token_type == tt,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Check for a list of tokens.
    fn check_tokens(&mut self, token_types: &[TokenKind], offset: usize) -> Result<bool, IzeErr> {
        // Check if token exist at the specified offset
        for t in token_types {
            if self.is_token(t.clone(), offset) {
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
