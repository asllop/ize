//! # IZE Parser
//!
//! This module contains all the types and methods necessary to parse commands and expressions and generate an AST.

use crate::{
    ast::{Arm, BinaryOp, Command, Expr, ExprSet, Literal, Type, TypeId, UnaryOp},
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
    Chain,
    Select,
    Unwrap,
    IfElse,
    Equality,
    Comparison,
    Logic,
    Term,
    Factor,
    Unary,
    Let,
    Dot,
    Call,
    Group,
}

/// Code parser.
pub struct Parser {
    pub tokens: VecDeque<Token>,
}

impl Parser {
    /// Create new parser from a vector of tokens.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: VecDeque::from(tokens),
        }
    }

    /// Parse the entire program.
    pub fn parse(&mut self) -> Result<Vec<Command>, IzeErr> {
        //TODO: parse all commands and return a vector
        todo!()
    }

    /// Check if parser ended processing tokens.
    pub fn ended(&self) -> bool {
        self.tokens.len() == 0
    }

    /// Parse a single expression.
    pub fn expression(&mut self) -> Result<Expr, IzeErr> {
        self.next_expr(ExprType::Expr)
    }

    fn select_expr(&mut self) -> Result<Expr, IzeErr> {
        if self.is_token(TokenKind::Select, 0) {
            let (pos, expr, alias, arms) = self.select_or_unwrap_expr()?;
            Ok(Expr {
                expr: ExprSet::Select {
                    expr: Box::new(expr),
                    alias,
                    arms,
                },
                pos,
            })
        } else {
            self.next_expr(ExprType::Select)
        }
    }

    fn unwrap_expr(&mut self) -> Result<Expr, IzeErr> {
        if self.is_token(TokenKind::Unwrap, 0) {
            let (pos, expr, alias, arms) = self.select_or_unwrap_expr()?;
            Ok(Expr {
                expr: ExprSet::Unwrap {
                    expr: Box::new(expr),
                    alias,
                    arms,
                },
                pos,
            })
        } else {
            self.next_expr(ExprType::Unwrap)
        }
    }

    fn chain_expr(&mut self) -> Result<Expr, IzeErr> {
        let mut expr = vec![self.next_expr(ExprType::Chain)?];

        while self.is_token(TokenKind::Semicolon, 0) {
            self.consume_token().into_particle()?; // consume ";"
            expr.push(self.next_expr(ExprType::Chain)?);
        }

        if expr.len() > 1 {
            let pos = expr[0].pos;
            Ok(Expr {
                expr: ExprSet::Chain { chain: expr },
                pos,
            })
        } else {
            Ok(expr.pop().unwrap())
        }
    }

    fn ifelse_expr(&mut self) -> Result<Expr, IzeErr> {
        let expr = self.next_expr(ExprType::IfElse)?;
        if self.is_token(TokenKind::If, 0) {
            let (_, if_pos) = self.consume_token().into_particle()?; // consume "if?"
            let then_expr = self.expression()?;
            if self.is_token(TokenKind::Else, 0) {
                self.consume_token().into_particle()?; // consume "else?"
                let else_expr = self.expression()?;
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
            let pos = right.pos;
            return Ok(Expr::new(
                ExprSet::Unary {
                    op,
                    expr: Box::new(right),
                },
                pos,
            ));
        }
        self.next_expr(ExprType::Unary)
    }

    fn let_expr(&mut self) -> Result<Expr, IzeErr> {
        if self.is_token(TokenKind::Let, 0) {
            let (_, let_pos) = self.consume_token().into_particle()?; // consume "let"
            if self.is_ident(0) {
                let (var_name, _) = self.consume_token().into_ident()?;
                // NOTE: we don't want let value to use chain expressions
                let expr = self.next_expr(ExprType::Chain)?;
                Ok(Expr::new(
                    ExprSet::Let {
                        name: var_name,
                        value: Box::new(expr),
                    },
                    let_pos,
                ))
            } else {
                Err(IzeErr {
                    message: "Let must be followed by an identifier".into(),
                    pos: let_pos,
                })
            }
        } else {
            self.next_expr(ExprType::Let)
        }
    }

    fn dot_expr(&mut self) -> Result<Expr, IzeErr> {
        let mut expr = vec![self.next_expr(ExprType::Dot)?];

        while self.is_token(TokenKind::Dot, 0) {
            self.consume_token().into_particle()?; // consume "."
            expr.push(self.next_expr(ExprType::Dot)?);
        }

        if expr.len() > 1 {
            let pos = expr[0].pos;
            Ok(Expr {
                expr: ExprSet::Dot { compos: expr },
                pos,
            })
        } else {
            Ok(expr.pop().unwrap())
        }
    }

    fn call_expr(&mut self) -> Result<Expr, IzeErr> {
        if self.is_token(TokenKind::Ident, 0) && self.is_token(TokenKind::OpenParenth, 1) {
            let (call_name, call_pos) = self.consume_token().into_ident()?;
            self.consume_token().into_particle()?; // consume "("
            if !self.is_token(TokenKind::ClosingParenth, 0) {
                let mut args: Vec<Expr> = Vec::new();
                loop {
                    let arg = self.expression()?;
                    let arg_pos = arg.pos;
                    args.push(arg);
                    if self.is_token(TokenKind::ClosingParenth, 0) {
                        self.consume_token().into_particle()?; // consume ")"
                        break;
                    }
                    if !self.is_token(TokenKind::Comma, 0) {
                        return Err(IzeErr {
                            message: "Expected a comma after function argument".into(),
                            pos: arg_pos,
                        });
                    }
                    self.consume_token().into_particle()?; // consume ","
                }
                Ok(Expr::new(
                    ExprSet::Call {
                        name: call_name,
                        args,
                    },
                    call_pos,
                ))
            } else {
                self.consume_token().into_particle()?; // consume ")"
                Ok(Expr::new(
                    ExprSet::Call {
                        name: call_name,
                        args: Default::default(),
                    },
                    call_pos,
                ))
            }
        } else {
            self.next_expr(ExprType::Call)
        }
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
            let pos = expr.pos;
            return Ok(Expr::new(
                ExprSet::Group {
                    expr: Box::new(expr),
                },
                pos,
            ));
        } else {
            self.next_expr(ExprType::Group)
        }
    }

    fn primary_expr(&mut self) -> Result<Expr, IzeErr> {
        // Literal
        if self.is_literal(0) {
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
        // Type
        if let Some(expr) = self.type_expr()? {
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

    /// Call the next expression parser in precedence order
    fn next_expr(&mut self, curr_expr: ExprType) -> Result<Expr, IzeErr> {
        // From lower precedence (Expr) to higher precedence (Primary).
        match curr_expr {
            ExprType::Expr => self.chain_expr(),
            ExprType::Chain => self.select_expr(),
            ExprType::Select => self.unwrap_expr(),
            ExprType::Unwrap => self.ifelse_expr(),
            ExprType::IfElse => self.equality_expr(),
            ExprType::Equality => self.comparison_expr(),
            ExprType::Comparison => self.logic_expr(),
            ExprType::Logic => self.term_expr(),
            ExprType::Term => self.factor_expr(),
            ExprType::Factor => self.unary_expr(),
            ExprType::Unary => self.let_expr(),
            ExprType::Let => self.dot_expr(),
            ExprType::Dot => self.call_expr(),
            ExprType::Call => self.group_expr(),
            ExprType::Group => self.primary_expr(),
        }
    }

    /// Parse any binary expression.
    fn binary_expr(
        &mut self,
        op_tokens: &[TokenKind],
        curr_expr: ExprType,
    ) -> Result<Expr, IzeErr> {
        let mut expr = self.next_expr(curr_expr)?;
        while self.check_tokens(op_tokens, 0) {
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
            let right = self.next_expr(curr_expr)?;
            let pos = expr.pos;
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

    /// Parse Unwrap or Select arms.
    fn arm_expr(&mut self) -> Result<Option<Arm>, IzeErr> {
        if self.is_token(TokenKind::ClosingParenth, 0) {
            return Ok(None);
        }

        let value = self.expression()?;
        if !self.is_token(TokenKind::Arrow, 0) {
            return Err(IzeErr {
                message: "Expected an arrow to structure the arm".into(),
                pos: value.pos,
            });
        }

        self.consume_token().into_particle()?; // Consume "->"

        let action = self.expression()?;
        if self.is_token(TokenKind::Comma, 0) {
            self.consume_token().into_particle()?; // Consume ","
        }

        Ok(Some(Arm {
            value: Box::new(value),
            action: Box::new(action),
        }))
    }

    /// Parse Unwrap or Select block.
    fn select_or_unwrap_expr(&mut self) -> Result<(Pos, Expr, String, Vec<Arm>), IzeErr> {
        let (_, block_pos) = self.consume_token().into_particle()?; // consume keyword "select" or "unwrap"
        let expr = self.expression()?;

        if !(self.is_token(TokenKind::As, 0) && self.is_token(TokenKind::Ident, 1)) {
            return Err(IzeErr {
                message: "Expected keyword 'as' and an identifier".into(),
                pos: block_pos,
            });
        }

        self.consume_token().into_particle()?; // consume "as"
        let (alias, _) = self.consume_token().into_ident()?;

        if !self.is_token(TokenKind::OpenParenth, 0) {
            return Err(IzeErr {
                message: "Missing open parenthesis".into(),
                pos: block_pos,
            });
        }
        self.consume_token().into_particle()?; // consume "("

        let mut arms = Vec::new();
        while let Some(arm) = self.arm_expr()? {
            arms.push(arm);
        }

        if !self.is_token(TokenKind::ClosingParenth, 0) {
            return Err(IzeErr {
                message: "Missing closing parenthesis".into(),
                pos: block_pos,
            });
        }
        self.consume_token().into_particle()?; // consume ")"

        Ok((block_pos, expr, alias, arms))
    }

    /// Parse type.
    fn type_expr(&mut self) -> Result<Option<Expr>, IzeErr> {
        // Simple types
        if self.check_tokens(
            &[
                TokenKind::StringType,
                TokenKind::IntegerType,
                TokenKind::FloatType,
                TokenKind::BooleanType,
                TokenKind::NullType,
                TokenKind::NoneType,
            ],
            0,
        ) {
            // Parse a base type (String, Integer, Float, Boolean, None and Null)
            let (type_token, pos) = self.consume_token().into_particle()?;
            let ize_type: Type = match type_token {
                TokenKind::IntegerType => Type {
                    id: TypeId::Integer,
                    inner: Default::default(),
                },
                TokenKind::FloatType => Type {
                    id: TypeId::Float,
                    inner: Default::default(),
                },
                TokenKind::BooleanType => Type {
                    id: TypeId::Boolean,
                    inner: Default::default(),
                },
                TokenKind::StringType => Type {
                    id: TypeId::String,
                    inner: Default::default(),
                },
                TokenKind::NullType => Type {
                    id: TypeId::Null,
                    inner: Default::default(),
                },
                TokenKind::NoneType => Type {
                    id: TypeId::None,
                    inner: Default::default(),
                },
                _ => Err(IzeErr {
                    message: "Unexpected token, expecting a base type".into(),
                    pos,
                })?,
            };
            return Ok(Some(Expr::new(ExprSet::Type(ize_type), pos)));
        }
        //TODO: Parse a composed type (Map, List, Tuple, and Mux)
        //TODO: Parse a custom type (any identifier)
        todo!("Parse a type")
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
    fn check_tokens(&mut self, token_types: &[TokenKind], offset: usize) -> bool {
        // Check if token exist at the specified offset
        for t in token_types {
            if self.is_token(*t, offset) {
                return true;
            }
        }
        return false;
    }

    /// Check if token is a literal.
    fn is_literal(&mut self, offset: usize) -> bool {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            match token.lexeme {
                Lexeme::Float(_)
                | Lexeme::Int(_)
                | Lexeme::Bool(_)
                | Lexeme::String(_)
                | Lexeme::Particle(TokenKind::NullLiteral)
                | Lexeme::Particle(TokenKind::NoneLiteral) => true,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Check if token is an identifier.
    fn is_ident(&mut self, offset: usize) -> bool {
        // Check if token exist at the specified offset
        if let Some(token) = self.tokens.get(offset) {
            if let Lexeme::Ident(_) = token.lexeme {
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}
