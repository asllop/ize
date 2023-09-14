//! # Parser / Common
//!
//! Common types and methods for the parser module.

use crate::{
    ast::Literal,
    common::BuildErr,
    lexer::{Lexeme, Token, TokenKind},
    parser::Parser,
    IzeErr, Pos,
};
use alloc::string::String;

pub(crate) trait FromToken {
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
            Result::ize_err("Token is None".into(), Pos::default())
        }
    }

    fn into_particle(self) -> Result<(TokenKind, Pos), IzeErr> {
        let (lexeme, pos) = self.into_parts()?;
        if let Lexeme::Particle(t) = lexeme {
            Ok((t, pos))
        } else {
            Result::ize_err("Expected a particle".into(), Pos::default())
        }
    }

    fn into_ident(self) -> Result<(String, Pos), IzeErr> {
        let (lexeme, pos) = self.into_parts()?;
        if let Lexeme::Ident(s) = lexeme {
            Ok((s, pos))
        } else {
            Result::ize_err("Expected an identifier".into(), Pos::default())
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
            _ => Result::ize_err("Expected a literal".into(), Pos::default()),
        }
    }
}

impl Parser {
    /// Check if parser ended processing tokens.
    pub fn ended(&self) -> bool {
        self.tokens.is_empty()
    }

    pub(crate) fn consume_token(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    /// Consume a particle token making sure the type is correct
    pub(crate) fn discard_particle(&mut self, particle: TokenKind) -> Result<(), IzeErr> {
        match self.consume_token().into_particle() {
            Ok((token_kind, _)) => {
                if token_kind == particle {
                    Ok(())
                } else {
                    Result::ize_err("Unexpected particle type".into(), self.last_pos())
                }
            }
            Err(_) => Result::ize_err("Unexpected token type".into(), self.last_pos()),
        }
    }

    /// Check for a particular token.
    pub(crate) fn is_token(&mut self, token_type: TokenKind, offset: usize) -> bool {
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

    /// Check for a particular token and return an error if not found.
    pub(crate) fn assert_token(
        &mut self,
        token_type: TokenKind,
        offset: usize,
        err_msg: &str,
    ) -> Result<(), IzeErr> {
        if !self.is_token(token_type, offset) {
            Result::ize_err(err_msg.into(), self.last_pos())
        } else {
            Ok(())
        }
    }

    /// Check for a list of tokens.
    pub(crate) fn check_tokens(&mut self, token_types: &[TokenKind], offset: usize) -> bool {
        // Check if token exist at the specified offset
        for t in token_types {
            if self.is_token(*t, offset) {
                return true;
            }
        }
        false
    }

    /// Check if token is a literal.
    pub(crate) fn is_literal(&mut self, offset: usize) -> bool {
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
    pub(crate) fn is_ident(&mut self, offset: usize) -> bool {
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

    /// Last pos
    pub(crate) fn last_pos(&mut self) -> Pos {
        if !self.ended() {
            self.tokens[0].pos
        } else {
            Default::default()
        }
    }
}
