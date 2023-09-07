//! # IZE Parser
//!
//! This module contains all the types and methods necessary to parse commands and expressions and generate an AST.

mod common;

mod expr;

mod stmt;

use crate::{ast::Command, lexer::Token, IzeErr};
use alloc::{collections::VecDeque, vec::Vec};

/// Code parser.
pub struct Parser {
    tokens: VecDeque<Token>,
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
        let mut commands = Vec::new();
        while !self.ended() {
            commands.push(self.command()?);
        }
        Ok(commands)
    }
}
