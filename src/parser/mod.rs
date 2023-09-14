//! # IZE Parser
//!
//! This module contains all the types and methods necessary to parse commands and expressions and generate an AST.

mod common;

mod expr;

mod stmt;

mod import;

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

    /* TODO:
       Import is not an actual command, it's more a metacommand. It should be executed at compile time.
       The result of parsing an IZE file is an AST. The same way, the result of importing a module must be an AST.
       When we parse an import, we must put the command in a separate place, and then exeute it: lexing and parsing it to generate an AST.
    */

    /// Parse the entire program.
    pub fn parse(&mut self) -> Result<Vec<Command>, IzeErr> {
        let mut commands = Vec::new();
        while !self.ended() {
            //TODO: check of command is an import, and load the file, lex and parse it
            commands.push(self.command()?);
        }
        Ok(commands)
    }
}
