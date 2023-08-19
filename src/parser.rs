//! # IZE Parser
//!
//! This module contains all the types and methods necessary to parse commands and expressions and generate an AST.

//TODO: create a struct for expression precedence, so we can parse asking to recursively parse a higher precedence thing without
// having to hardcode the precedence in the parser.

use crate::{ast::Command, lexer::Token, IzeErr};
use alloc::vec::Vec;

pub struct Parser {
    pub tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Result<Vec<Command>, IzeErr> {
        //TODO
        todo!()
    }
}
