//! # Parser / Statements
//!
//! Methods and types for parsing commands (statements).

use crate::{
    ast::{Command, CommandSet},
    lexer::TokenKind,
    parser::Parser,
    IzeErr, Pos,
};

impl Parser {
    /// Parse a single command.
    pub fn command(&mut self) -> Result<Command, IzeErr> {
        if self.is_token(TokenKind::Import, 0) {
            self.import_command()
        } else if self.is_token(TokenKind::Model, 0) {
            self.model_command()
        } else if self.is_token(TokenKind::Transfer, 0) {
            self.transfer_command()
        } else if self.check_tokens(&[TokenKind::Run, TokenKind::Pipe], 0) {
            self.pipe_command()
        } else {
            Err(IzeErr {
                message: "Unknown command".into(),
                pos: self.last_pos(),
            })
        }
    }

    fn import_command(&mut self) -> Result<Command, IzeErr> {
        todo!()
    }

    fn model_command(&mut self) -> Result<Command, IzeErr> {
        todo!()
    }

    fn transfer_command(&mut self) -> Result<Command, IzeErr> {
        todo!()
    }

    fn pipe_command(&mut self) -> Result<Command, IzeErr> {
        todo!()
    }
}
