//! # Parser / Statements
//!
//! Methods and types for parsing commands (statements).

use alloc::vec::Vec;

use crate::{
    ast::{Command, CommandSet, Package, Import, Literal, ImportPath},
    lexer::TokenKind,
    parser::{Parser, common::FromToken},
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
        let (_, pos) = self.consume_token().into_particle()?; // Consume "import"
        if !self.is_token(TokenKind::OpenParenth, 0) {
            return Err(IzeErr { message: "Expecting an open parenthesis after import command".into(), pos: self.last_pos() })
        }
        self.consume_token().into_particle()?; // Consume "("
        let mut packages = Vec::new();
        while !self.is_token(TokenKind::ClosingParenth, 0) {
            if let Some(package) = self.parse_import_line()? {
                packages.push(package);
            }
        }
        self.consume_token().into_particle()?; // Consume ")"
        let import = Import {
            packages
        };
        Ok(Command::new(CommandSet::Import(import), pos))
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

    fn parse_import_line(&mut self) -> Result<Option<Package>, IzeErr> {
        if self.is_token(TokenKind::Comma, 0) {
            // End of package line
            self.consume_token().into_particle()?; // Consume ","
            Ok(None)
        } else if self.is_token(TokenKind::ClosingParenth, 0) {
            // End of import command
            Ok(None)
        } else if self.is_token(TokenKind::StringLiteral, 0) {
            if let (Literal::String(path), _) = self.consume_token().into_literal()? {
                if self.is_token(TokenKind::As, 0) {
                    self.consume_token().into_particle()?; // Consume "as"
                    let (alias, _) = self.consume_token().into_ident()?;
                    Ok(Some(Package {
                        path: ImportPath::File(path),
                        alias,
                    }))
                } else {
                    Err(IzeErr { message: "Expecting 'as' after path".into(), pos: self.last_pos() })
                }
            } else {
                Err(IzeErr { message: "Expecting a string literal".into(), pos: self.last_pos() })
            }
        } else if self.is_token(TokenKind::Ident, 0) {
            //TODO: parse dot line
            todo!("Dot path line")
        } else {
            Err(IzeErr { message: "Unrecognized import line format".into(), pos: self.last_pos() })
        }
    }
}
