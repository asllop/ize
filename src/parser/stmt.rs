//! # Parser / Statements
//!
//! Methods and types for parsing commands (statements).

use alloc::vec::Vec;
use rustc_hash::FxHashMap;

use crate::{
    ast::{
        Command, CommandSet, DotPath, FieldName, FieldType, Import, ImportPath, Literal, Model,
        ModelField, ModelType, Package, StructModel,
    },
    lexer::TokenKind,
    parser::{common::FromToken, Parser},
    IzeErr,
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
            return Err(IzeErr {
                message: "Expecting an open parenthesis after import command".into(),
                pos: self.last_pos(),
            });
        }
        self.consume_token().into_particle()?; // Consume "("
        let mut packages = Vec::new();
        while !self.is_token(TokenKind::ClosingParenth, 0) {
            if let Some(package) = self.parse_import_line()? {
                packages.push(package);
            }
        }
        self.consume_token().into_particle()?; // Consume ")"
        let import = Import { packages };
        Ok(Command::new(CommandSet::Import(import), pos))
    }

    fn model_command(&mut self) -> Result<Command, IzeErr> {
        let (_, pos) = self.consume_token().into_particle()?; // Consume "model"
        if !self.is_token(TokenKind::Ident, 0) {
            return Err(IzeErr {
                message: "Expecting an identifier as model name".into(),
                pos: self.last_pos(),
            });
        }
        let (model_name, _) = self.consume_token().into_ident()?;
        if self.is_token(TokenKind::OpenParenth, 0) {
            // Struct model
            self.consume_token().into_particle()?; // Consume "("
            let mut fields = FxHashMap::default();
            let mut field_order = Vec::new();
            while !self.is_token(TokenKind::ClosingParenth, 0) {
                if let Some((field_name, model_field)) = self.parse_struct_line()? {
                    field_order.push(field_name.clone());
                    fields.insert(field_name, model_field);
                }
            }
            self.consume_token().into_particle()?; // Consume ")"
            let struct_model = StructModel {
                fields,
                field_order,
            };
            let model = Model {
                name: model_name,
                model_type: ModelType::Struct(struct_model),
            };
            Ok(Command::new(CommandSet::Model(model), pos))
        } else {
            // Type alias model
            if let Some((model_type, _)) = self.parse_type()? {
                let model = Model {
                    name: model_name,
                    model_type: ModelType::Alias(model_type),
                };
                Ok(Command::new(CommandSet::Model(model), pos))
            } else {
                Err(IzeErr {
                    message: "Expecting a type for model definition".into(),
                    pos: self.last_pos(),
                })
            }
        }
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
                let alias = if self.is_token(TokenKind::As, 0) {
                    self.consume_token().into_particle()?; // Consume "as"
                    let (alias, _) = self.consume_token().into_ident()?;
                    Some(alias)
                } else {
                    None
                };
                Ok(Some(Package {
                    path: ImportPath::File(path),
                    alias,
                }))
            } else {
                Err(IzeErr {
                    message: "Expecting a string literal".into(),
                    pos: self.last_pos(),
                })
            }
        } else if self.is_token(TokenKind::Ident, 0) {
            let dot_path = self.parse_dot_path()?;
            let alias = if self.is_token(TokenKind::As, 0) {
                self.consume_token().into_particle()?; // Consume "as"
                let (alias, _) = self.consume_token().into_ident()?;
                Some(alias)
            } else {
                None
            };
            Ok(Some(Package {
                path: ImportPath::Dot(dot_path),
                alias,
            }))
        } else {
            Err(IzeErr {
                message: "Unrecognized import line format".into(),
                pos: self.last_pos(),
            })
        }
    }

    fn parse_dot_path(&mut self) -> Result<DotPath, IzeErr> {
        let mut components = Vec::new();
        loop {
            let (component, _) = self.consume_token().into_ident()?;
            components.push(component);
            if self.is_token(TokenKind::Dot, 0) {
                self.consume_token().into_particle()?; // Consume "."
            } else {
                break;
            }
        }
        Ok(DotPath { path: components })
    }

    //TODO: parse remaining field "..."
    fn parse_struct_line(&mut self) -> Result<Option<(FieldName, ModelField)>, IzeErr> {
        if self.is_token(TokenKind::Comma, 0) {
            // End of line
            self.consume_token().into_particle()?; // Consume ","
            Ok(None)
        } else if self.is_token(TokenKind::ClosingParenth, 0) {
            // End of command
            Ok(None)
        } else if self.is_token(TokenKind::Ident, 0) {
            let (field_name, _) = self.consume_token().into_ident()?;
            if self.is_token(TokenKind::As, 0) {
                // Field definiton with rename
                self.consume_token().into_particle()?; // Consume "as"
                if let (Literal::String(actual_name), _) = self.consume_token().into_literal()? {
                    if !self.is_token(TokenKind::Colon, 0) {
                        return Err(IzeErr {
                            message: "Model definition expecting a colon after field name".into(),
                            pos: self.last_pos(),
                        });
                    }
                    self.consume_token().into_particle()?; // Consume ":"
                    if let Some((field_type, _)) = self.parse_type()? {
                        let model_field = ModelField {
                            rename: Some(actual_name),
                            field_type: FieldType::Type(field_type),
                        };
                        Ok(Some((field_name, model_field)))
                    } else {
                        Err(IzeErr {
                            message: "Expecting a type for model field definition".into(),
                            pos: self.last_pos(),
                        })
                    }
                } else {
                    Err(IzeErr {
                        message: "Model definition expecting a string literal as field alias"
                            .into(),
                        pos: self.last_pos(),
                    })
                }
            } else {
                // Field definition without rename
                if !self.is_token(TokenKind::Colon, 0) {
                    return Err(IzeErr {
                        message: "Model definition expecting a colon after field name".into(),
                        pos: self.last_pos(),
                    });
                }
                self.consume_token().into_particle()?; // Consume ":"
                if self.is_token(TokenKind::ThreeDots, 0) {
                    self.consume_token().into_particle()?; // Consume "..."
                    let model_field = ModelField {
                        rename: None,
                        field_type: FieldType::Remain,
                    };
                    Ok(Some((field_name, model_field)))
                } else {
                    if let Some((field_type, _)) = self.parse_type()? {
                        let model_field = ModelField {
                            rename: None,
                            field_type: FieldType::Type(field_type),
                        };
                        Ok(Some((field_name, model_field)))
                    } else {
                        Err(IzeErr {
                            message: "Expecting a type for model field definition".into(),
                            pos: self.last_pos(),
                        })
                    }
                }
            }
        } else {
            Err(IzeErr {
                message: "Unrecognized model line format".into(),
                pos: self.last_pos(),
            })
        }
    }
}
