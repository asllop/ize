//! # Parser / Statements
//!
//! Methods and types for parsing commands (statements).

use alloc::vec::Vec;
use rustc_hash::FxHashMap;

use crate::{
    ast::{
        Command, CommandSet, DotPath, Expr, FieldName, FieldType, Import, ImportPath, Literal,
        Model, ModelField, ModelType, Package, Pipe, PipeItem, PipeStruct, PipeVal, StructModel,
        Transfer, TransferDef, TransferFields,
    },
    common::BuildErr,
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
            Result::ize_err("Unknown command".into(), self.last_pos())
        }
    }

    fn import_command(&mut self) -> Result<Command, IzeErr> {
        let (_, pos) = self.consume_token().into_particle()?; // Consume "import"
        self.assert_token(
            TokenKind::OpenParenth,
            0,
            "Expecting an open parenthesis after import command",
        )?;
        self.discard_particle(TokenKind::OpenParenth)?;
        let mut packages = Vec::new();
        while !self.is_token(TokenKind::ClosingParenth, 0) {
            if let Some(package) = self.parse_import_line()? {
                packages.push(package);
            }
        }
        self.discard_particle(TokenKind::ClosingParenth)?;
        let import = Import { packages };
        Ok(Command::new(CommandSet::Import(import), pos))
    }

    fn model_command(&mut self) -> Result<Command, IzeErr> {
        let (_, pos) = self.consume_token().into_particle()?; // Consume "model"
        self.assert_token(TokenKind::Ident, 0, "Expecting an identifier as model name")?;
        let (model_name, _) = self.consume_token().into_ident()?;
        if self.is_token(TokenKind::OpenParenth, 0) {
            // Struct model
            self.discard_particle(TokenKind::OpenParenth)?;
            let mut fields = FxHashMap::default();
            let mut field_order = Vec::new();
            while !self.is_token(TokenKind::ClosingParenth, 0) {
                if let Some((field_name, model_field)) = self.parse_struct_line()? {
                    field_order.push(field_name.clone());
                    fields.insert(field_name, model_field);
                }
            }
            self.discard_particle(TokenKind::ClosingParenth)?;
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
                Result::ize_err(
                    "Expecting a type for model definition".into(),
                    self.last_pos(),
                )
            }
        }
    }

    fn transfer_command(&mut self) -> Result<Command, IzeErr> {
        let (_, pos) = self.consume_token().into_particle()?; // Consume "transfer"
        self.assert_token(
            TokenKind::Ident,
            0,
            "Expecting an identifier as transfer name",
        )?;
        let (transfer_name, _) = self.consume_token().into_ident()?;
        let input_type = if let Some((input_type, _)) = self.parse_type()? {
            input_type
        } else {
            return Result::ize_err("Transfer expecting an input type".into(), self.last_pos());
        };
        self.assert_token(
            TokenKind::Arrow,
            0,
            "Transfer expecting an arrow between input and output types",
        )?;
        self.discard_particle(TokenKind::Arrow)?;
        let output_type = if let Some((output_type, _)) = self.parse_type()? {
            output_type
        } else {
            return Result::ize_err("Transfer expecting an output type".into(), self.last_pos());
        };
        self.assert_token(
            TokenKind::OpenParenth,
            0,
            "Transfer expecting an open parenthesis after output type",
        )?;
        self.discard_particle(TokenKind::OpenParenth)?;
        let transfer_def =
            if self.is_token(TokenKind::Ident, 0) && self.is_token(TokenKind::Colon, 1) {
                // Parse attribute-expression pairs
                TransferDef::Fields(self.parse_transfer_body()?)
            } else {
                // Parse single expression transfer
                TransferDef::Expr(self.expression()?)
            };
        self.assert_token(
            TokenKind::ClosingParenth,
            0,
            "Transfer expecting a closing parenthesis after body definition",
        )?;
        self.discard_particle(TokenKind::ClosingParenth)?;
        let transfer = Transfer {
            name: transfer_name,
            input_type,
            output_type,
            def: transfer_def,
        };
        Ok(Command::new(CommandSet::Transfer(transfer), pos))
    }

    fn pipe_command(&mut self) -> Result<Command, IzeErr> {
        let (run, pos) = if self.is_token(TokenKind::Run, 0) {
            let (_, pos) = self.consume_token().into_particle()?; // Consume "run"
            self.discard_particle(TokenKind::Pipe)?;
            (true, pos)
        } else {
            let (_, pos) = self.consume_token().into_particle()?; // Consume "pipe"
            (false, pos)
        };
        self.assert_token(TokenKind::Ident, 0, "Expecting an identifier as pipe name")?;
        let (pipe_name, _) = self.consume_token().into_ident()?;
        self.assert_token(
            TokenKind::OpenParenth,
            0,
            "Pipe expecting an open parenthesis after name",
        )?;
        self.discard_particle(TokenKind::OpenParenth)?;

        let pipe_body = self.parse_pipe_body()?;

        self.assert_token(
            TokenKind::ClosingParenth,
            0,
            "Pipe expecting a closing parenthesis after pipe body",
        )?;
        self.discard_particle(TokenKind::ClosingParenth)?;
        let pipe = Pipe {
            run,
            name: pipe_name,
            items: pipe_body,
        };
        Ok(Command::new(CommandSet::Pipe(pipe), pos))
    }

    fn parse_import_line(&mut self) -> Result<Option<Package>, IzeErr> {
        if self.is_token(TokenKind::Comma, 0) {
            // End of package line
            self.discard_particle(TokenKind::Comma)?;
            Ok(None)
        } else if self.is_token(TokenKind::ClosingParenth, 0) {
            // End of import command
            Ok(None)
        } else if self.is_token(TokenKind::StringLiteral, 0) {
            if let (Literal::String(path), _) = self.consume_token().into_literal()? {
                let alias = if self.is_token(TokenKind::As, 0) {
                    self.discard_particle(TokenKind::As)?;
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
                Result::ize_err("Expecting a string literal".into(), self.last_pos())
            }
        } else if self.is_token(TokenKind::Ident, 0) {
            let dot_path = self.parse_dot_path()?;
            let alias = if self.is_token(TokenKind::As, 0) {
                self.discard_particle(TokenKind::As)?;
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
            Result::ize_err("Unrecognized import line format".into(), self.last_pos())
        }
    }

    fn parse_dot_path(&mut self) -> Result<DotPath, IzeErr> {
        let mut components = Vec::new();
        loop {
            let (component, _) = self.consume_token().into_ident()?;
            components.push(component);
            if self.is_token(TokenKind::Dot, 0) {
                self.discard_particle(TokenKind::Dot)?;
            } else {
                break;
            }
        }
        Ok(DotPath { path: components })
    }

    fn parse_struct_line(&mut self) -> Result<Option<(FieldName, ModelField)>, IzeErr> {
        if self.is_token(TokenKind::Comma, 0) {
            // End of line
            self.discard_particle(TokenKind::Comma)?;
            Ok(None)
        } else if self.is_token(TokenKind::ClosingParenth, 0) {
            // End of command
            Ok(None)
        } else if self.is_token(TokenKind::Ident, 0) {
            let (field_name, _) = self.consume_token().into_ident()?;
            if self.is_token(TokenKind::As, 0) {
                // Field definiton with rename
                self.discard_particle(TokenKind::As)?;
                if let (Literal::String(actual_name), _) = self.consume_token().into_literal()? {
                    self.assert_token(
                        TokenKind::Colon,
                        0,
                        "Model definition expecting a colon after field name",
                    )?;
                    self.discard_particle(TokenKind::Colon)?;
                    if let Some((field_type, _)) = self.parse_type()? {
                        let model_field = ModelField {
                            rename: Some(actual_name),
                            field_type: FieldType::Type(field_type),
                        };
                        Ok(Some((field_name, model_field)))
                    } else {
                        Result::ize_err(
                            "Expecting a type for model field definition".into(),
                            self.last_pos(),
                        )
                    }
                } else {
                    Result::ize_err(
                        "Model definition expecting a string literal as field alias".into(),
                        self.last_pos(),
                    )
                }
            } else {
                // Field definition without rename
                self.assert_token(
                    TokenKind::Colon,
                    0,
                    "Model definition expecting a colon after field name",
                )?;
                self.discard_particle(TokenKind::Colon)?;
                if self.is_token(TokenKind::ThreeDots, 0) {
                    self.discard_particle(TokenKind::ThreeDots)?;
                    let model_field = ModelField {
                        rename: None,
                        field_type: FieldType::Remain,
                    };
                    Ok(Some((field_name, model_field)))
                } else if let Some((field_type, _)) = self.parse_type()? {
                    let model_field = ModelField {
                        rename: None,
                        field_type: FieldType::Type(field_type),
                    };
                    Ok(Some((field_name, model_field)))
                } else {
                    Result::ize_err(
                        "Expecting a type for model field definition".into(),
                        self.last_pos(),
                    )
                }
            }
        } else {
            Result::ize_err("Unrecognized model line format".into(), self.last_pos())
        }
    }

    fn parse_transfer_body(&mut self) -> Result<TransferFields, IzeErr> {
        let mut fields = FxHashMap::default();
        while !self.is_token(TokenKind::ClosingParenth, 0) {
            if let Some((field_name, expr)) = self.parse_transfer_line()? {
                fields.insert(field_name, expr);
            }
        }
        Ok(TransferFields { fields })
    }

    fn parse_transfer_line(&mut self) -> Result<Option<(FieldName, Expr)>, IzeErr> {
        if self.is_token(TokenKind::Comma, 0) {
            // End of line
            self.discard_particle(TokenKind::Comma)?;
            Ok(None)
        } else if self.is_token(TokenKind::ClosingParenth, 0) {
            // End of command
            Ok(None)
        } else if self.is_token(TokenKind::Ident, 0) {
            let (attr_name, _) = self.consume_token().into_ident()?;
            if self.is_token(TokenKind::Colon, 0) {
                self.discard_particle(TokenKind::Colon)?;
                let expr = self.expression()?;
                Ok(Some((attr_name, expr)))
            } else {
                Result::ize_err(
                    "Transfer line expecting a colon after identifier".into(),
                    self.last_pos(),
                )
            }
        } else {
            Result::ize_err(
                "Transfer line expecting an identifier".into(),
                self.last_pos(),
            )
        }
    }

    fn parse_pipe_body(&mut self) -> Result<Vec<PipeItem>, IzeErr> {
        let mut items = Vec::new();
        while !self.is_token(TokenKind::ClosingParenth, 0) {
            if let Some(item) = self.parse_pipe_item()? {
                items.push(item);
            }
        }
        Ok(items)
    }

    fn parse_pipe_item(&mut self) -> Result<Option<PipeItem>, IzeErr> {
        if self.is_token(TokenKind::Comma, 0) {
            // End of line
            self.discard_particle(TokenKind::Comma)?;
            Ok(None)
        } else if self.is_token(TokenKind::ClosingParenth, 0) {
            // End of command
            Ok(None)
        } else if self.is_token(TokenKind::Ident, 0) {
            let (id, _) = self.consume_token().into_ident()?;
            let pipe_item = PipeItem {
                name: id,
                pipe_struct: self.parse_pipe_struct()?,
            };
            Ok(Some(pipe_item))
        } else {
            Result::ize_err("Pipe item expecting an identifier".into(), self.last_pos())
        }
    }

    fn parse_pipe_struct(&mut self) -> Result<Option<PipeStruct>, IzeErr> {
        if self.is_token(TokenKind::OpenParenth, 0) {
            self.discard_particle(TokenKind::OpenParenth)?;
            let mut fields = FxHashMap::default();
            while !self.is_token(TokenKind::ClosingParenth, 0) {
                if let Some((id, pipe_val)) = self.parse_pipe_struct_line()? {
                    fields.insert(id, pipe_val);
                }
            }
            self.discard_particle(TokenKind::ClosingParenth)?;
            Ok(Some(PipeStruct { fields }))
        } else {
            Ok(None)
        }
    }

    fn parse_pipe_struct_line(&mut self) -> Result<Option<(FieldName, PipeVal)>, IzeErr> {
        if self.is_token(TokenKind::Ident, 0) {
            let (id, _) = self.consume_token().into_ident()?;
            self.assert_token(
                TokenKind::Colon,
                0,
                "Pipe struct line expecting a colon after identifier",
            )?;
            self.discard_particle(TokenKind::Colon)?;
            let pipe_val = if self.is_token(TokenKind::Ident, 0) {
                let (ident, _) = self.consume_token().into_ident()?;
                PipeVal::Identifier(ident)
            } else if self.check_tokens(
                &[
                    TokenKind::IntegerLiteral,
                    TokenKind::FloatLiteral,
                    TokenKind::BooleanLiteral,
                    TokenKind::StringLiteral,
                    TokenKind::NoneLiteral,
                    TokenKind::NullLiteral,
                ],
                0,
            ) {
                let (literal, _) = self.consume_token().into_literal()?;
                PipeVal::Literal(literal)
            } else if self.is_token(TokenKind::OpenParenth, 0) {
                if let Some(pipe_struct) = self.parse_pipe_struct()? {
                    PipeVal::Struct(pipe_struct)
                } else {
                    Result::ize_err(
                        "Pipe struct line expecting a struct as value.".into(),
                        self.last_pos(),
                    )?
                }
            } else {
                Result::ize_err(
                    "Pipe struct line expecting a valid value: identifier, literal or struct."
                        .into(),
                    self.last_pos(),
                )?
            };
            if self.is_token(TokenKind::Comma, 0) {
                self.discard_particle(TokenKind::Comma)?;
            } else {
                self.assert_token(TokenKind::ClosingParenth, 0, "Pipe struct expecting either comma or closing parenthesis after pasing a line.")?;
            }
            Ok(Some((id, pipe_val)))
        } else {
            Result::ize_err(
                "Pipe struct line expecting an identifier".into(),
                self.last_pos(),
            )
        }
    }
}
