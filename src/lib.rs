//! # IZE Language
//!
//! IZE is a domain-specific programming language specialized in data pipelines and ETL processes, designed to accomplish the following data related tasks:
//! - Aggregate
//! - Buffer
//! - Combine
//! - Convert
//! - Expand
//! - Filter
//! - Route
//! - Wrap
//!
//! It transpiles into Rust and can run in multiple environments:
//! - System service (deamon) for Unix-like systems and Windows.
//! - On-cloud Lambda/Function for AWS, Azure and Google Cloud.
//! - CLI tool for Unix-like systems and Windows.
//!
//! Is statically typed, async oriented, and declarative, with a minimalistic and expressive syntax, influenced by:
//! - Protobuf
//! - Rust / Serde
//! - Vector VRL
//! - New Relic Flex
//! - SQL
//! - Go
//! - TypeScript

#![no_std]

#[macro_use]
extern crate alloc;

// Common types, used by all parts of the crate.
mod common;
pub use common::{IzeErr, Pos};

/// Lexical analyzer.
pub mod lexer;

/// Abstract Syntax Tree.
pub mod ast;

/// Parser (syntactic analyzer).
pub mod parser;

/// Semantic analyzer.
pub mod semanter;

/// Interface for creating Extensions / Plugins.
pub mod ext;

//TODO: tests

use alloc::string::String;
use ast::{Ast, CommandSet, ImportPath};
use lexer::Lexer;
use parser::Parser;

/// Build AST
pub fn build<'a>(code: &'a str, reader: fn(&ImportPath) -> String) -> Result<Ast, IzeErr> {
    let mut ast = ast::Ast {
        commands: Default::default(),
        imports: Default::default(),
    };

    let mut parser = Parser::new(Lexer::new(code).tokenize()?);

    while !parser.ended() {
        let command = parser.command()?;
        if let CommandSet::Import(import) = command.command {
            for pkg in import.packages {
                let pkg_ast = build(&reader(&pkg.path), reader)?;
                //TODO: what if alias is None??? We have to get it from the package last component
                ast.imports.insert(pkg.alias.unwrap(), pkg_ast);
            }
        } else {
            ast.commands.push(command);
        }
    }

    //TODO: call the semantic checker

    Ok(ast)
}
