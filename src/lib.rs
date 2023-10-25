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

//TODO: uncomment this
//#![no_std]

#[macro_use]
extern crate alloc;

/* IMPROVEMENTS
- Put Pos on every AST component, not only top level ones (Command and Expr).
- Modify the AST and parser to better fit the structure of the code.
    - Each sintactic component needs a separate AST component and a Pos.
    - Separate the AST from the symbol table.
    - Separate Import from Command. Import is not a command.
*/

// Common types, used by all parts of the crate.
mod common;
pub use common::{BuildErr, IzeErr, Pos};

mod compiler;
pub use compiler::build;

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
