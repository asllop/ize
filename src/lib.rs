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

mod common;
pub use common::{IzeErr, Pos};

/// Lexical analyzer.
pub mod lexer;

/// Abstract Syntax Tree.
pub mod ast;

/// Parser.
pub mod parser;

//TODO: tests
