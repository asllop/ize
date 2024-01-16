//! # IZE Language
//!
//! IZE is a Turing-complete, domain-specific programming language, specialized in data pipelines and ETL processes, designed to accomplish the following data related tasks:
//!
//! - Aggregate
//! - Buffer
//! - Collect
//! - Combine
//! - Convert
//! - Expand
//! - Filter
//! - Route
//! - Sample
//! - Wrap
//!
//! It transpiles into Rust and can generate binaries for the following environments:
//!
//! - System service (deamon).
//! - Run-once service (cron).
//! - On-cloud Lambda for AWS.
//! - CLI tool.
//!
//! The language is statically typed, async oriented, and declarative, with a minimalistic and expressive syntax, influenced by:
//!
//! - Protobuf
//! - Rust / Serde
//! - Vector VRL
//! - New Relic Flex
//! - SQL
//! - Go
//! - TypeScript
//! - Lua

#![no_std]

#[macro_use]
extern crate alloc;

/// Error type.
pub mod err;

/// Lexical analyzer.
pub mod lexer;

/// Abstract Syntax Tree.
pub mod ast;

/// Parser combinator primitives.
pub mod parser;

/// Grammar rules to parse the IZE language.
pub mod grammar;

#[cfg(test)]
mod tests;
