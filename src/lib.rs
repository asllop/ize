//! # IZE Language: The glue code of data engineering.
//!
//! IZE is a Turing-complete, domain-specific programming language, specialized in data pipelines and ETL processes, designed to accomplish, among others, the following data related tasks:
//!
//! - Aggregate
//! - Buffer
//! - Collect
//! - Combine
//! - Convert
//! - Deduplicate
//! - Expand
//! - Filter
//! - Group
//! - Report
//! - Route
//! - Sample
//! - Split
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
//! - Python

#![no_std]

#[macro_use]
extern crate alloc;

/// Position types.
pub mod pos;

/// Error type.
pub mod err;

/// Lexical analyzer.
pub mod lexer;

/// Abstract Syntax Tree.
pub mod ast;

/// Parser combinator primitives.
pub mod parser;

/// Grammar rules to parse IZE expressions.
pub mod grammar_expr;

/// Grammar rules to parse IZE commands.
pub mod grammar_cmd;

/// Symbol table.
pub mod symbol_table;

/// Semantic checker.
pub mod semcheck;

/// Rust transpiler.
pub mod transpiler;

#[cfg(test)]
mod tests;
