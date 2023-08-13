//! # IZE Language
//!
//! IZE is a domain-specific programming language specialized in data pipelines and ETL processes, designed to accomplish the following data related tasks:
//! - Convert
//! - Aggregate
//! - Filter
//! - Route
//! - Expand
//! - Buffer
//! - Pack
//!
//! It transpiles into Rust and can run in multiple environments:
//! - System service (deamon) for Unix-like systems and Windows.
//! - On-cloud Lambda/Function for AWS, Azure and Google Cloud.
//! - CLI tool for Unix-like systems and Windows.
//!
//! Is statically typed, async oriented, and declarative, with a minimalistic and expressive syntax, influenced by:
//! - Protobuf
//! - Rust
//! - Serde
//! - Go
//! - TypeScript
//! - Vector VRL
//! - New Relic Flex
//! - SQL

#![no_std]

#[macro_use]
extern crate alloc;

//mod env;

mod common;

/// First strage: Lexical analyzer.
pub mod lexer;

// /// Second stage: Parser.
// pub mod parser;

// /// Third stage: Interpreter.
// pub mod eval;
