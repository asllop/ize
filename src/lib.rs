//! # IZE Language
//!
//! IZE is a programming language specialized in data pipelines and ETL processes, designed to accomplish the following tasks:
//!
//! - Define data models.
//! - Convert from different data formats.
//! - Apply data transformations.
//! - Route data from multiple origins to multiple destinations.
//! - Define data fetching and pushing policies.
//!
//! It can either transcompile to Rust or run an interpreter.
//! 
//! IZE is a statically typed, async oriented, and functional-styled programming language with a minimalistic and expressive syntax.

#![no_std]

#[macro_use]
extern crate alloc;

/// First strage: Lexical analyzer.
pub mod lexer;

/// Second stage: Parser.
pub mod parser;
