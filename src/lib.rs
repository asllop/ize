//! # IZE Language
//!
//! IZE is a programming language specialized in data pipelines and ETL processes. Desgned to accomplish the following tasks:
//!
//! - Define data models.
//! - Convert from different data formats.
//! - Apply data transformations.
//! - Route data from multiple origins to multiple destinations.
//! - Define data fetching and pushing policies.
//!
//! It transcompiles into Rust code.

#![no_std]
extern crate alloc;

/// Lexical analyzer.
pub mod lexer;
