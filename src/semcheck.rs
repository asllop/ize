//! # Semantic checker
//!
//! The semantic checker has two main jobs:
//!
//! 1st, ensure that an IZE program is semantically correct, looking for:
//!
//! - Type mismatches.
//! - Undefined symbols.
//! - Incorrect arguments.
//! - Illegal expressions.
//!
//! 2nd, generate a symbol table and decorate the AST:
//!
//! - Specify the entity of each symbol (is it a transfer, a module, etc).
//! - Convert identifiers into Type expressions when necessary.
//! - Infere types of variables, constants, and anonymous transfers.
//!
//! It can also offer lint warning like: variable declared and never used, imported module never used, etc.
//!
//! Semchecking is the last step before code generation.
//!

/*
Required checks:

TRANSFERS:

    - Check that argument types and return type are either Type expressions or Primary identifiers.

*/
