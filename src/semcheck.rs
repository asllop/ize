//! # Semantic checker
//!
//! The semantic checker ensures that an IZE program is semantically correct, looking for:
//!
//! - Type mismatches.
//! - Undefined symbols.
//! - Incorrect number of arguments.
//! - Illegal expressions.
//!
//! It can also offer lint warning like: variable declared and never used, imported module never used, etc.
//!
//! Semcheck is the last step before code generation.
//!

/*
Required checks:

TRANSFERS:

    - Check that argument types and return type are either Type expressions or Primary identifiers.

*/
