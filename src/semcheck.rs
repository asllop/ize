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

use crate::{
    ast::{AstNode, CommandKind},
    err::IzeErr
};
use alloc::{string::String, vec::Vec};
use rustc_hash::FxHashMap;

type Identifier = String;
type Alias = String;

//TODO: imports will also fill data in the SymbolTable

//TODO: chain expressions define a scope for variables. Chains can contain other chains, thhat inherit the parent scope plus adding its own.

/// Table of symbols present in the AST.
pub struct SymbolTable {
    pub symbols: FxHashMap<Identifier, Symbol>,
}

/// Symbol information and metadata.
pub enum Symbol {
    Model {
        body: ModelBody
    },
    //TODO: transfers, with environments and scopes for variables (let symbols)
    Transfer,
    //TODO: pipe
    Pipe,
    Const {
        value_type: LiteralType
    },
}

/// Types of literals. Used by const symbols.
pub enum LiteralType {
    String,
    Integer,
    Float,
    Boolean,
    None,
    Null,
}

/// Model content.
pub enum ModelBody {
    Alias(Type),
    Struct(
        FxHashMap<Identifier, (Option<Alias>, Type)>
    )
}

/// Representation of a type.
pub struct Type {
    pub name: String,
    pub subtypes: Vec<Type>,
}

/// Check AST validity.
pub fn check_ast(ast: &[AstNode]) -> Result<(), IzeErr> {
    //TODO: build Symbol Table
    for node in ast {
        match node {
            AstNode::Command(cmd) => match &cmd.kind {
                CommandKind::Model { ident_token, body } => check_model_cmd(ident_token, body)?,
                CommandKind::Import { path_vec } => todo!(),
                CommandKind::Transfer { ident_token, param_vec, return_type, body } => todo!(),
                CommandKind::Pipe { ident_token, pipe_body_expr } => todo!(),
                CommandKind::Run { pipe } => todo!(),
                CommandKind::Const { ident_token, value_token } => todo!(),
            },
            _ => return Err(IzeErr::new("All nodes in the root node must be commands".into(), Default::default()))
        }
    }
    Ok(())
}

fn check_model_cmd(ident_token: &AstNode, body: &AstNode) -> Result<(), IzeErr> {

    todo!()
}

/*
Required checks:

GENERAL:

    - Check that there is no repeated name across models, pipes and consts.
    - Check that only transfers and models can have the same name. Not transfers and other kinds of commands.

TRANSFERS:

    - Check that transfers with the same name have different signatures.
    - Check that argument types and return type are either Type expressions or Primary identifiers.
    - Check all symbols used by the expression (need ST).
    - If transfer is expression, check that expression is correct in terms of types (need ETE).
    - If transfer is key/val, check all attributes exist in the return model and values are of the right type for the attribute (need ST).

MODELS:

    - Check there is no repeated model names.
    - Check all types are valid (either Type expr of Primary ident).
    - Check there is no repeated attribute names.
    - Check there is no repeated alias.
    - Check no more than one "..." field is present.
    - Check all types actually exist (need ST).

PIPE:

    - Check there is no repeated pipe names.
    - Check that symbols exist and are the appropiate.
    - Check that input and output types of each step match.
    - Check arguments.

RUN:

    - If it's a pipe id, check that the id exists and it's a pipe.
    - If it's an inline pipe, same checks as PIPE apply.

CONST:

    - Check there is no repeated const names.
    - Check that expression is a literal.

Expressions:

    Types:
        - Check that compound types are correctly formed (List, Map, Tuple, etc).
        - Make sure that Mux types with the same subtypes in different order are actually the same type.
*/


/* TODO

- Build symbol table (ST):
    Scan de AST and add each model, transfer, pipe, and const identifier to a hashmap. Also scan transfer bodies looking for variables.
    Each entry will contain the entity type (model, transfer, pipe, const, let), and the types involved.
    - pipe, no types.
    - transfer, the signature, parameters and return types.
    - const, the literal type.
    - let, the type is not explicit, we have to calculate it from the expression used to define the variable. We need an Expression Type Evaluator (ETE).
*/

//TODO: Build Symbol Table (ST) with explicit type symbols (transfers, models and consts).
//TODO: Expression Type Evaluator (ETE), given an expression, calculate the resulting type. It requires a ST already filled with explicit type symbols.
//TODO: Use the ETE to fill the ST with non-explicit type symbols (lets).
