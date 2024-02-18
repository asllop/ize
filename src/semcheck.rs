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
//! 2nd, generate a symbol table:
//!
//! - Specify the entity of each symbol (is it a transfer, a module, etc).
//! - Convert identifiers into Type expressions when necessary.
//! - Infere types of variables, constants, and anonymous transfers.
//!
//! It can also offer lint warning like: variable declared and never used, imported module never used, etc.
//!
//! Semchecking is the last step before code generation.

use alloc::string::String;
use rustc_hash::FxHashMap;

use crate::{
    ast::{Ast, Command, CommandKind, ExpressionKind, Primary},
    err::IzeErr,
};

#[derive(Debug)]
/// Symbol Table, associate each identifier in the code with metadata.
pub struct SymbolTable {
    pub symbols: FxHashMap<String, SymbolMetadata>,
}

#[derive(Debug)]
/// Symbol metadata.
pub struct SymbolMetadata {
    pub kind: SymbolKind,
}

#[derive(Debug)]
/// Symbol kind.
pub enum SymbolKind {
    Model,
    Transfer,
    Const,
    Pipe,
}

/// Check AST validity. TODO: return symbol table
pub fn check_ast(ast: &Ast) -> Result<(), IzeErr> {
    // TODO: Build the symbol table

    check_imports(ast)?;

    // TODO: Model Check:
    //      - Scan each model and put the name in the ST.
    //      - Scan again and check that each property has a type that actually exist and is a model.
    Ok(())
}

/// Check import commands. It's a pre-semcheck, for import commands only, to make sure that:
/// - Path is a Dot expr and contains only Primary identifiers.
/// - Imports with "*" only contain that symbol and no rename.
pub fn check_import_commands(commands: &[Command]) -> Result<(), IzeErr> {
    for cmd in commands {
        if let CommandKind::Import { path, symbols } = &cmd.kind {
            // Check path
            match &path.kind {
                ExpressionKind::Dot(expr_vec) => {
                    for expr in expr_vec {
                        if let ExpressionKind::Primary(Primary::Identifier(_)) = &expr.kind {
                            // Correct
                        } else {
                            return Err(IzeErr::new(
                                "All components of an import path must be identifiers".into(),
                                expr.pos,
                            ));
                        }
                    }
                }
                ExpressionKind::Primary(Primary::Identifier(_)) => {
                    // Correct
                }
                _ => {
                    return Err(IzeErr::new(
                        "The path of an import must be a dot expression or identifier".into(),
                        path.pos,
                    ))
                }
            }
            // Check symbols
            if symbols.len() == 1 {
                // Make sure it's a "*", and no rename present
                let sym = &symbols[0];
                if sym.symbol.id == "*" && sym.rename.is_some() {
                    return Err(IzeErr::new(
                        "Imported '*' can't include a rename".into(),
                        cmd.pos,
                    ));
                }
            } else if symbols.len() > 1 {
                // Make sure there's no "*" symbol here
                for sym in symbols {
                    if sym.symbol.id == "*" {
                        return Err(IzeErr::new("Imported '*' must be alone".into(), cmd.pos));
                    }
                }
            } else {
                return Err(IzeErr::new(
                    "Imported symbols must contain at least one element".into(),
                    cmd.pos,
                ));
            }
        } else {
            return Err(IzeErr::new(
                "Commands passed to 'check_import_commands' must be imports".into(),
                cmd.pos,
            ));
        }
    }
    Ok(())
}

// TODO: Import Check:
//      - Check that each imported symbol actually exist in the imported AST.
//      - Add them to the ST and decorate with the kind of symbol it is.
fn check_imports(ast: &Ast) -> Result<(), IzeErr> {
    // for (sym, (index, _)) in &ast.imported_symbols {
    //     let index = *index;
    //     let import = &ast.imports[index];
    // }
    Ok(())
}

fn find_symbol_in_ast(sym: String, ast: &Ast) -> bool {
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
    - Recursive references (or we can use a Box to avoid problems).

PIPE:

    - Check there is no repeated pipe names.
    - Check that symbols exist and are the appropiate.
    - Check that input and output types of each step match.
    - Check arguments.

RUN:

    - If it's a pipe id, check that the id exists and it's a pipe.
    - If it's an inline pipe, same checks as PIPE apply.

IMPORT:

    - Check that imported entities do exist.
    - Check name clashes.

CONST:

    - Check there is no repeated const names.
    - Check that expression is a literal.

Expressions:

    Types:
        - Check that compound types are correctly formed (List, Map, Tuple, etc).
        - Make sure that Mux types with the same subtypes in different order are actually the same type.

    Chain expressions define a scope for variables. Chains can contain other chains, thhat inherit the parent scope plus adding its own.
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
