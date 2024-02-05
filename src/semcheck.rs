//! # TODO: Semantic checker
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
//! 2nd, generate a symbol table and an intermediate AST:
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
    ast::{AstNode, Command, CommandKind, ExpressionKind},
    symbols::*,
    err::IzeErr,
    lexer::TokenKind,
};
use alloc::string::String;
use rustc_hash::FxHashMap;

//TODO: imports must fill the ST with the symbols imported from other modules.
//      Or the checker must inspect the ST of the other ASTs (we have an AST per module/file).

//TODO: chain expressions define a scope for variables. Chains can contain other chains, thhat inherit the parent scope plus adding its own.

/// Check AST validity.
pub fn check_ast(ast: &[AstNode]) -> Result<SymbolTable, IzeErr> {
    let mut symtab = SymbolTable::default();
    for node in ast {
        match node {
            AstNode::Command(cmd) => match &cmd.kind {
                //TODO: Two stages:
                //  1. Put all models in the ST.
                //  2. Check for invalid types inspecting the ST (unknown types, incorrect subtypes, etc).
                CommandKind::Model { .. } => build_st_for_model_cmd(&mut symtab, cmd)?,
                CommandKind::Import { .. } => todo!(),
                CommandKind::Transfer { .. } => todo!(),
                CommandKind::Pipe { .. } => todo!(),
                CommandKind::Run { .. } => todo!(),
                CommandKind::Const { .. } => todo!(),
            },
            _ => {
                return Err(IzeErr::new(
                    "All nodes in the root node must be commands".into(),
                    node.start_pos(),
                ))
            }
        }
    }
    Ok(symtab)
}

//////////////////////////////
// Model checking functions //
//////////////////////////////

/// Put all models in the symbol table.
fn build_st_for_model_cmd(symtab: &mut SymbolTable, cmd: &Command) -> Result<(), IzeErr> {
    let (model_name, body) = unwrap_model_cmd(cmd)?;
    match body {
        AstNode::Expression(expr) => match &expr.kind {
            ExpressionKind::Primary { token } => {
                let type_obj = build_type_from_primary_expr(token)?;
                let symbol = Symbol::new(
                    SymbolKind::Model {
                        body: ModelBody::Alias(type_obj),
                        body_pos: expr.as_ref().into()
                    },
                    cmd.into()
                );
                symtab.symbols.insert(model_name, symbol);
            }
            ExpressionKind::Type {
                ident_token,
                subtypes_vec,
            } => {
                let type_obj = build_type_from_type_expr(ident_token, subtypes_vec)?;
                let symbol = Symbol::new(
                    SymbolKind::Model {
                        body: ModelBody::Alias(type_obj),
                        body_pos: expr.as_ref().into()
                    },
                    cmd.into()
                );
                symtab.symbols.insert(model_name, symbol);
            },
            _ => {
                return Err(IzeErr::new(
                    "Model body expression must be either a type or a primary".into(),
                    body.start_pos(),
                ))
            }
        },
        AstNode::Vec(pairs_vec) => {
            let mut model_struct = FxHashMap::default();

            for pair_expr in pairs_vec {
                let (identifier, alias_opt, type_obj) = build_struct_tuple_from_pair_expr(pair_expr)?;
                model_struct.insert(identifier, (alias_opt, type_obj));
            }

            // Create entry in the symbol table
            let symbol = Symbol::new(
                SymbolKind::Model {
                    body: ModelBody::Struct(model_struct),
                    body_pos: pairs_vec.as_slice().into(),
                },
                cmd.into()
            );
            symtab.symbols.insert(model_name, symbol);
        }
        _ => {
            return Err(IzeErr::new(
                "Model body must be either a vector or an expression".into(),
                body.start_pos(),
            ))
        }
    }
    Ok(())
}

/// Get the contents of a ExpressionKind::Primary and generates a Type struct.
fn build_type_from_primary_expr(primary_token: &AstNode) -> Result<Type, IzeErr> {
    let token = primary_token.token_ref().ok_or(
        IzeErr::new(
            "Primary expresion must contain a token".into(),
            primary_token.start_pos(),
        )
    )?;
    if let TokenKind::Identifier(ident_type) = &token.kind {
        Ok(Type {
            name: ident_type.clone(),
            subtypes: vec![],
            pos: token.into(),
        })
    } else {
        return Err(IzeErr::new(
            "Type must be an identifier token".into(),
            token.pos,
        ));
    }
}

/// Get the contents of a ExpressionKind::Type and generates a Type struct.
fn build_type_from_type_expr(ident_token: &AstNode, subtypes_vec: &AstNode) -> Result<Type, IzeErr> {
    let token = ident_token.token_ref().ok_or(
        IzeErr::new(
            "Type identifier must be a token".into(),
            ident_token.start_pos(),
        )
    )?;
    let ident_type = match &token.kind {
        TokenKind::List => "List".into(),
        TokenKind::Map => "Map".into(),
        TokenKind::Mux => "Mux".into(),
        TokenKind::Tuple => "Tuple".into(),
        TokenKind::Traf => "Traf".into(),
        TokenKind::Identifier(id) => id.clone(),
        _ => return Err(IzeErr::new(
            format!("Type has an unknown identifier token {:?}", token.kind),
            token.pos,
        ))
    };
    let subtypes_vec = subtypes_vec.vec_ref().ok_or(
        IzeErr::new(
            "Subtypes of a type expression must be a vector".into(),
            subtypes_vec.start_pos(),
        )
    )?;

    let mut subtypes = vec![];
    for subtype in subtypes_vec {
        if let AstNode::Expression(e) = subtype {
            if let ExpressionKind::Type { ident_token, subtypes_vec } = &e.kind {
                let type_obj = build_type_from_type_expr(ident_token, subtypes_vec)?;
                subtypes.push(type_obj);
            } else {
                return Err(IzeErr::new(
                    "Subtype is not a type expression".into(),
                    e.start_pos,
                ))
            }
        } else {
            return Err(IzeErr::new(
                "Subtype is not an expression".into(),
                subtype.start_pos(),
            ))
        }
    }

    let type_obj = Type {
        name: ident_type,
        subtypes,
        pos: token.into(),
    };

    Ok(type_obj)
}

/// Return identifier and body parts from a CommandKind::Model.
fn unwrap_model_cmd(cmd: &Command) -> Result<(String, &AstNode), IzeErr> {
    let (ident_token, body) = if let CommandKind::Model { ident_token, body } = &cmd.kind {
        (ident_token, body)
    } else {
        return Err(IzeErr::new(
            "Command is not a model".into(),
            cmd.start_pos,
        ));
    };
    let ident_token = ident_token.token_ref().ok_or(
        IzeErr::new(
            "Model identifier must be a token".into(),
            ident_token.start_pos(),
        )
    )?;
    let model_name = if let TokenKind::Identifier(id) = &ident_token.kind {
        id.clone()
    } else {
        return Err(IzeErr::new(
            "Model identifier must be an identifier token".into(),
            ident_token.pos,
        ));
    };

    Ok((model_name, body))
}

/// Generate a (String, Option<String>, Type) tuple from a Pair expression.
fn build_struct_tuple_from_pair_expr(pair_expr: &AstNode) -> Result<(Identifier, Option<Alias>, Type), IzeErr> {
    if let AstNode::Expression(pair_expr) = pair_expr {
        if let ExpressionKind::Pair { left_expr, alias_token, right_expr } = &pair_expr.kind {
            let identifier = if let AstNode::Expression(left_expr) = left_expr {
                if let ExpressionKind::Primary { token: AstNode::Token(token) } = &left_expr.kind {
                    if let TokenKind::Identifier(key) = &token.kind {
                        key.clone()
                    } else {
                        return Err(IzeErr::new(
                            "Token must be an identifier".into(),
                            token.pos,
                        ))   
                    }
                } else {
                    return Err(IzeErr::new(
                        "Left expression must be a primary expression".into(),
                        left_expr.start_pos,
                    ))
                }
            } else {
                return Err(IzeErr::new(
                    "Left part of pair must be an expression".into(),
                    left_expr.start_pos(),
                ))
            };

            let alias_opt = if let Some(alias_token) = alias_token {
                let alias_token = alias_token.token_ref().ok_or(
                    IzeErr::new(
                        "Alias must be a token".into(),
                        alias_token.start_pos(),
                    )
                )?;
                let alias = if let TokenKind::StringLiteral(alias) = &alias_token.kind {
                    alias.clone()
                } else {
                    return Err(IzeErr::new(
                        "Alias must be a string literal".into(),
                        left_expr.start_pos(),
                    ))
                };
                Some(alias)
            } else {
                None
            };

            let right_expr = right_expr.expr_ref().ok_or(
                IzeErr::new(
                    "Right expression must be an expression".into(),
                    left_expr.start_pos(),
                )
            )?;

            let type_obj = match &right_expr.kind {
                ExpressionKind::Primary { token } => build_type_from_primary_expr(token)?,
                ExpressionKind::Type { ident_token, subtypes_vec } => build_type_from_type_expr(ident_token, subtypes_vec)?,
                _ => return Err(IzeErr::new(
                    "Right expression must be either a Type or a Primary expression".into(),
                    left_expr.start_pos(),
                ))
            };

            Ok((identifier, alias_opt, type_obj))
        } else {
            return Err(IzeErr::new(
                "Each entry of a model vector body must be a pair expression".into(),
                pair_expr.start_pos,
            ))
        }
    } else {
        return Err(IzeErr::new(
            "Each entry of a model vector body must be an expression".into(),
            pair_expr.start_pos(),
        ))
    }
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
