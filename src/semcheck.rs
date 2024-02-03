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
    ast::{AstNode, Command, CommandKind, ExpressionKind},
    err::IzeErr,
    lexer::{TokenKind, TokenPos},
};
use alloc::{string::String, vec::Vec};
use rustc_hash::FxHashMap;

type Identifier = String;
type Alias = String;

//TODO: imports must fill the ST with the symbols imported from other modules.
//      Or the checker must inspect the ST of the other ASTs (we have an AST per module/file).

//TODO: chain expressions define a scope for variables. Chains can contain other chains, thhat inherit the parent scope plus adding its own.

/// Table of symbols present in the AST.
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub symbols: FxHashMap<Identifier, Symbol>,
}

/// Symbol information and metadata.
#[derive(Debug)]
pub enum Symbol {
    Model {
        body: ModelBody,
        start_pos: TokenPos,
    },
    //TODO: transfers, with environments and scopes for variables (let symbols)
    Transfer,
    //TODO: pipe
    Pipe,
    Const {
        value_type: LiteralType,
        start_pos: TokenPos,
    },
}

/// Types of literals. Used by const symbols.
#[derive(Debug)]
pub enum LiteralType {
    String,
    Integer,
    Float,
    Boolean,
    None,
    Null,
}

/// Model content.
#[derive(Debug)]
pub enum ModelBody {
    Alias(Type),
    Struct(FxHashMap<Identifier, (Option<Alias>, Type)>),
}

/// Representation of a type.
#[derive(Debug, Default)]
pub struct Type {
    pub name: String,
    pub subtypes: Vec<Type>,
    pub start_pos: TokenPos,
}

/// Check AST validity.
pub fn check_ast(ast: &[AstNode]) -> Result<SymbolTable, IzeErr> {
    let mut symtab = SymbolTable::default();
    for node in ast {
        match node {
            AstNode::Command(cmd) => match &cmd.kind {
                CommandKind::Model { .. } => check_model_cmd(&mut symtab, cmd)?,
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

//TODO: Two stages:
//  1. Put all models in the ST.
//  2. Check for invalid types inspecting the ST (unknown types, incorrect subtypes, etc).
fn check_model_cmd(symtab: &mut SymbolTable, cmd: &Command) -> Result<(), IzeErr> {
    let (ident_token, body) = if let CommandKind::Model { ident_token, body } = &cmd.kind {
        (ident_token, body)
    } else {
        return Err(IzeErr::new(
            "Command is not a model".into(),
            cmd.start_pos,
        ));
    };
    let model_name = if let Some(tk) = ident_token.token_ref() {
        tk
    } else {
        return Err(IzeErr::new(
            "Model identifier must be a token".into(),
            ident_token.start_pos(),
        ));
    };
    let model_name = if let TokenKind::Identifier(id) = &model_name.kind {
        id.clone()
    } else {
        return Err(IzeErr::new(
            "Model identifier must be an identifier token".into(),
            ident_token.start_pos(),
        ));
    };
    match body {
        AstNode::Expression(expr) => match &expr.kind {
            ExpressionKind::Primary {
                token: AstNode::Token(token),
            } => {
                if let TokenKind::Identifier(ident_type) = &token.kind {
                    // Create entry in the symbol table
                    let symbol = Symbol::Model {
                        body: ModelBody::Alias(Type {
                            name: ident_type.clone(),
                            subtypes: vec![],
                            start_pos: token.pos,
                        }),
                        start_pos: cmd.start_pos,
                    };
                    symtab.symbols.insert(model_name, symbol);
                } else {
                    return Err(IzeErr::new(
                        "Model type must be an identifier token".into(),
                        token.pos,
                    ));
                }
            }
            ExpressionKind::Type {
                ident_token,
                subtypes_vec,
            } => {
                let type_obj = build_type(ident_token, subtypes_vec)?;
                // Create entry in the symbol table
                let symbol = Symbol::Model {
                    body: ModelBody::Alias(type_obj),
                    start_pos: cmd.start_pos,
                };
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
            //TODO: generate model symbol from vector body
            let mut model_struct = FxHashMap::default();

            for pair_expr in pairs_vec {
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

                        //TODO: alias_token must be an identifier token

                        //TODO: right_expr must be either a type or a primary identifier expression
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
                };
            }

            // Create entry in the symbol table
            let symbol = Symbol::Model {
                body: ModelBody::Struct(model_struct),
                start_pos: cmd.start_pos,
            };
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

fn build_type(ident_token: &AstNode, subtypes_vec: &AstNode) -> Result<Type, IzeErr> {
    let token = if let Some(token) = ident_token.token_ref() {
        token
    } else {
        return Err(IzeErr::new(
            "Model identifier must be a token".into(),
            ident_token.start_pos(),
        ));
    };
    let ident_type = match &token.kind {
        TokenKind::List => "List".into(),
        TokenKind::Map => "Map".into(),
        TokenKind::Mux => "Mux".into(),
        TokenKind::Tuple => "Tuple".into(),
        TokenKind::Traf => "Traf".into(),
        TokenKind::Identifier(id) => id.clone(),
        _ => return Err(IzeErr::new(
            format!("Model has an unknown type identifier token {:?}", token.kind),
            token.pos,
        ))
    };
    let subtypes_vec = if let AstNode::Vec(v) = subtypes_vec {
        v
    } else {
        return Err(IzeErr::new(
            "Subtypes of a type expression must be a vector".into(),
            subtypes_vec.start_pos(),
        ))
    };

    let mut subtypes = vec![];
    for subtype in subtypes_vec {
        if let AstNode::Expression(e) = subtype {
            if let ExpressionKind::Type { ident_token, subtypes_vec } = &e.kind {
                let type_obj = build_type(ident_token, subtypes_vec)?;
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
        start_pos: token.pos,
    };

    Ok(type_obj)
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
