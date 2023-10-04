//! Semantic Analyzer
//!
//! Performs semantic analysis: symbol resolution (models, transfers, variables, and pipes), and type checking of all operations.

//TODO: traverse the AST
//TODO: perform as many checks as possible on each AST pass
//TODO: build a symbol table, that contains all synbols defined on each scope, in a hasmap for fast search.

use crate::{
    ast::{Ast, Command, CommandSet, FieldType, ModelType, Pipe, Transfer, Type, TypeId},
    BuildErr, IzeErr, Pos,
};

/// Check semantics.
pub fn sem_check(ast: &Ast) -> Result<(), IzeErr> {
    traverse_ast(ast)?;
    Ok(())
}

fn traverse_ast(ast: &Ast) -> Result<(), IzeErr> {
    for command in ast.commands.iter() {
        check_command(ast, command)?;
    }
    for ast in ast.imports.values() {
        traverse_ast(ast)?;
    }
    Ok(())
}

fn check_command(ast: &Ast, command: &Command) -> Result<(), IzeErr> {
    match &command.command {
        CommandSet::Model(model) => check_model(ast, &model.model_type),
        CommandSet::Transfer(transfer) => check_transfer(ast, transfer),
        CommandSet::Pipe(pipe) => check_pipe(ast, pipe),
        CommandSet::Import(_) => {
            Result::ize_err("Unexpected import command in the AST".into(), command.pos)
        }
    }
}

/// Check types of a model.
fn check_model(ast: &Ast, model_type: &ModelType) -> Result<(), IzeErr> {
    match model_type {
        ModelType::Struct(struct_model) => {
            let mut num_remains = 0;
            for (field_name, model_field) in &struct_model.fields {
                match &model_field.field_type {
                    FieldType::Remain => {
                        if num_remains > 0 {
                            //PROBLEM: Pos not available, we should put a pos on every AST component, not only top level ones.
                            Result::ize_err(
                                "Only one '...' field allowed.".into(),
                                Pos::default(),
                            )?;
                        }
                        num_remains += 1;
                        if struct_model.field_order.last() != Some(field_name) {
                            //PROBLEM: Pos not available, we should put a pos on every AST component, not only top level ones.
                            Result::ize_err(
                                "Field '...' must come the last.".into(),
                                Pos::default(),
                            )?;
                        }
                        if model_field.rename.is_some() {
                            //PROBLEM: Pos not available, we should put a pos on every AST component, not only top level ones.
                            Result::ize_err(
                                "Field '...' can't have an alias.".into(),
                                Pos::default(),
                            )?;
                        }
                    }
                    FieldType::Type(model_field_type) => {
                        check_null_or_none(model_field_type)?;
                        check_type(ast, model_field_type)?
                    },
                }
            }
            Ok(())
        }
        ModelType::Alias(alias_model) => {
            check_null_or_none(alias_model)?;
            check_type(ast, alias_model)
        },
    }
}

fn check_transfer(ast: &Ast, transfer: &Transfer) -> Result<(), IzeErr> {
    todo!("check_transfer")
}

fn check_pipe(ast: &Ast, pipe: &Pipe) -> Result<(), IzeErr> {
    todo!("check_pipe")
}

//TODO: Check type integrity:
//  - Mux contains no repeated types.
//  - List contains only one type.
//  - Map contains one or two types.
//  - Defined type is in the symbol table
// NOTE: types can be a chain like: "my.mod.MyType"
fn check_type(ast: &Ast, ize_type: &Type) -> Result<(), IzeErr> {
    match &ize_type.id {
        TypeId::Custom(custom_type) => {
            if !ize_type.inner.is_empty() {
                Result::ize_err(
                    "Custom types can't contain compound types".into(),
                    //TODO: get actual pos
                    Pos::default(),
                )
            } else {
                if !ast.symbols.contains_key(custom_type) {
                    Result::ize_err(
                        "Custom type not defined".into(),
                        //TODO: get actual pos
                        Pos::default(),
                    )
                } else {
                    Ok(())
                }
            }
        },
        TypeId::String
        | TypeId::Integer
        | TypeId::Float
        | TypeId::Boolean
        | TypeId::Null
        | TypeId::None
        | TypeId::Any => {
            if !ize_type.inner.is_empty() {
                Result::ize_err(
                    "Base types can't contain compound types".into(),
                    //TODO: get actual pos
                    Pos::default(),
                )
            } else {
                Ok(())
            }
        }
        //TODO: check compound types integrity
        TypeId::List => todo!(),
        TypeId::Map => todo!(),
        TypeId::Mux => todo!(),
        TypeId::Tuple => todo!(),
    }
}

fn check_null_or_none(ize_type: &Type) -> Result<(), IzeErr> {
    match &ize_type.id {
        TypeId::None | TypeId::Null => {
            Result::ize_err(
                "Null or None types not permitted in this position".into(),
                //TODO: get actual pos
                Pos::default(),
            )
        },
        _ => Ok(())
    }
}

//TODO: Check reserved identifiers: no let, model, transfer or pipe is named "in" or "New".

//TODO: Check model integrity:
//  - Only one "..." field that comes the last.
//  - No field is of type None.
//  - Custom types actually exist.

//TODO: Check transfer integrity:
//  - Input and output custom types actually exist.
//  - No input and output type is None.

//TODO: Calculate the resulting type of every expression and annotate it in the AST.

//TODO: Check if the types match as they should:
//  - Math/logic operations are performed using the correct types.
//  - If-else condition contains an expression of type Boolean.
//  - If and else bodies are expressions of the same type.
//  - Select/Unwrap arms are expressions of the same type.
//  - Select's condition and arm's left side are of the same type.
//  - Resulting type of expressions matches the parent expression (transfer output, model structure, select/unwrap/if-else result, etc).

//TODO: Check variables:
//  - They exist when used and the type is correct.

//TODO: Unwrap:
//  - Left side of arm is a type.
//  - All possibilities are covered with corresponding arms.

//TODO: Select
//  - Left side of arm is a literal.
//  - All possibilities are covered with corresponding arms.

//TODO: Create a way to check method type checking.
//  - For exaple "any_string.Match()" must contain a single argument of type String.
//  - Not only for internal methods, also for extensions made in Rust (see "ext" module).

//TODO: Check dot expressions and function calls.
//  - All components of the dot actually exist.
//  - All function calls actually exist and the argument number of type are correct.
