//! # Semantic checker
//!
//! The semantic checker has two main jobs: Ensure that an IZE program is semantically correct, and generate a symbol table.
//!
//! Semchecking is the last step before code generation.

use alloc::{borrow::ToOwned, string::String};
use rustc_hash::FxHashSet;

use crate::{
    ast::{
        Ast, BinaryOp, Command, CommandKind, Expression, ExpressionKind, Literal, ModelBody,
        Primary, UnaryOp,
    },
    err::IzeErr,
    pos::RangePos,
    symbol_table::*,
};

/// Check AST validity.
pub fn check_ast(ast: &Ast) -> Result<SymbolTable, IzeErr> {
    let mut sym_tab = SymbolTable::default();

    check_imported_symbols(ast, &mut sym_tab)?;
    insert_symbols(ast, &mut sym_tab)?;

    // Check commands in the order they appear.
    for cmd in &ast.commands {
        match &cmd.kind {
            CommandKind::Model { body, .. } => check_model(body, &mut sym_tab)?,
            CommandKind::Transfer { .. } => {
                //TODO: check transfer!!
                //TODO: expression type evaluator: calculates the resulting type of an expression
                //TODO: for struct transfers, check that return type is a struct model and atttributes and types match.
            }
            CommandKind::Pipe { .. } => {}
            CommandKind::Run(_) => {}
            _ => {}
        }
    }

    Ok(sym_tab)
}

/// TODO: Expression Type Evaluator.
fn expr_type_eval(expr: &Expression) -> Result<Type, IzeErr> {
    match &expr.kind {
        ExpressionKind::Primary(p) => match p {
            Primary::Identifier(id) => todo!("Search id in the ST and return type"),
            Primary::Literal(lit) => match lit {
                Literal::String(_) => Ok(Type::new(STR_TYPE.into(), vec![])),
                Literal::Integer(_) => Ok(Type::new(INT_TYPE.into(), vec![])),
                Literal::Float(_) => Ok(Type::new(FLOAT_TYPE.into(), vec![])),
                Literal::Boolean(_) => Ok(Type::new(BOOL_TYPE.into(), vec![])),
                Literal::Null => Ok(Type::new(NULL_TYPE.into(), vec![])),
                Literal::None => Ok(Type::new(NONE_TYPE.into(), vec![])),
            },
        },
        ExpressionKind::Group(expr) => expr_type_eval(expr),
        ExpressionKind::Let { expr, .. } => expr_type_eval(expr),
        ExpressionKind::Chain(expr_vec) => {
            if let Some((last, rest)) = expr_vec.split_last() {
                for expr in rest {
                    expr_type_eval(expr)?;
                }
                expr_type_eval(last)
            } else {
                Err(IzeErr::new("Chain expression is empty".into(), expr.pos))
            }
        }
        ExpressionKind::IfElse {
            cond,
            if_expr,
            else_expr,
        } => {
            if expr_type_eval(cond)?.ident != BOOL_TYPE {
                return Err(IzeErr::new(
                    "If-else condition must evaluate into a Bool type".into(),
                    cond.pos,
                ));
            }
            let if_expr_type = expr_type_eval(if_expr)?;
            let else_expr_type = expr_type_eval(else_expr)?;
            if if_expr_type == else_expr_type {
                Ok(if_expr_type)
            } else {
                // NOTE:
                // We generate a provisional Mux with the two types. Later steps will check if the provisional Mux is compatible
                // with the requiered type. For example, Mux[Str,Int] is compatible with a transfer returning Mux[Str,Int,Float],
                // because the subtypes (Str,Int) are a subset of the required subtypes (Str,Int,Float).
                // Cases: transfer return type, transfer parameter type.
                Ok(Type::new(
                    MAP_TYPE.into(),
                    vec![if_expr_type, else_expr_type],
                ))
            }
        }
        ExpressionKind::Binary { op, left, right } => {
            let left_expr_type = expr_type_eval(left)?;
            let right_expr_type = expr_type_eval(right)?;

            if left_expr_type != right_expr_type {
                return Err(IzeErr::new(
                    "Left and right expressions must evaluate into the same type".into(),
                    left.pos,
                ));
            }
            let ident = &left_expr_type.ident;
            match op {
                // Can only be applied to Str, Int, and Float and return the same type of arguments
                BinaryOp::Plus => {
                    if ident != STR_TYPE && ident != INT_TYPE && ident != FLOAT_TYPE {
                        return Err(IzeErr::new(
                            "Plus operator con only be applied to Str, Int and Float types".into(),
                            left.pos,
                        ));
                    }
                    Ok(left_expr_type)
                }
                // Can only be applied to Int, and Float and return the same type of arguments
                BinaryOp::Minus | BinaryOp::Star | BinaryOp::Slash | BinaryOp::Percent => {
                    if ident != INT_TYPE && ident != FLOAT_TYPE {
                        return Err(IzeErr::new(
                            "Aritmetic operators con only be applied to Int and Float types".into(),
                            left.pos,
                        ));
                    }
                    Ok(left_expr_type)
                }
                // Can only be applied to Int, and Float and return bool type
                BinaryOp::LesserThan
                | BinaryOp::GreaterThan
                | BinaryOp::GtEqual
                | BinaryOp::LtEqual => {
                    if ident != INT_TYPE && ident != FLOAT_TYPE {
                        return Err(IzeErr::new("Ordering comparation operators con only be applied to Int and Float types".into(), left.pos));
                    }
                    Ok(Type::new(BOOL_TYPE.into(), vec![]))
                }
                // Can be applied to any type and return bool type
                BinaryOp::NotEqual | BinaryOp::TwoEquals => Ok(left_expr_type),
                // Can only be applied to int and bool and return the same type of arguments
                BinaryOp::And | BinaryOp::Or => {
                    if ident != INT_TYPE && ident != BOOL_TYPE {
                        return Err(IzeErr::new(
                            "Logic operators con only be applied to Int and Bool types".into(),
                            left.pos,
                        ));
                    }
                    Ok(left_expr_type)
                }
                // Can only be applied to bool type and return bool type
                BinaryOp::TwoAnds | BinaryOp::TwoOrs => {
                    if ident != BOOL_TYPE {
                        return Err(IzeErr::new(
                            "Logic comparation operators con only be applied to Bool type".into(),
                            left.pos,
                        ));
                    }
                    Ok(Type::new(BOOL_TYPE.into(), vec![]))
                }
            }
        }
        ExpressionKind::Unary { op, expr } => match op {
            UnaryOp::Minus => {
                let expr_type = expr_type_eval(expr)?;
                if expr_type.ident != INT_TYPE && expr_type.ident != FLOAT_TYPE {
                    return Err(IzeErr::new(
                        "Minus operator con only be applied to Int and Float types".into(),
                        expr.pos,
                    ));
                }
                Ok(expr_type)
            }
            UnaryOp::Not => {
                let expr_type = expr_type_eval(expr)?;
                if expr_type.ident != INT_TYPE && expr_type.ident != BOOL_TYPE {
                    return Err(IzeErr::new(
                        "Not operator con only be applied to Int and Bool types".into(),
                        expr.pos,
                    ));
                }
                Ok(expr_type)
            }
        },
        ExpressionKind::Call { ident, args } => todo!(),
        ExpressionKind::Dot(_) => todo!(),
        ExpressionKind::SelectUnwrap {
            op,
            expr,
            alias,
            arms,
        } => todo!(),
        ExpressionKind::Arm { left, right } => todo!(),
        ExpressionKind::Type { ident, subtypes } => todo!(),
        // Invalid
        ExpressionKind::Pair { .. } => Err(IzeErr::new(
            "ETE can't be used over a Pair expression".into(),
            expr.pos,
        )),
        ExpressionKind::PipeBody(_) => Err(IzeErr::new(
            "ETE can't be used over a PipeBody expression".into(),
            expr.pos,
        )),
    }
}

/// Scan each command and insert an entry into the ST, checking for duplicated symbols and reserved words.
fn insert_symbols(ast: &Ast, sym_tab: &mut SymbolTable) -> Result<(), IzeErr> {
    for cmd in &ast.commands {
        match &cmd.kind {
            CommandKind::Model { ident, .. } => {
                let sym = ident.id.clone();
                check_is_reserved_word(&sym, cmd.pos)?;
                sym_tab.insert(sym.clone(), Symbol::default_model(), cmd.pos)?;
            }
            CommandKind::Transfer { ident, .. } => {
                let sym = ident.id.clone();
                check_is_reserved_word(&sym, cmd.pos)?;
                sym_tab.insert(sym.clone(), Symbol::default_transfer(), cmd.pos)?;
            }
            CommandKind::Pipe { ident, .. } => {
                let sym = ident.id.clone();
                check_is_reserved_word(&sym, cmd.pos)?;
                sym_tab.insert(sym.clone(), Symbol::default_pipe(), cmd.pos)?;
            }
            CommandKind::Const { ident, .. } => {
                let sym = ident.id.clone();
                check_is_reserved_word(&sym, cmd.pos)?;
                sym_tab.insert(sym.clone(), Symbol::default_const(), cmd.pos)?;
            }
            _ => {}
        }
    }
    Ok(())
}

/// Check imports from an AST.
/// - Check that each imported symbol actually exists in the imported AST.
/// - Insert impored symbols into the Symbol Table and check for duplicated.
fn check_imported_symbols(ast: &Ast, sym_tab: &mut SymbolTable) -> Result<(), IzeErr> {
    for ((sym, _), import_ref) in &ast.imported_symbols {
        if is_reserved_word(sym) {
            return Err(IzeErr::new(
                format!("Imported symbol is a reserved identifier: {sym}"),
                import_ref.pos,
            ));
        }
        let import_ast = &ast.imports[import_ref.ast_index];
        // Check that imported symbol actually exists in the imported AST.
        if let Some(sym_data) = find_symbol_in_ast(sym, import_ast) {
            let real_sym = sym.clone();
            let sym = if let Some(rename) = &import_ref.rename {
                if is_reserved_word(rename) {
                    return Err(IzeErr::new(
                        format!("Rename symbol is a reserved identifier: {rename} "),
                        import_ref.pos,
                    ));
                }
                rename.clone()
            } else {
                sym.clone()
            };
            // Insert impored symbol into the Symbol Table, or return an error if it's duplicated.
            sym_tab.insert(
                sym,
                Symbol::new_imported(real_sym, sym_data),
                import_ref.pos,
            )?;
        } else {
            return Err(IzeErr::new(
                format!(
                    "Symbol {sym} not found in imported module {}",
                    import_ast.file_path
                ),
                import_ref.pos,
            ));
        }
    }
    Ok(())
}

/// Model Check.
/// - Check that referenced types actually exist.
/// - Check struct integrity: unique attribute names, unique alias names, only one "..." field.
fn check_model(body: &ModelBody, sym_tab: &mut SymbolTable) -> Result<(), IzeErr> {
    match body {
        ModelBody::Type(type_expr) => match &type_expr.kind {
            ExpressionKind::Primary(Primary::Identifier(ident)) => {
                check_type_exists(ident, None, sym_tab, type_expr.pos)?;
            }
            ExpressionKind::Type { ident, subtypes } => {
                check_type_exists(&ident.id, Some(subtypes), sym_tab, type_expr.pos)?;
            }
            _ => {
                return Err(IzeErr::new(
                    "ModelBody::Type must contain a Type expression or a Primary identifier".into(),
                    type_expr.pos,
                ));
            }
        },
        ModelBody::Struct(pairs) => {
            let mut three_dots_found = false;
            let mut field_names = FxHashSet::default();
            let mut field_aliases = FxHashSet::default();
            for pair in pairs {
                if let ExpressionKind::Pair { left, alias, right } = &pair.kind {
                    // Look for duplicated field aliases
                    if let Some(alias) = alias {
                        if field_aliases.contains(&alias.id) {
                            return Err(IzeErr::new("Duplicated field alias".into(), alias.pos));
                        }
                        field_aliases.insert(alias.id.clone());
                    }
                    // Look for duplicated field names
                    if let ExpressionKind::Primary(Primary::Identifier(name)) = &left.kind {
                        if field_names.contains(name) {
                            return Err(IzeErr::new("Duplicated field name".into(), left.pos));
                        }
                        field_names.insert(name.clone());
                    } else {
                        return Err(IzeErr::new(
                            "Field name must be a primary identifier".into(),
                            left.pos,
                        ));
                    }

                    match &right.kind {
                        ExpressionKind::Primary(Primary::Identifier(ident)) => {
                            // Look for duplicated '...' fields
                            if ident == "..." {
                                if three_dots_found {
                                    return Err(IzeErr::new(
                                        "Only one '...' field allowed".into(),
                                        right.pos,
                                    ));
                                }
                                three_dots_found = true;
                            }

                            check_type_exists(ident, None, sym_tab, right.pos)?;
                        }
                        ExpressionKind::Type { ident, subtypes } => {
                            check_type_exists(&ident.id, Some(subtypes), sym_tab, right.pos)?;
                        }
                        _ => {
                            return Err(IzeErr::new(
                                "Pair right must contain a Type expression or a Primary identifier"
                                    .into(),
                                right.pos,
                            ));
                        }
                    }
                } else {
                    return Err(IzeErr::new(
                        "ModelBody::Struct must contain Pair expressions".into(),
                        pair.pos,
                    ));
                }
            }
        }
    }
    Ok(())
}

/// Check that a type is either a primitive type or is defined in the ST.
fn check_type_exists(
    ident: &str,
    subtypes: Option<&[Expression]>,
    sym_tab: &mut SymbolTable,
    pos: RangePos,
) -> Result<(), IzeErr> {
    // Type is either a primitive or is in the ST
    if !is_primitive_type(ident) && !sym_tab.contains_model(ident) {
        return Err(IzeErr::new(format!("Undefined type: {}", ident), pos));
    }
    if let Some(subtypes) = subtypes {
        for type_expr in subtypes {
            if let ExpressionKind::Type { ident, subtypes } = &type_expr.kind {
                check_type_exists(&ident.id, Some(subtypes), sym_tab, type_expr.pos)?;
            } else {
                return Err(IzeErr::new(
                    "Subtypes must be Type expressions".into(),
                    type_expr.pos,
                ));
            }
        }
        // Check compound types.
        match ident {
            // Map must contain two subtypes, and the first must be Str or Int.
            // NOTE: we would like to use any type as a key, but we can't use floats or anything that contains a float:
            // https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust
            // In the future, we could improve it and use types without floats, but meanwhile we only allow Str and Int.
            "Map" => {
                if subtypes.len() != 2 {
                    return Err(IzeErr::new(
                        "Map must have two subtypes, key and value".into(),
                        pos,
                    ));
                }
                let key_type = &subtypes[0];
                if let ExpressionKind::Type { ident, .. } = &key_type.kind {
                    if ident.id != "Str" && ident.id != "Int" {
                        return Err(IzeErr::new(
                            "Map key must be either Str or Int".into(),
                            key_type.pos,
                        ));
                    }
                } else {
                    return Err(IzeErr::new(
                        "Subtypes must be Type expressions".into(),
                        key_type.pos,
                    ));
                }
            }
            // List only one subtype.
            "List" => {
                if subtypes.len() != 1 {
                    return Err(IzeErr::new("List must have one subtype".into(), pos));
                }
            }
            // Mux subtypes can't be duplicated.
            "Mux" => {
                let mut subtype_ids = FxHashSet::default();
                for type_expr in subtypes {
                    if let ExpressionKind::Type { ident, subtypes } = &type_expr.kind {
                        // Convert each type into a string representation we can use as a map key.
                        let subtype_repr = stringify_type(&ident.id, subtypes)?;
                        if subtype_ids.contains(&subtype_repr) {
                            return Err(IzeErr::new(
                                "Mux contains a duplicated subtype".into(),
                                type_expr.pos,
                            ));
                        }
                        subtype_ids.insert(subtype_repr);
                    } else {
                        return Err(IzeErr::new(
                            "Subtypes must be Type expressions".into(),
                            type_expr.pos,
                        ));
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}

//TODO: rewrite this using split_last:
// https://users.rust-lang.org/t/how-to-check-whether-this-is-the-last-item-in-an-iterator/105612/3
// https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=f95a7586388b9bab06d2b66674227177
/// Convert type expression into string representation.
fn stringify_type(ident: &str, subtypes: &[Expression]) -> Result<String, IzeErr> {
    let mut serialized = ident.to_owned() + "[";
    for (index, subtype_expr) in subtypes.iter().enumerate() {
        if let ExpressionKind::Type {
            ident,
            subtypes: next_subtype,
        } = &subtype_expr.kind
        {
            let s = stringify_type(&ident.id, next_subtype)?;
            serialized += s.as_str();
            if index < subtypes.len() - 1 {
                serialized += ",";
            }
        } else {
            return Err(IzeErr::new(
                "Subtypes must be Type expressions".into(),
                subtype_expr.pos,
            ));
        }
    }
    serialized += "]";
    Ok(serialized)
}

/// Search a symbo in an AST and returns the symbol kind if it is found.
fn find_symbol_in_ast(sym: &str, ast: &Ast) -> Option<SymbolData> {
    for cmd in &ast.commands {
        match &cmd.kind {
            CommandKind::Transfer { ident, .. } => {
                if ident.id == sym {
                    return Some(SymbolData::Transfer(Default::default()));
                }
            }
            CommandKind::Model { ident, .. } => {
                if ident.id == sym {
                    return Some(SymbolData::Model(Default::default()));
                }
            }
            CommandKind::Pipe { ident, .. } => {
                if ident.id == sym {
                    return Some(SymbolData::Pipe(Default::default()));
                }
            }
            CommandKind::Const { ident, .. } => {
                if ident.id == sym {
                    return Some(SymbolData::Const(Default::default()));
                }
            }
            CommandKind::Run(_) => {}
            CommandKind::Import { .. } => panic!("Ast.commands can't contain an import"),
        }
    }
    None
}

/// Check if symbol is a primitive type.
fn is_primitive_type(sym: &str) -> bool {
    matches!(
        sym,
        STR_TYPE
            | INT_TYPE
            | FLOAT_TYPE
            | BOOL_TYPE
            | NONE_TYPE
            | NULL_TYPE
            | ANY_TYPE
            | MAP_TYPE
            | LIST_TYPE
            | MUX_TYPE
            | TRAF_TYPE
            | TUPLE_TYPE
            | OTHER_TYPE
    )
}

/// Check if identifier is a reserved word.
fn is_reserved_word(id: &str) -> bool {
    matches!(
        id,
        STR_TYPE | INT_TYPE | FLOAT_TYPE | BOOL_TYPE | NONE_TYPE | NULL_TYPE | ANY_TYPE
    )
}

fn check_is_reserved_word(id: &str, pos: RangePos) -> Result<(), IzeErr> {
    if is_reserved_word(id) {
        Err(IzeErr::new(format!("Symbol is a reserved word: {id}"), pos))
    } else {
        Ok(())
    }
}

/// Check import commands. It's a pre-check, before we have the AST.
/// - Check that "path" is a Dot expr and contains only Primary identifiers.
/// - Check that imports with "*" only contain that symbol and no rename.
/// - Check for duplicated imported symbols.
pub fn check_import_commands(commands: &[Command]) -> Result<(), IzeErr> {
    let mut imported_syms = FxHashSet::default();
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
            if symbols.is_empty() {
                return Err(IzeErr::new(
                    "Imported symbols must contain at least one element".into(),
                    cmd.pos,
                ));
            }
            if symbols.len() == 1 {
                // Make sure it's a "*", and no rename present
                let sym = &symbols[0];
                if sym.symbol.id == "*" && sym.rename.is_some() {
                    return Err(IzeErr::new(
                        "Imported '*' can't include a rename".into(),
                        cmd.pos,
                    ));
                }
                // TODO: implement importing all symbols
                if sym.symbol.id == "*" {
                    return Err(IzeErr::new(
                        "Importing '*' is not implemented yet".into(),
                        cmd.pos,
                    ));
                }
            }
            for sym in symbols {
                if symbols.len() > 1 {
                    // Make sure there's no "*" symbol here
                    if sym.symbol.id == "*" {
                        return Err(IzeErr::new("Imported '*' must be alone".into(), cmd.pos));
                    }
                }
                // Look for duplicated symbols
                let actual_sym = if let Some(rename) = &sym.rename {
                    rename
                } else {
                    &sym.symbol
                };
                if imported_syms.contains(&actual_sym.id) {
                    return Err(IzeErr::new(
                        format!("Duplicated imported symbol: {}", actual_sym.id),
                        actual_sym.pos,
                    ));
                }
                imported_syms.insert(actual_sym.id.clone());
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

/*

- Build symbol table (ST):
    Scan de AST and add each model, transfer, pipe, and const identifier to a hashmap. Also scan transfer bodies looking for variables.
    Each entry will contain the entity type (model, transfer, pipe, const, let), and the types involved.
    - pipe, no types.
    - transfer, the signature, parameters and return types.
    - const, the literal type.
    - let, the type is not explicit, we have to calculate it from the expression used to define the variable. We need an Expression Type Evaluator (ETE).
*/
//- Expression Type Evaluator (ETE), given an expression, calculate the resulting type. It requires a ST already filled with explicit type symbols.
//- Use the ETE to fill the ST with non-explicit type symbols (lets).
