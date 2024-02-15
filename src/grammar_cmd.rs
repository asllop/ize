//! # Command Grammars
//!
//! Contains the grammar rules to parse IZE commands.

use alloc::{boxed::Box, vec::Vec};

use crate::{
    ast::{Command, Expression, ExpressionKind, Identifier, ImportSymbol, Primary},
    err::IzeErr,
    grammar_expr::{expr, expr_call, expr_dot, expr_type},
    lexer::{Token, TokenKind},
    parser::{Parser::*, *},
};

/////////////////////
// Command parsers //
/////////////////////

/// Parse a command.
pub fn cmd(input: &[Token]) -> IzeResult {
    cmd_transfer(input)
}

/// Parse a Transfer command.
fn cmd_transfer(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::Transfer, 1),
            Fun(token_ident, 2),
            Opt(&[
                Fun(param_pair_expr, 3),
                Zero(&[Key(TokenKind::Comma, 4), Fun(param_pair_expr, 5)]),
            ]),
            Tk(TokenKind::Arrow, 6),
            Fun(expr_type, 7),
            Sel(&[
                Con(&[
                    Key(TokenKind::OpenParenth, 8),
                    Fun(transfer_body_struct, 9),
                    Tk(TokenKind::ClosingParenth, 10),
                ]),
                Fun(expr, 11),
            ]),
        ],
        |mut node_vec| {
            let body = node_vec.pop().unwrap();
            let ret_type = node_vec.pop().unwrap().expr().unwrap();
            let ret_type = if let ExpressionKind::Primary(Primary::Identifier(id)) = ret_type.kind {
                let pos = ret_type.pos;
                Expression::new_type(Identifier::new(id, pos), vec![], pos)
            } else {
                ret_type
            };
            node_vec.pop().unwrap().token().unwrap(); // Arrow token

            // Block of optional parameters
            let mut opt_args = node_vec.pop().unwrap().vec().unwrap();
            let params = if opt_args.is_empty() {
                vec![]
            } else {
                let params_vec = opt_args.pop().unwrap().vec().unwrap();
                let first_param = opt_args.pop().unwrap().expr().unwrap();
                let mut params = vec![first_param];
                for param in params_vec {
                    let mut param = param.vec().unwrap();
                    let pair = param.pop().unwrap().expr().unwrap();
                    params.push(pair);
                }
                params
            };

            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start; // token "transfer"

            if let ParseNode::Vec(mut body_vec) = body {
                // It's a struct body
                let end_pos = body_vec.pop().unwrap().token().unwrap().pos.end; // token ')'
                let pair_expr_vec = body_vec.pop().unwrap().vec().unwrap();
                let body = pair_expr_vec
                    .into_iter()
                    .map(|n| n.expr().unwrap())
                    .collect();
                Command::new_transfer_with_struct(
                    ident.try_into().unwrap(),
                    params,
                    ret_type,
                    body,
                    end_pos - start_pos,
                )
                .into()
            } else {
                // It's an expression body
                let body = body.expr().unwrap();
                let end_pos = body.pos.end;
                Command::new_transfer_with_expr(
                    ident.try_into().unwrap(),
                    params,
                    ret_type,
                    body,
                    end_pos - start_pos,
                )
                .into()
            }
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => cmd_model(input),
                // Errors
                2 => Err(
                    IzeErr::new("Expected transfer name, an identifier".into(), e.err.pos).into(),
                ),
                5 => Err(IzeErr::new(
                    format!("Expected parameter after comma: {}", e.err.message),
                    e.err.pos,
                )
                .into()),
                6 => Err(IzeErr::new("Expected arrow".into(), e.err.pos).into()),
                7 => Err(IzeErr::new("Expected return type".into(), e.err.pos).into()),
                9 => Err(IzeErr::new(
                    format!("Expected struct body: {}", e.err.message),
                    e.err.pos,
                )
                .into()),
                10 => Err(IzeErr::new("Expected ')".into(), e.err.pos).into()),
                11 => Err(IzeErr::new(
                    format!("Expected expression body: {}", e.err.message),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Transfer command.
fn cmd_model(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::Model, 1),
            Fun(token_ident, 2),
            Sel(&[
                Con(&[
                    Key(TokenKind::OpenParenth, 3),
                    Tk(TokenKind::ClosingParenth, 4),
                ]),
                Con(&[
                    Key(TokenKind::OpenParenth, 5),
                    Fun(model_body_struct, 6),
                    Tk(TokenKind::ClosingParenth, 7),
                ]),
                Fun(expr_type, 8),
            ]),
        ],
        |mut node_vec| {
            let body = node_vec.pop().unwrap();
            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start; // 'model' token
            if let ParseNode::Vec(mut body_struct) = body {
                let end_pos = body_struct.pop().unwrap().token().unwrap().pos.end; // token ')'
                                                                                   // Body is a struct
                let body = if body_struct.len() == 1 {
                    // Empty struct
                    vec![]
                } else {
                    // Struct with attributes
                    let pairs = body_struct.pop().unwrap().vec().unwrap();
                    pairs.into_iter().map(|n| n.expr().unwrap()).collect()
                };
                Command::new_model_with_struct(ident.try_into().unwrap(), body, end_pos - start_pos)
                    .into()
            } else {
                // Body is an expression
                let body = body.expr().unwrap();
                let end_pos = body.pos.end;
                Command::new_model_with_type(ident.try_into().unwrap(), body, end_pos - start_pos)
                    .into()
            }
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => cmd_const(input),
                // Errors
                2 => {
                    Err(IzeErr::new("Expected model name, an identifier".into(), e.err.pos).into())
                }
                4 | 7 => Err(IzeErr::new("Expected ')'".into(), e.err.pos).into()),
                6 => Err(IzeErr::new("Expected expression after '('".into(), e.err.pos).into()),
                8 => Err(IzeErr::new("Expected expression as model body".into(), e.err.pos).into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Const command.
fn cmd_const(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::Const, 1),
            Fun(token_ident, 2),
            Fun(token_literal, 3),
        ],
        |mut node_vec| {
            let value = node_vec.pop().unwrap().token().unwrap();
            let end_pos = value.pos.end;
            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start;
            Command::new_const(
                ident.try_into().unwrap(),
                value.try_into().unwrap(),
                end_pos - start_pos,
            )
            .into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => cmd_pipe(input),
                // Errors
                2 => Err(
                    IzeErr::new("Expected constant name, an identifier".into(), e.err.pos).into(),
                ),
                3 => {
                    Err(IzeErr::new("Expected constant value, a literal".into(), e.err.pos).into())
                }
                _ => Err(e),
            }
        },
    )
}

/// Parse a Pipe command.
fn cmd_pipe(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::Pipe, 1),
            Fun(token_ident, 2),
            Fun(pipe_body, 3),
        ],
        |mut node_vec| {
            let pipe_body = node_vec.pop().unwrap().expr().unwrap();
            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start;
            Command::new_pipe(ident.try_into().unwrap(), pipe_body, start_pos).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => cmd_run(input),
                // Errors
                2 => Err(IzeErr::new("Expected pipe name, an identifier".into(), e.err.pos).into()),
                3 => Err(
                    IzeErr::new(format!("Expected pipe body: {}", e.err.message), e.err.pos).into(),
                ),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Run command.
fn cmd_run(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::Run, 1),
            Sel(&[Fun(token_ident, 2), Fun(pipe_body, 3)]),
        ],
        |mut node_vec| {
            let pipe = node_vec.pop().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start;
            if let ParseNode::Token(_) = pipe {
                let pipe_ident = pipe.token().unwrap();
                Command::new_run_with_ident(pipe_ident.try_into().unwrap(), start_pos).into()
            } else {
                let pipe_body = pipe.expr().unwrap();
                Command::new_run_with_body(pipe_body, start_pos).into()
            }
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => cmd_import(input),
                // Errors
                2 => Err(IzeErr::new("Expected pipe name, an identifier".into(), e.err.pos).into()),
                3 => Err(
                    IzeErr::new(format!("Expected pipe body: {}", e.err.message), e.err.pos).into(),
                ),
                _ => Err(e),
            }
        },
    )
}

// Grammar: "import" ("*" | ID) ("as" ID)? ("," ID ("as" ID)?)* "from" expr_dot
/// Parse an Import command.
fn cmd_import(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::Import, 1),
            Sel(&[Tk(TokenKind::Star, 2), Fun(token_ident, 3)]),
            Opt(&[Key(TokenKind::As, 4), Fun(token_ident, 5)]),
            Zero(&[
                Tk(TokenKind::Comma, 6),
                Fun(token_ident, 7),
                Opt(&[Key(TokenKind::As, 8), Fun(token_ident, 9)]),
            ]),
            Tk(TokenKind::From, 10),
            Fun(expr_dot, 11),
        ],
        |mut node_vec| {
            let path = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // token "from"
            let following_imports = node_vec.pop().unwrap().vec().unwrap();
            let mut first_import_alias = node_vec.pop().unwrap().vec().unwrap();
            let first_import = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start;
            let end_pos = path.pos.end;

            let first_import = match first_import.kind {
                TokenKind::Star => Identifier::new("*".into(), first_import.pos),
                TokenKind::Identifier(id) => Identifier::new(id, first_import.pos),
                _ => panic!("First imported symbol must be either an identifier or a '*'"),
            };
            let first_import_alias = if first_import_alias.is_empty() {
                None
            } else {
                let alias = first_import_alias.pop().unwrap().token().unwrap();
                Some(Identifier::try_from(alias).unwrap())
            };
            let mut imports = vec![ImportSymbol::new(first_import, first_import_alias)];
            for import_vec in following_imports {
                let mut import_vec = import_vec.vec().unwrap();
                let mut import_alias_vec = import_vec.pop().unwrap().vec().unwrap();
                let import_id = import_vec.pop().unwrap().token().unwrap();
                let import_id = Identifier::try_from(import_id).unwrap();
                let import_alias = if import_alias_vec.is_empty() {
                    None
                } else {
                    let alias = import_alias_vec.pop().unwrap().token().unwrap();
                    Some(Identifier::try_from(alias).unwrap())
                };
                imports.push(ImportSymbol::new(import_id, import_alias));
            }
            Command::new_import(imports, path, end_pos - start_pos).into()
        },
        |_, e| match e.id {
            1 => Err(IzeErr::new("Unknown command".into(), e.err.pos).into()),
            //TODO: error handling
            _ => Err(e),
        },
    )
}

///////////////////////
// Support functions //
///////////////////////

/// Parse pipe body
fn pipe_body(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Sel(&[
            Con(&[
                Tk(TokenKind::OpenParenth, 1),
                Tk(TokenKind::ClosingParenth, 2),
            ]),
            Con(&[
                Tk(TokenKind::OpenParenth, 3),
                Fun(expr_call, 4),
                Zero(&[Key(TokenKind::Comma, 5), Fun(expr_call, 6)]),
                Tk(TokenKind::ClosingParenth, 7),
            ]),
        ])],
        |mut node_vec| {
            let mut node_vec = node_vec.pop().unwrap().vec().unwrap();
            let end_pos = node_vec.pop().unwrap().token().unwrap().pos.end; // token ')'
            if node_vec.len() == 1 {
                // Empty pipe
                let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start; // token '('
                Expression::new_pipe_body(vec![], end_pos - start_pos).into()
            } else {
                let pipe_expressions = node_vec.pop().unwrap().vec().unwrap();
                let first_pipe_expr = node_vec.pop().unwrap().expr().unwrap();
                let start_pos = node_vec.pop().unwrap().token().unwrap().pos.start; // token '('
                let mut pipe_body_vec = vec![first_pipe_expr];
                for pair in pipe_expressions {
                    let mut pair = pair.vec().unwrap();
                    let expr = pair.pop().unwrap().expr().unwrap();
                    pipe_body_vec.push(expr);
                }
                Expression::new_pipe_body(pipe_body_vec, end_pos - start_pos).into()
            }
        },
        |_, e| match e.id {
            1 | 3 => Err(IzeErr::new("Expected '('".into(), e.err.pos).into()),
            2 | 7 => Err(IzeErr::new("Expected ')'".into(), e.err.pos).into()),
            4 => Err(IzeErr::new("Expected expression after '('".into(), e.err.pos).into()),
            6 => Err(IzeErr::new("Expected expression after comma".into(), e.err.pos).into()),
            _ => Err(e),
        },
    )
}

/// Parse the struct body of a model.
fn model_body_struct(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(model_body_pair_expr, 1),
            Zero(&[Key(TokenKind::Comma, 2), Fun(model_body_pair_expr, 3)]),
        ],
        |mut node_vec| {
            let vec_of_pairs = node_vec.pop().unwrap().vec().unwrap();
            let first_pair = node_vec.pop().unwrap().expr().unwrap();
            let mut pairs = vec![first_pair.into()];
            for pair in vec_of_pairs {
                let mut pair = pair.vec().unwrap();
                let pair = pair.pop().unwrap();
                pairs.push(pair);
            }
            // Return a vec of pair expressions
            pairs.into()
        },
        |_, e| match e.id {
            1 | 3 => Err(IzeErr::new(
                format!("Expected pair attribute-type: {}", e.err.message),
                e.err.pos,
            )
            .into()),
            _ => Err(e),
        },
    )
}

/// Parse a Pair expression for model body.
fn model_body_pair_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(token_ident, 1),
            Opt(&[Key(TokenKind::As, 2), Fun(token_str, 3)]),
            Tk(TokenKind::Colon, 4),
            Sel(&[Fun(expr_type, 5), Tk(TokenKind::ThreeDots, 5)]),
        ],
        |mut node_vec| {
            let right_expr = match node_vec.pop().unwrap() {
                ParseNode::Token(token) => {
                    let pos = token.pos;
                    Expression::new_primary(token.try_into().unwrap(), pos)
                }
                ParseNode::Expression(right_expr) => right_expr,
                _ => panic!("Unexpected right expression"),
            };
            node_vec.pop().unwrap().token().unwrap(); // colon token

            let mut opt_as_alias = node_vec.pop().unwrap().vec().unwrap();
            if opt_as_alias.is_empty() {
                // No alias
                let left_ident = node_vec.pop().unwrap().token().unwrap();
                let pos = left_ident.pos;
                let left_expr =
                    Box::new(Expression::new_primary(left_ident.try_into().unwrap(), pos));
                Expression::new_pair(left_expr, right_expr.into()).into()
            } else {
                // Alias
                let alias = opt_as_alias.pop().unwrap().token().unwrap();
                let alias_pos = alias.pos;
                let alias = if let TokenKind::StringLiteral(s) = alias.kind {
                    Identifier::new(s, alias_pos)
                } else {
                    panic!("Token must be a string literal")
                };
                let left_ident = node_vec.pop().unwrap().token().unwrap();
                let pos = left_ident.pos;
                let left_expr =
                    Box::new(Expression::new_primary(left_ident.try_into().unwrap(), pos));
                Expression::new_pair_with_alias(left_expr, alias, right_expr.into()).into()
            }
        },
        |_, e| match e.id {
            1 => Err(IzeErr::new("Expected attribute identifier".into(), e.err.pos).into()),
            3 => Err(IzeErr::new("Expected alias string after 'as'".into(), e.err.pos).into()),
            4 => Err(IzeErr::new("Expected colon after attribute name".into(), e.err.pos).into()),
            5 => Err(IzeErr::new(
                format!("Expected expression as value: {}", e.err.message),
                e.err.pos,
            )
            .into()),
            _ => Err(e),
        },
    )
}

/// Parse the struct body of a transfer.
fn transfer_body_struct(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(transfer_body_pair_expr, 1),
            Zero(&[Key(TokenKind::Comma, 2), Fun(transfer_body_pair_expr, 3)]),
        ],
        |mut node_vec| {
            let vec_of_pairs = node_vec.pop().unwrap().vec().unwrap();
            let first_pair = node_vec.pop().unwrap().expr().unwrap();
            let mut pairs = vec![first_pair.into()];
            for pair in vec_of_pairs {
                let mut pair = pair.vec().unwrap();
                let pair = pair.pop().unwrap();
                pairs.push(pair);
            }
            // Return a vec of pair expressions
            pairs.into()
        },
        |_, e| match e.id {
            1 | 3 => Err(IzeErr::new(
                format!("Expected pair atttribute-expression: {}", e.err.message),
                e.err.pos,
            )
            .into()),
            _ => Err(e),
        },
    )
}

/// Parse a Pair expression for transfer parameters.
fn param_pair_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(token_ident, 1),
            Tk(TokenKind::Colon, 2),
            Fun(expr_type, 3),
        ],
        simple_pair_success,
        |_, e| match e.id {
            1 => Err(IzeErr::new("Expected identifier".into(), e.err.pos).into()),
            2 => Err(IzeErr::new("Expected colon after identifier".into(), e.err.pos).into()),
            3 => Err(IzeErr::new("Expected type after colon".into(), e.err.pos).into()),
            _ => Err(e),
        },
    )
}

/// Parse a Pair expression for transfer body.
fn transfer_body_pair_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Fun(token_ident, 1), Tk(TokenKind::Colon, 2), Fun(expr, 3)],
        simple_pair_success,
        |_, e| match e.id {
            1 => Err(IzeErr::new("Expected identifier".into(), e.err.pos).into()),
            2 => Err(IzeErr::new("Expected colon after identifier".into(), e.err.pos).into()),
            3 => Err(IzeErr::new("Expected expression after colon".into(), e.err.pos).into()),
            _ => Err(e),
        },
    )
}

/// Collect Pair expression for grammar: IDENTIFIER ":" EXPRESSION
fn simple_pair_success(mut node_vec: Vec<ParseNode>) -> ParseNode {
    let right_expr = node_vec.pop().unwrap().expr().unwrap();
    node_vec.pop().unwrap().token().unwrap(); // colon token
    let left_ident = node_vec.pop().unwrap().token().unwrap();
    let pos = left_ident.pos;
    let left_expr = Box::new(Expression::new_primary(left_ident.try_into().unwrap(), pos));
    Expression::new_pair(left_expr, right_expr.into()).into()
}
