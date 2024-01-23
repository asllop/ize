//! # Command Grammars
//!
//! Contains the grammar rules to parse IZE commands.

use alloc::{boxed::Box, vec::Vec};

use crate::{
    ast::{AstNode, Expression, Command},
    err::IzeErr,
    grammar_expr::{expr, expr_type},
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
            Fun(param_pair_expr, 3),
            Zero(&[Key(TokenKind::Comma, 4), Fun(param_pair_expr, 5)]),
            Tk(TokenKind::Arrow, 6),
            Fun(expr_type, 7),
            Sel(&[
                Con(&[
                    Key(TokenKind::OpenParenth, 8),
                    Fun(transfer_body_struct, 9),
                    Tk(TokenKind::ClosingParenth, 10),
                ]),
                Fun(expr, 8),
            ]),
        ],
        |mut node_vec| {
            let body = node_vec.pop().unwrap();
            let (body, end_pos) = if let AstNode::Vec(mut body_vec) = body {
                // It's a struct body
                let end_pos = body_vec.pop().unwrap().token().unwrap().pos; // token ')'
                let pair_vec = body_vec.pop().unwrap();
                (pair_vec, end_pos)
            } else {
                // It's an expression body
                let body = body.expr().unwrap();
                let end_pos = body.end_pos;
                (body.into(), end_pos)
            };
            let ret_type = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Arrow token
            let params_vec = node_vec.pop().unwrap().vec().unwrap();
            let first_param = node_vec.pop().unwrap().expr().unwrap();
            let mut params: Vec<AstNode> = vec![first_param.into()];
            for param in params_vec {
                let mut param = param.vec().unwrap();
                let pair = param.pop().unwrap().expr().unwrap();
                params.push(pair.into());
            }
            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos; // token "transfer"

            Command::new_transfer(ident, params, ret_type, body, start_pos, end_pos).into()
        },
        //TODO: handle errors
        |input, e| todo!("Transfer error = {:#?}", e),
    )
}

///////////////////////
// Support functions //
///////////////////////

// Parse the struct body of a transfer.
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
        //TODO: error handling
        |_, e| Err(e),
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
        //TODO: error handling
        |_, e| Err(e),
    )
}

/// Parse a Pair expression for transfer body.
fn transfer_body_pair_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Fun(token_ident, 1), Tk(TokenKind::Colon, 2), Fun(expr, 3)],
        simple_pair_success,
        //TODO: error handling
        |_, e| Err(e),
    )
}

// Collect Pair expression for grammar: IDENTIFIER ":" EXPRESSION
fn simple_pair_success(mut node_vec: Vec<AstNode>) -> AstNode {
    let right_expr = node_vec.pop().unwrap().expr().unwrap();
    node_vec.pop().unwrap().token().unwrap(); // colon token
    let left_ident = node_vec.pop().unwrap().token().unwrap();
    let left_expr = Box::new(Expression::new_primary(left_ident));
    Expression::new_pair(left_expr, right_expr).into()
}
