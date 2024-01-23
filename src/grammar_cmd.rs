//! # Command Grammars
//!
//! Contains the grammar rules to parse IZE commands.

use alloc::vec::Vec;

use crate::{
    ast::{AstNode, Expression},
    err::IzeErr,
    lexer::{Token, TokenKind},
    parser::{Parser::*, *},
    grammar_expr::{expr, expr_type, expr_primary},
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
            Zero(&[
                Key(TokenKind::Comma, 4),
                Fun(param_pair_expr, 5),
            ]),
            Tk(TokenKind::Arrow, 6),
            Fun(expr_type, 7),
            Sel(&[
                Con(&[
                    Key(TokenKind::OpenParenth, 8),
                    Fun(transfer_body_pair_expr, 9),
                    Zero(&[
                        Key(TokenKind::Comma, 10),
                        Fun(transfer_body_pair_expr, 11),
                    ]),
                    Tk(TokenKind::ClosingParenth, 12),
                ]),
                Fun(expr, 8),
            ])
        ],
        |node_vec| todo!("Transfer success = {:#?}", node_vec),
        |input, e| todo!("Transfer error = {:#?}", e),
    )
}

///////////////////////
// Support functions //
///////////////////////

/// Parse a Pair expression for transfer parameters.
fn param_pair_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Fun(expr_primary, 1), Tk(TokenKind::Colon, 2), Fun(expr_type, 3)],
        pair_success,
        |_, e| Err(e),
    )
}

/// Parse a Pair expression for transfer body.
fn transfer_body_pair_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Fun(expr_primary, 1), Tk(TokenKind::Colon, 2), Fun(expr, 3)],
        pair_success,
        |_, e| Err(e),
    )
}

// Collect Pair expression
fn pair_success(mut node_vec: Vec<AstNode>) -> AstNode {
    let right_expr = node_vec.pop().unwrap().expr().unwrap();
    node_vec.pop().unwrap().token().unwrap(); // Arrow token
    let left_expr = node_vec.pop().unwrap().expr().unwrap();
    Expression::new_pair(left_expr, right_expr).into()
}