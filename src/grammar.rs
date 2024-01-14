//! # Grammar
//!
//! Contains the grammar rules to parse the IZE language.

use crate::{
    ast::{AstNode, Expression},
    err::IzeErr,
    lexer::{Token, TokenKind},
    parser::{Parser::*, *},
};

/// Parse an expression.
pub fn expr(input: &[Token]) -> IzeResult {
    expr_chain(input)
}

/// Parse a Chain expression.
fn expr_chain(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_let, 0),
            Zero(&[Key(TokenKind::Semicolon, 1), Fun(expr_let, 2)]),
        ],
        |node_vec| {
            let mut node_vec = node_vec.vec().unwrap();
            let chain_vec = node_vec.pop().unwrap().vec().unwrap();
            let let_expr = node_vec.pop().unwrap();

            if !chain_vec.is_empty() {
                let mut expr_vec = vec![let_expr];
                for chain_pair in chain_vec {
                    let mut chain_pair = chain_pair.vec().unwrap();
                    let expr = chain_pair.pop().unwrap();
                    expr_vec.push(expr);
                }
                Expression::new_chain(expr_vec).into()
            } else {
                // Precedence
                let_expr
            }
        },
        |_, e| {
            if e.id == 2 {
                Err(IzeErr::new(
                    "Chain expression, expected expression after semicolon".into(),
                    e.err.pos,
                )
                .into())
            } else {
                // Propagate the error we received.
                Err(e)
            }
        },
    )
}

/// Parse a Let expression.
fn expr_let(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Key(TokenKind::Let, 0),
            Fun(token_ident, 1),
            Fun(expr_let, 2),
        ],
        |node_vec| {
            let mut node_vec = node_vec.vec().unwrap();
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos;
            Expression::new_let(ident, expr, start_pos).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                0 => expr_ifelse(input),
                // Errors
                1 => Err(IzeErr::new(
                    "Let expression, expected identifier after 'let'".into(),
                    e.err.pos,
                )
                .into()),
                2 => Err(IzeErr::new(
                    "Let expression, expected expression after variable name".into(),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse an If-Else expression.
fn expr_ifelse(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Key(TokenKind::If, 0),
            Tk(TokenKind::OpenParenth, 1),
            Fun(expr, 2),
            Tk(TokenKind::ClosingParenth, 3),
            Fun(expr, 4),
            Tk(TokenKind::Else, 5),
            Fun(expr, 6),
        ],
        |node_vec| {
            let mut node_vec = node_vec.vec().unwrap();
            let else_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token "else"
            let if_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token ")"
            let cond_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token "("
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos; // Token "if"
            Expression::new_ifelse(cond_expr, if_expr, else_expr, start_pos).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                0 => expr_term(input),
                // Errors
                1 => Err(IzeErr::new(
                    "If-Else expression, expected '(' after 'if'".into(),
                    e.err.pos,
                )
                .into()),
                2 => Err(IzeErr::new(
                    "If-Else expression, expected condition expression".into(),
                    e.err.pos,
                )
                .into()),
                3 => Err(IzeErr::new(
                    "If-Else expression, expected ')' after condition".into(),
                    e.err.pos,
                )
                .into()),
                4 | 6 => Err(IzeErr::new(
                    "If-Else expression, expected branch expression".into(),
                    e.err.pos,
                )
                .into()),
                5 => Err(IzeErr::new(
                    "If-Else expression, expected 'else' after branch expression".into(),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Term Binary expression.
fn expr_term(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_factor, 0),
            Zero(&[
                Sel(&[Key(TokenKind::Plus, 1), Key(TokenKind::Minus, 2)]),
                Fun(expr_factor, 10),
            ]),
        ],
        binary_success,
        binary_error,
    )
}

/// Parse a Factor Binary expression.
fn expr_factor(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_group, 0),
            Zero(&[
                Sel(&[
                    Key(TokenKind::Star, 1),
                    Key(TokenKind::Slash, 2),
                    Key(TokenKind::Percent, 3),
                ]),
                Fun(expr_group, 10),
            ]),
        ],
        binary_success,
        binary_error,
    )
}

/// Parse a Group expression.
fn expr_group(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::OpenParenth, 0),
            Fun(expr, 1),
            Tk(TokenKind::ClosingParenth, 2),
        ],
        |node_vec| {
            let mut node_vec = node_vec.vec().unwrap();
            let end = node_vec.pop().unwrap().token().unwrap().pos; // Token ")"
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let start = node_vec.pop().unwrap().token().unwrap().pos; // Token "("
            Expression::new_group(expr, start, end).into()
        },
        |input, e| {
            if e.id == 0 {
                // Precedence
                expr_primary(input)
            } else if e.id == 2 {
                Err(IzeErr::new(
                    "Group expression expected a closing parenthesis".into(),
                    e.err.pos,
                )
                .into())
            } else {
                // Propagate error.
                Err(e)
            }
        },
    )
}

/// Parse a Primary expression.
fn expr_primary(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Sel(&[
            Fun(token_ident, 0),
            Fun(token_int, 1),
            Fun(token_flt, 2),
            Fun(token_str, 3),
            Fun(token_bool, 4),
            Tk(TokenKind::NoneLiteral, 5),
            Tk(TokenKind::NullLiteral, 6),
        ])],
        |node_vec| {
            let mut node_vec = node_vec.vec().unwrap();
            let token = node_vec.pop().unwrap().token().unwrap();
            Expression::new_primary(token).into()
        },
        |input, _| {
            let pos = if input.len() > 0 {
                input[0].pos
            } else {
                Default::default()
            };
            Err(IzeErr::new("Error parsing primary expr".into(), pos).into())
        },
    )
}

/// Collect binary expression
fn binary_success(node_vec: AstNode) -> AstNode {
    let mut node_vec = node_vec.vec().unwrap();
    let expr_vec = node_vec.pop().unwrap().vec().unwrap();
    let next_expr = node_vec.pop().unwrap();

    if !expr_vec.is_empty() {
        let mut final_expr = next_expr.expr().unwrap();
        for expr_pair in expr_vec {
            let mut expr_pair = expr_pair.vec().unwrap();
            let right_expr = expr_pair.pop().unwrap().expr().unwrap();
            let op = expr_pair.pop().unwrap().token().unwrap();
            final_expr = Expression::new_binary(op, final_expr, right_expr).into();
        }
        final_expr.into()
    } else {
        // Precedence
        next_expr
    }
}

/// Generate error for binary expression
fn binary_error(_: &[Token], e: ParseErr) -> IzeResult {
    if e.id == 10 {
        Err(IzeErr::new("Expected expression after operator".into(), e.err.pos).into())
    } else {
        Err(e)
    }
}

/* Grammar changes:

- Force argument name for transfers:
    transfer NAME in_name: IN_TYPE -> OUT_TYPE ...

- Two different syntaxes for transfers, one for key-value lists and one for expressions:
    transfer NAME in_name: IN_TYPE -> OUT_TYPE ( key: val, key: val, ... )
    transfer NAME in_name: IN_TYPE -> OUT_TYPE expression

- Allow multiple arguments for transfers:
    transfer NAME name_a:IN_TYPE_A, name_b:IN_TYPE_B, name_c:IN_TYPE_C -> OUT_TYPE ...
  as a syntax sugar for:
    transfer NAME Tuple[IN_TYPE_A, IN_TYPE_B, IN_TYPE_C] -> OUT_TYPE ...

- Select and Unwrap, use a more concise syntax. Now we are always forced to use the "as _":
    select (EXPR) ( ... )
    select (EXPR as IDENT) ( ... )
*/
