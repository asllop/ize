//! # Grammar
//!
//! Contains the grammar rules to parse the IZE language.

use crate::{
    ast::Expression,
    err::IzeErr,
    lexer::{Token, TokenKind},
    parser::{*, Parser::*},
};

/// Parse an expression.
pub fn expr(input: &[Token]) -> IzeResult {
    expr_chain(input)
}

/// Parse a Chain expression.
fn expr_chain(input: &[Token]) -> IzeResult {
    grammar(
        &Con(&[
            Fn(expr_let, 0),
            Zero(&Con(&[
                Key(TokenKind::Semicolon, 1),
                Fn(expr_let, 2),
            ])),
        ]),
        input,
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
                Err(IzeErr::new("Chain expected expression after semicolon".into(), e.err.pos).into())
            } else {
                // Propagate the error we received.
                Err(e)
            }
        }
    )
}

// fn __expr_chain(input: &[Token]) -> IzeResult {
//     def_grammar(
//         &[
//             Fn(expr_let, 0),
//             Zero(&Con(&[
//                 Tk(TokenKind::Semicolon, 1),
//                 Fn(expr_let, 2),
//             ])),
//         ],
//         input,
//         //TODO: handle partial errors
//         |rest, node_vec| {
//             let mut node_vec = node_vec.vec().unwrap();
//             let chain_vec = node_vec.pop().unwrap().vec().unwrap();
//             let let_expr = node_vec.pop().unwrap();

//             if !chain_vec.is_empty() {
//                 let mut expr_vec = vec![let_expr];
//                 for chain_pair in chain_vec {
//                     let mut chain_pair = chain_pair.vec().unwrap();
//                     let expr = chain_pair.pop().unwrap();
//                     expr_vec.push(expr);
//                 }
//                 Ok((rest, Expression::new_chain(expr_vec).into()))
//             } else {
//                 // Precedence
//                 Ok((rest, let_expr))
//             }
//         },
//     )
// }

// fn _expr_chain(mut input: &[Token]) -> IzeResult {
//     let mut expressions = vec![];
//     //TODO: aquí podem emprar el composer "zero_more"
//     let rest: &[Token] = loop {
//         let (rest, expr) = expr_let(input)?;
//         expressions.push(expr);
//         if let Ok((rest, _)) = token(&TokenKind::Semicolon, rest) {
//             input = rest;
//         } else {
//             break rest;
//         }
//     };
//     // If we are here, `expressions` Vec contains at least one element.
//     if expressions.len() == 1 {
//         Ok((rest, expressions.pop().unwrap()))
//     } else {
//         Ok((rest, Expression::new_chain(expressions).into()))
//     }
// }

/// Parse a Let expression.
fn expr_let(input: &[Token]) -> IzeResult {
    //TODO: aquí podem emprar el composer "concat"
    if let Ok((rest, let_token, _)) = token(&TokenKind::Let, input) {
        let (rest, ident_token, _) = token_ident(rest)?;
        let (rest, expr, _) = expr_let(rest)?;
        let let_expr = Expression::new_let(
            ident_token.token().unwrap(),
            expr.expr().unwrap(),
            let_token.token().unwrap().pos,
        );
        Ok((rest, let_expr.into(), false))
    } else {
        expr_ifelse(input)
    }
}

/// Parse an If-Else expression.
fn expr_ifelse(input: &[Token]) -> IzeResult {
    let grammar = concat(
        &[
            Tk(TokenKind::If, 0),
            Tk(TokenKind::OpenParenth, 1),
            Fn(expr, 2),
            Tk(TokenKind::ClosingParenth, 3),
            Fn(expr, 4),
            Tk(TokenKind::Else, 5),
            Fn(expr, 6),
        ],
        input,
    );
    match grammar {
        Ok((rest, node_vec, _)) => {
            // Collector
            let mut node_vec = node_vec.vec().unwrap();

            let else_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token "else"
            let if_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token ")"
            let cond_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token "("
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos; // Token "if"

            let ifelse_expr = Expression::new_ifelse(cond_expr, if_expr, else_expr, start_pos);
            Ok((rest, ifelse_expr.into(), false))
        }
        Err(e) => {
            // if e.after_key {
            //     //TODO: how to generate more detailed errors: "expected closing parenthesis", "expected 'else' token", etc.
            //     //      we need more information about where it failed parsing.

            //     // If-Else expression error
            //     Err(IzeErr::new("If-Else expression failed parsing".into(), e.err.pos).into())
            // } else {
            //     // Precedence
            //     expr_term(input)
            // }
            expr_term(input)
        }
    }
}

/// Parse a Term Binary expression.
fn expr_term(mut input: &[Token]) -> IzeResult {
    let (rest, mut expr, _) = expr_group(input)?;
    input = rest;
    //TODO: aquí podem emprar el composer "zero_more"
    loop {
        if let Ok((rest, op, _)) = select(
            &[Parser::Tk(TokenKind::Plus, 0), Parser::Tk(TokenKind::Minus, 1)],
            input,
        ) {
            let (rest, right, _) = expr_group(rest)?;
            expr = Expression::new_binary(
                op.token().unwrap(),
                expr.expr().unwrap(),
                right.expr().unwrap(),
            )
            .into();
            input = rest;
        } else {
            break;
        }
    }
    Ok((input, expr, false))
}

/// Parse a Group expression.
fn expr_group(input: &[Token]) -> IzeResult {
    grammar(
        &Con(&[
            Tk(TokenKind::OpenParenth, 0),
            Fn(expr, 1),
            Tk(TokenKind::ClosingParenth, 2),
        ]),
        input,
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
            } else {
                Err(IzeErr::new(format!("Group expression failed parsing at {}", e.id), e.err.pos).into())
            }
        },
    )
}

/// Parse a Primary expression.
fn expr_primary(input: &[Token]) -> IzeResult {
    let grammar = select(
        &[
            //TODO: parse type
            Fn(token_ident, 0),
            Fn(token_int, 1),
            Fn(token_flt, 2),
            Fn(token_str, 3),
            Fn(token_bool, 4),
            Tk(TokenKind::NoneLiteral, 5),
            Tk(TokenKind::NullLiteral, 6),
        ],
        input,
    );
    if let Ok((rest, node, _)) = grammar {
        // Collector
        let token = node.token().unwrap();
        let primary_expr = Expression::new_primary(token);
        Ok((rest, primary_expr.into(), false))
    } else {
        // Precedence
        let pos = if input.len() > 0 {
            input[0].pos
        } else {
            Default::default()
        };
        Err(IzeErr::new("Error parsing primary expr".into(), pos).into())
    }
}

/* Grammar changes:

- Two different syntaxes for transfers, one for key-value lists and one for expressions:
    transfer NAME IN_TYPE -> OUT_TYPE ( key: val, key: val, ... )
    transfer NAME IN_TYPE -> OUT_TYPE expression

- Allow multiple arguments for transfers:
    transfer NAME IN_TYPE_A: name_a, IN_TYPE_B: name_b, IN_TYPE_C: name_c -> OUT_TYPE ...
  as a syntax sugar for:
    transfer NAME Tuple[IN_TYPE_A, IN_TYPE_B, IN_TYPE_C] -> OUT_TYPE ...

- Select and Unwrap, use a more concise syntax. Now we are always forced to use the "as _":
    select EXPR do ( ... )
    select EXPR as IDENT do ( ... )
*/
