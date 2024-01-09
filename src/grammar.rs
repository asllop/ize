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
    def_grammar(
        &[
            Fn(expr_let),
            Zero(&Con(&[
                Tk(TokenKind::Semicolon),
                Fn(expr_let),
            ])),
        ],
        input,
        //TODO: handle partial errors
        |rest, node_vec| {
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
                Ok((rest, Expression::new_chain(expr_vec).into(), false))
            } else {
                // Precedence
                Ok((rest, let_expr, false))
            }
        },
    )
}

fn _expr_chain(mut input: &[Token]) -> IzeResult {
    let mut expressions = vec![];
    //TODO: aquí podem emprar el composer "zero_more"
    let rest: &[Token] = loop {
        let (rest, expr, _) = expr_let(input)?;
        expressions.push(expr);
        if let Ok((rest, _, _)) = token(&TokenKind::Semicolon, rest) {
            input = rest;
        } else {
            break rest;
        }
    };
    // If we are here, `expressions` Vec contains at least one element.
    if expressions.len() == 1 {
        Ok((rest, expressions.pop().unwrap(), false))
    } else {
        Ok((rest, Expression::new_chain(expressions).into(), false))
    }
}

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
        IzeResult::Ok((rest, let_expr.into(), false))
    } else {
        expr_ifelse(input)
    }
}

/// Parse an If-Else expression.
fn expr_ifelse(input: &[Token]) -> IzeResult {
    let grammar = concat(
        &[
            Key(&Tk(TokenKind::If)),
            Tk(TokenKind::OpenParenth),
            Fn(expr),
            Tk(TokenKind::ClosingParenth),
            Fn(expr),
            Tk(TokenKind::Else),
            Fn(expr),
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
            if e.after_key {
                //TODO: how to generate more detailed errors: "expected closing parenthesis", "expected 'else' token", etc.
                //      we need more information about where it failed parsing.

                // If-Else expression error
                Err(IzeErr::new("If-Else expression failed parsing".into(), e.err.pos).into())
            } else {
                // Precedence
                expr_term(input)
            }
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
            &[Parser::Tk(TokenKind::Plus), Parser::Tk(TokenKind::Minus)],
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
            Key(&Tk(TokenKind::OpenParenth)),
            Fn(expr),
            Tk(TokenKind::ClosingParenth),
        ]),
        input,
        |node_vec| {
            let mut node_vec = node_vec.vec().unwrap();
            let end = node_vec.pop().unwrap().token().unwrap().pos; // Token ")"
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let start = node_vec.pop().unwrap().token().unwrap().pos; // Token "("
            Expression::new_group(expr, start, end).into()
        },
        |e| Err(IzeErr::new("Group expression failed parsing".into(), e.err.pos).into()),
        expr_primary,
    )
}

/// Parse a Primary expression.
fn expr_primary(input: &[Token]) -> IzeResult {
    let grammar = select(
        &[
            //TODO: parse type
            Fn(token_ident),
            Fn(token_int),
            Fn(token_flt),
            Fn(token_str),
            Fn(token_bool),
            Tk(TokenKind::NoneLiteral),
            Tk(TokenKind::NullLiteral),
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
