//! # Grammar
//!
//! Contains the grammar rules to parse the IZE language.

use crate::{
    ast::Expression,
    err::IzeErr,
    lexer::{Token, TokenKind},
    parser::*,
};

/// Parse an expression.
pub fn expr(input: &[Token]) -> IzeResult {
    expr_chain(input)
}

/// Parse a Chain expression.
fn expr_chain(mut input: &[Token]) -> IzeResult {
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
    //TODO: aquí podem emprar el composer "concat"
    if let Ok((rest, if_token, _)) = token(&TokenKind::If, input) {
        let (rest, _, _) = token(&TokenKind::OpenParenth, rest)?;
        let (rest, cond_expr, _) = expr(rest)?;
        let (rest, _, _) = token(&TokenKind::ClosingParenth, rest)?;
        let (rest, if_expr, _) = expr(rest)?;
        let (rest, _, _) = token(&TokenKind::Else, rest)?;
        let (rest, else_expr, _) = expr(rest)?;
        let ifelse_expr = Expression::new_ifelse(
            cond_expr.expr().unwrap(),
            if_expr.expr().unwrap(),
            else_expr.expr().unwrap(),
            if_token.token().unwrap().pos,
        );
        Ok((rest, ifelse_expr.into(), false))
    } else {
        expr_term(input)
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
    let grammar = concat(
        &[
            Parser::Key(&Parser::Tk(TokenKind::OpenParenth)),
            Parser::Fn(expr),
            Parser::Tk(TokenKind::ClosingParenth),
        ],
        input,
    );
    match grammar {
        Ok((rest, node_vec, _)) => {
            // Collector
            let mut node_vec = node_vec.vec().unwrap();
            let end = node_vec.pop().unwrap().token().unwrap().pos; // Token ")"
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let start = node_vec.pop().unwrap().token().unwrap().pos; // Token "("

            let group_expr = Expression::new_group(expr, start, end);
            Ok((rest, group_expr.into(), false))
        },
        Err(e) => {
            if e.after_key {
                // Group expression error
                Err(IzeErr::new("Group expression failed parsing".into(), e.err.pos).into())
            } else {
                // Precedence
                expr_primary(input)   
            }
        },
    }
}

/// Parse a Primary expression.
fn expr_primary(input: &[Token]) -> IzeResult {
    let grammar = select(
        &[
            //TODO: parse type
            Parser::Fn(token_ident),
            Parser::Fn(token_int),
            Parser::Fn(token_flt),
            Parser::Fn(token_str),
            Parser::Fn(token_bool),
            Parser::Tk(TokenKind::NoneLiteral),
            Parser::Tk(TokenKind::NullLiteral),
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
  As a syntax sugar for:
    transfer NAME Tuple[IN_TYPE_A, IN_TYPE_B, IN_TYPE_C] -> OUT_TYPE ...
*/