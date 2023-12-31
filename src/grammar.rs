use crate::{
    ast::Expression,
    err::IzeErr,
    lexer::{Token, TokenKind},
    parser::*,
};

pub fn expr(input: &[Token]) -> IzeResult {
    expr_chain(input)
}

fn expr_chain(mut input: &[Token]) -> IzeResult {
    let mut expressions = vec![];
    //TODO: aquí podem emprar el composer "zero_more"
    let rest: &[Token] = loop {
        let (rest, expr) = expr_let(input)?;
        expressions.push(expr);
        if let Ok((rest, _)) = token(&TokenKind::Semicolon, rest) {
            input = rest;
        } else {
            break rest;
        }
    };
    // If we are here, `expressions` Vec contains at least one element.
    if expressions.len() == 1 {
        Ok((rest, expressions.pop().unwrap()))
    } else {
        Ok((rest, Expression::new_chain(expressions).into()))
    }
}

fn expr_let(input: &[Token]) -> IzeResult {
    //TODO: aquí podem emprar el composer "concat"
    if let Ok((rest, let_token)) = token(&TokenKind::Let, input) {
        let (rest, ident_token) = token_ident(rest)?;
        let (rest, expr) = expr_let(rest)?;
        let let_expr = Expression::new_let(
            ident_token.token().unwrap(),
            expr.expr().unwrap(),
            let_token.token().unwrap().pos,
        );
        IzeResult::Ok((rest, let_expr.into()))
    } else {
        expr_ifelse(input)
    }
}

fn expr_ifelse(input: &[Token]) -> IzeResult {
    //TODO: aquí podem emprar el composer "concat"
    if let Ok((rest, if_token)) = token(&TokenKind::If, input) {
        let (rest, _) = token(&TokenKind::OpenParenth, rest)?;
        let (rest, cond_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::ClosingParenth, rest)?;
        let (rest, if_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::Else, rest)?;
        let (rest, else_expr) = expr(rest)?;
        let ifelse_expr = Expression::new_ifelse(
            cond_expr.expr().unwrap(),
            if_expr.expr().unwrap(),
            else_expr.expr().unwrap(),
            if_token.token().unwrap().pos,
        );
        Ok((rest, ifelse_expr.into()))
    } else {
        expr_term(input)
    }
}

fn expr_term(mut input: &[Token]) -> IzeResult {
    let (rest, mut expr) = expr_group(input)?;
    input = rest;
    //TODO: aquí podem emprar el composer "zero_more"
    loop {
        if let Ok((rest, op)) = select(
            &[Parser::Tk(TokenKind::Plus), Parser::Tk(TokenKind::Minus)],
            input,
        ) {
            let (rest, right) = expr_group(rest)?;
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
    Ok((input, expr))
}

fn expr_group(input: &[Token]) -> IzeResult {
    let grammar = concat(
        &[
            Parser::Tk(TokenKind::OpenParenth),
            Parser::Fn(expr),
            Parser::Tk(TokenKind::ClosingParenth),
        ],
        input,
    );
    if let Ok((rest, node_vec)) = grammar {
        // Collector
        let mut node_vec = node_vec.vec().unwrap();
        let end = node_vec.pop().unwrap().token().unwrap().pos; // Token ")"
        let expr = node_vec.pop().unwrap().expr().unwrap();
        let start = node_vec.pop().unwrap().token().unwrap().pos; // Token "("

        let group_expr = Expression::new_group(expr, start, end);
        Ok((rest, group_expr.into()))
    } else {
        // Precedence
        expr_primary(input)
    }
}

fn expr_primary(input: &[Token]) -> IzeResult {
    let grammar = select(
        &[
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
    if let Ok((rest, node)) = grammar {
        // Collector
        let token = node.token().unwrap();
        let primary_expr = Expression::new_primary(token);
        Ok((rest, primary_expr.into()))
    } else {
        // Precedence
        let pos = if input.len() > 0 {
            input[0].pos
        } else {
            Default::default()
        };
        Err(IzeErr::new("Error parsing primary expr".into(), pos))
    }
}
