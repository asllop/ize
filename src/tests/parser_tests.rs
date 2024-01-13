//! # PARSER TESTS

use alloc::vec::Vec;

use crate::{
    ast::{AstNode, Expression},
    grammar,
    lexer::{self, Token, TokenKind, TokenPos},
};

fn parse(code: &str) -> Vec<AstNode> {
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_ok());
    let tokens = tokens.unwrap();
    let mut input = tokens.as_slice();
    let mut res = vec![];
    while !input.is_empty() {
        let expr = grammar::expr(input);
        assert!(expr.is_ok());
        let (rest, matched, _) = expr.unwrap();
        input = rest;
        res.push(matched);
    }
    res
}

fn parse_single_expr(code: &str) -> AstNode {
    let mut expr = parse(code);
    assert!(expr.len() == 1);
    let expr = expr.pop();
    assert!(expr.is_some());
    expr.unwrap()
}

#[test]
fn check_primary() {
    let code = "100";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 3, 0),
            TokenKind::IntegerLiteral(100)
        ))
        .into()
    );

    let code = "100.01";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 6, 0),
            TokenKind::FloatLiteral(100.01)
        ))
        .into()
    );

    let code = "true";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 4, 0),
            TokenKind::BooleanLiteral(true)
        ))
        .into()
    );

    let code = "none";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 4, 0),
            TokenKind::NoneLiteral
        ))
        .into()
    );

    let code = "null";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 4, 0),
            TokenKind::NullLiteral
        ))
        .into()
    );

    let code = "\"Hello World\"";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 13, 0),
            TokenKind::StringLiteral("\"Hello World\"".into())
        ))
        .into()
    );

    let code = "_my_var";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 7, 0),
            TokenKind::Identifier("_my_var".into())
        ))
        .into()
    );

    //TODO: check primary error
}

#[test]
fn check_binary_term() {
    let code = "1 + 2";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            Token::new(TokenPos::new(0, 2, 3, 2), TokenKind::Plus),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 0, 1, 0),
                TokenKind::IntegerLiteral(1)
            ))
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 4, 5, 4),
                TokenKind::IntegerLiteral(2)
            ))
            .into()
        )
        .into()
    );

    let code = "1 + 2 - c";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            Token::new(TokenPos::new(0, 6, 7, 6), TokenKind::Minus),
            Expression::new_binary(
                Token::new(TokenPos::new(0, 2, 3, 2), TokenKind::Plus),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 0, 1, 0),
                    TokenKind::IntegerLiteral(1)
                ))
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 4, 5, 4),
                    TokenKind::IntegerLiteral(2)
                ))
                .into()
            )
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 8, 9, 8),
                TokenKind::Identifier("c".into())
            ))
            .into()
        )
        .into()
    );

    //TODO: check binary error
}

#[test]
fn check_group() {
    //TODO
}

#[test]
fn check_let() {
    //TODO
}

#[test]
fn check_chain() {
    //TODO
}

#[test]
fn check_if_else() {
    //TODO
}

#[test]
fn check_complex_chain() {
    //TODO
}

#[test]
fn check_complex_group() {
    //TODO
}
