//! # COMMAND GRAMMAR TESTS

use alloc::{boxed::Box, vec::Vec};

use crate::{
    ast::{AstNode, Command, Expression},
    grammar_cmd,
    lexer::{self, Token, TokenKind, TokenPos},
};

fn parse(code: &str) -> Vec<AstNode> {
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_ok());
    let tokens = tokens.unwrap();
    let mut input = tokens.as_slice();
    let mut res = vec![];
    while !input.is_empty() {
        let cmd = grammar_cmd::cmd(input);
        assert!(cmd.is_ok());
        let (rest, matched, _) = cmd.unwrap();
        input = rest;
        res.push(matched);
    }
    res
}

fn parse_single_cmd(code: &str) -> AstNode {
    let mut cmd = parse(code);
    assert!(cmd.len() == 1);
    let cmd = cmd.pop();
    assert!(cmd.is_some());
    cmd.unwrap()
}

//TODO: test error handling in all kinds of commands

#[test]
fn check_model() {
    let code = "model X Map[Str,Int]";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_model(
            Token::new(TokenPos::new(0, 6, 7, 6), TokenKind::Identifier("X".into())),
            Expression::new_type(
                Token::new(TokenPos::new(0, 8, 11, 8), TokenKind::Map),
                vec![
                    Expression::new_type(
                        Token::new(
                            TokenPos::new(0, 12, 15, 12),
                            TokenKind::Identifier("Str".into())
                        ),
                        vec![],
                        TokenPos::new(0, 12, 15, 12),
                    )
                    .into(),
                    Expression::new_type(
                        Token::new(
                            TokenPos::new(0, 16, 19, 16),
                            TokenKind::Identifier("Int".into())
                        ),
                        vec![],
                        TokenPos::new(0, 16, 19, 16),
                    )
                    .into(),
                ],
                TokenPos::new(0, 19, 20, 19)
            )
            .into(),
            TokenPos::new(0, 0, 5, 0),
            TokenPos::new(0, 19, 20, 19)
        )
        .into()
    );

    let code = r#"model X (one: Str, two as "num.two": Int)"#;
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_model(
            Token::new(TokenPos::new(0, 6, 7, 6), TokenKind::Identifier("X".into())),
            vec![
                Expression::new_pair(
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 9, 12, 9),
                        TokenKind::Identifier("one".into())
                    ))),
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 14, 17, 14),
                        TokenKind::Identifier("Str".into())
                    ))),
                )
                .into(),
                Expression::new_pair_with_alias(
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 19, 22, 19),
                        TokenKind::Identifier("two".into())
                    ))),
                    Token::new(
                        TokenPos::new(0, 26, 35, 26),
                        TokenKind::StringLiteral("\"num.two\"".into())
                    ),
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 37, 40, 37),
                        TokenKind::Identifier("Int".into())
                    ))),
                )
                .into()
            ]
            .into(),
            TokenPos::new(0, 0, 5, 0),
            TokenPos::new(0, 40, 41, 40),
        )
        .into()
    );
}

#[test]
fn check_const() {
    let code = r#"const X "hello world""#;
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_const(
            Token::new(TokenPos::new(0, 6, 7, 6), TokenKind::Identifier("X".into())),
            Token::new(
                TokenPos::new(0, 8, 21, 8),
                TokenKind::StringLiteral("\"hello world\"".into())
            ),
            TokenPos::new(0, 0, 5, 0)
        )
        .into()
    );
}

#[test]
fn check_transfer() {
    let code = "transfer Suma a:Int, b:Int -> Int a + b";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_transfer(
            Token::new(
                TokenPos::new(0, 9, 13, 9),
                TokenKind::Identifier("Suma".into())
            ),
            vec![
                Expression::new_pair(
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 14, 15, 14),
                        TokenKind::Identifier("a".into())
                    ))),
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 16, 19, 16),
                        TokenKind::Identifier("Int".into())
                    ))),
                )
                .into(),
                Expression::new_pair(
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 21, 22, 21),
                        TokenKind::Identifier("b".into())
                    ))),
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 23, 26, 23),
                        TokenKind::Identifier("Int".into())
                    ))),
                )
                .into()
            ],
            Box::new(Expression::new_primary(Token::new(
                TokenPos::new(0, 30, 33, 30),
                TokenKind::Identifier("Int".into())
            ))),
            Expression::new_binary(
                Token::new(TokenPos::new(0, 36, 37, 36), TokenKind::Plus),
                Box::new(Expression::new_primary(Token::new(
                    TokenPos::new(0, 34, 35, 34),
                    TokenKind::Identifier("a".into())
                ))),
                Box::new(Expression::new_primary(Token::new(
                    TokenPos::new(0, 38, 39, 38),
                    TokenKind::Identifier("b".into())
                ))),
            )
            .into(),
            TokenPos::new(0, 0, 8, 0),
            TokenPos::new(0, 38, 39, 38)
        )
        .into()
    );

    let code = "transfer X -> O (a: 0, b: 1)";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_transfer(
            Token::new(
                TokenPos::new(0, 9, 10, 9),
                TokenKind::Identifier("X".into())
            ),
            vec![],
            Box::new(Expression::new_primary(Token::new(
                TokenPos::new(0, 14, 15, 14),
                TokenKind::Identifier("O".into())
            ))),
            vec![
                Expression::new_pair(
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 17, 18, 17),
                        TokenKind::Identifier("a".into())
                    ))),
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 20, 21, 20),
                        TokenKind::IntegerLiteral(0)
                    ))),
                )
                .into(),
                Expression::new_pair(
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 23, 24, 23),
                        TokenKind::Identifier("b".into())
                    ))),
                    Box::new(Expression::new_primary(Token::new(
                        TokenPos::new(0, 26, 27, 26),
                        TokenKind::IntegerLiteral(1)
                    ))),
                )
                .into()
            ]
            .into(),
            TokenPos::new(0, 0, 8, 0),
            TokenPos::new(0, 27, 28, 27)
        )
        .into()
    )
}
