//! # EXPRESSION GRAMMAR TESTS

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

    let code: &str = "false";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(Token::new(
            TokenPos::new(0, 0, 5, 0),
            TokenKind::BooleanLiteral(false)
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

    let code = "x*-1";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            Token::new(TokenPos::new(0, 1, 2, 1), TokenKind::Star),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 0, 1, 0),
                TokenKind::Identifier("x".into())
            ))
            .into(),
            Expression::new_unary(
                Token::new(TokenPos::new(0, 2, 3, 2), TokenKind::Minus),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 3, 4, 3),
                    TokenKind::IntegerLiteral(1)
                ))
                .into()
            )
            .into()
        )
        .into()
    );

    //TODO: check binary error
}

#[test]
fn check_binary_factor() {
    let code = "a+b*c";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            Token::new(TokenPos::new(0, 1, 2, 1), TokenKind::Plus),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 0, 1, 0),
                TokenKind::Identifier("a".into())
            ))
            .into(),
            Expression::new_binary(
                Token::new(TokenPos::new(0, 3, 4, 3), TokenKind::Star),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 2, 3, 2),
                    TokenKind::Identifier("b".into())
                ))
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 4, 5, 4),
                    TokenKind::Identifier("c".into())
                ))
                .into()
            )
            .into()
        )
        .into()
    );
}

#[test]
fn check_binary_precedence() {
    let code = "a==b>c&d+e*f";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            Token::new(TokenPos::new(0, 1, 3, 1), TokenKind::TwoEquals),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 0, 1, 0),
                TokenKind::Identifier("a".into())
            ))
            .into(),
            Expression::new_binary(
                Token::new(TokenPos::new(0, 4, 5, 4), TokenKind::GreaterThan),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 3, 4, 3),
                    TokenKind::Identifier("b".into())
                ))
                .into(),
                Expression::new_binary(
                    Token::new(TokenPos::new(0, 6, 7, 6), TokenKind::And),
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 5, 6, 5),
                        TokenKind::Identifier("c".into())
                    ))
                    .into(),
                    Expression::new_binary(
                        Token::new(TokenPos::new(0, 8, 9, 8), TokenKind::Plus),
                        Expression::new_primary(Token::new(
                            TokenPos::new(0, 7, 8, 7),
                            TokenKind::Identifier("d".into())
                        ))
                        .into(),
                        Expression::new_binary(
                            Token::new(TokenPos::new(0, 10, 11, 10), TokenKind::Star),
                            Expression::new_primary(Token::new(
                                TokenPos::new(0, 9, 10, 9),
                                TokenKind::Identifier("e".into())
                            ))
                            .into(),
                            Expression::new_primary(Token::new(
                                TokenPos::new(0, 11, 12, 11),
                                TokenKind::Identifier("f".into())
                            ))
                            .into(),
                        )
                        .into()
                    )
                    .into()
                )
                .into()
            )
            .into()
        )
        .into()
    );

    let code = "a*b+c&d>e==f";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            Token::new(TokenPos::new(0, 9, 11, 9), TokenKind::TwoEquals),
            Expression::new_binary(
                Token::new(TokenPos::new(0, 7, 8, 7), TokenKind::GreaterThan),
                Expression::new_binary(
                    Token::new(TokenPos::new(0, 5, 6, 5), TokenKind::And),
                    Expression::new_binary(
                        Token::new(TokenPos::new(0, 3, 4, 3), TokenKind::Plus),
                        Expression::new_binary(
                            Token::new(TokenPos::new(0, 1, 2, 1), TokenKind::Star),
                            Expression::new_primary(Token::new(
                                TokenPos::new(0, 0, 1, 0),
                                TokenKind::Identifier("a".into())
                            ))
                            .into(),
                            Expression::new_primary(Token::new(
                                TokenPos::new(0, 2, 3, 2),
                                TokenKind::Identifier("b".into())
                            ))
                            .into(),
                        )
                        .into(),
                        Expression::new_primary(Token::new(
                            TokenPos::new(0, 4, 5, 4),
                            TokenKind::Identifier("c".into())
                        ))
                        .into(),
                    )
                    .into(),
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 6, 7, 6),
                        TokenKind::Identifier("d".into())
                    ))
                    .into(),
                )
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 8, 9, 8),
                    TokenKind::Identifier("e".into())
                ))
                .into(),
            )
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 11, 12, 11),
                TokenKind::Identifier("f".into())
            ))
            .into(),
        )
        .into()
    );
}

#[test]
fn check_unary() {
    let code = "-a";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_unary(
            Token::new(TokenPos::new(0, 0, 1, 0), TokenKind::Minus),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 1, 2, 1),
                TokenKind::Identifier("a".into())
            ))
            .into()
        )
        .into()
    );

    let code = "!a";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_unary(
            Token::new(TokenPos::new(0, 0, 1, 0), TokenKind::Not),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 1, 2, 1),
                TokenKind::Identifier("a".into())
            ))
            .into()
        )
        .into()
    );

    let code = "-!-!a";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_unary(
            Token::new(TokenPos::new(0, 0, 1, 0), TokenKind::Minus),
            Expression::new_unary(
                Token::new(TokenPos::new(0, 1, 2, 1), TokenKind::Not),
                Expression::new_unary(
                    Token::new(TokenPos::new(0, 2, 3, 2), TokenKind::Minus),
                    Expression::new_unary(
                        Token::new(TokenPos::new(0, 3, 4, 3), TokenKind::Not),
                        Expression::new_primary(Token::new(
                            TokenPos::new(0, 4, 5, 4),
                            TokenKind::Identifier("a".into())
                        ))
                        .into()
                    )
                    .into()
                )
                .into()
            )
            .into()
        )
        .into()
    );
}

#[test]
fn check_group() {
    let code = "(1 + 2)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_group(
            Expression::new_binary(
                Token::new(TokenPos::new(0, 3, 4, 3), TokenKind::Plus),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 1, 2, 1),
                    TokenKind::IntegerLiteral(1)
                ))
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 5, 6, 5),
                    TokenKind::IntegerLiteral(2)
                ))
                .into()
            )
            .into(),
            TokenPos::new(0, 0, 1, 0),
            TokenPos::new(0, 6, 7, 6)
        )
        .into()
    );
}

#[test]
fn check_let() {
    let code = "let num 100";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            Token::new(
                TokenPos::new(0, 4, 7, 4),
                TokenKind::Identifier("num".into())
            ),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 8, 11, 8),
                TokenKind::IntegerLiteral(100)
            ))
            .into(),
            TokenPos::new(0, 0, 3, 0)
        )
        .into()
    );

    let code = "let var a+b";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            Token::new(
                TokenPos::new(0, 4, 7, 4),
                TokenKind::Identifier("var".into())
            ),
            Expression::new_binary(
                Token::new(TokenPos::new(0, 9, 10, 9), TokenKind::Plus),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 8, 9, 8),
                    TokenKind::Identifier("a".into())
                ))
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 10, 11, 10),
                    TokenKind::Identifier("b".into())
                ))
                .into()
            )
            .into(),
            TokenPos::new(0, 0, 3, 0)
        )
        .into()
    );

    let code = "let a let b 100";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            Token::new(TokenPos::new(0, 4, 5, 4), TokenKind::Identifier("a".into())),
            Expression::new_let(
                Token::new(
                    TokenPos::new(0, 10, 11, 10),
                    TokenKind::Identifier("b".into())
                ),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 12, 15, 12),
                    TokenKind::IntegerLiteral(100)
                ))
                .into(),
                TokenPos::new(0, 6, 9, 6)
            )
            .into(),
            TokenPos::new(0, 0, 3, 0)
        )
        .into()
    );

    let code = "let var (100;x)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            Token::new(
                TokenPos::new(0, 4, 7, 4),
                TokenKind::Identifier("var".into())
            ),
            Expression::new_group(
                Expression::new_chain(vec![
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 9, 12, 9),
                        TokenKind::IntegerLiteral(100)
                    ))
                    .into(),
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 13, 14, 13),
                        TokenKind::Identifier("x".into())
                    ))
                    .into()
                ])
                .into(),
                TokenPos::new(0, 8, 9, 8),
                TokenPos::new(0, 14, 15, 14)
            )
            .into(),
            TokenPos::new(0, 0, 3, 0)
        )
        .into()
    );
}

#[test]
fn check_chain() {
    let code = "a;b;c";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_chain(vec![
            Expression::new_primary(Token::new(
                TokenPos::new(0, 0, 1, 0),
                TokenKind::Identifier("a".into())
            ))
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 2, 3, 2),
                TokenKind::Identifier("b".into())
            ))
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 4, 5, 4),
                TokenKind::Identifier("c".into())
            ))
            .into()
        ])
        .into()
    );

    let code = "let var 100; var";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_chain(vec![
            Expression::new_let(
                Token::new(
                    TokenPos::new(0, 4, 7, 4),
                    TokenKind::Identifier("var".into())
                ),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 8, 11, 8),
                    TokenKind::IntegerLiteral(100)
                ))
                .into(),
                TokenPos::new(0, 0, 3, 0)
            )
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 13, 16, 13),
                TokenKind::Identifier("var".into())
            ))
            .into()
        ])
        .into()
    );
}

#[test]
fn check_if_else() {
    let code = "if (true) 10 else 20";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_ifelse(
            Expression::new_primary(Token::new(
                TokenPos::new(0, 4, 8, 4),
                TokenKind::BooleanLiteral(true)
            ))
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 10, 12, 10),
                TokenKind::IntegerLiteral(10)
            ))
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 18, 20, 18),
                TokenKind::IntegerLiteral(20)
            ))
            .into(),
            TokenPos::new(0, 0, 2, 0)
        )
        .into()
    );
}

#[test]
fn check_dot() {
    let code = "a.(b+c).d";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_dot(vec![
            Expression::new_primary(Token::new(
                TokenPos::new(0, 0, 1, 0),
                TokenKind::Identifier("a".into())
            ))
            .into(),
            Expression::new_group(
                Expression::new_binary(
                    Token::new(TokenPos::new(0, 4, 5, 4), TokenKind::Plus),
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 3, 4, 3),
                        TokenKind::Identifier("b".into())
                    ))
                    .into(),
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 5, 6, 5),
                        TokenKind::Identifier("c".into())
                    ))
                    .into(),
                )
                .into(),
                TokenPos::new(0, 2, 3, 2),
                TokenPos::new(0, 6, 7, 6)
            )
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 8, 9, 8),
                TokenKind::Identifier("d".into())
            ))
            .into(),
        ])
        .into()
    );
}

#[test]
fn check_call() {
    let code = "foo()";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            Token::new(
                TokenPos::new(0, 0, 3, 0),
                TokenKind::Identifier("foo".into())
            ),
            vec![],
            TokenPos::new(0, 4, 5, 4)
        )
        .into()
    );

    let code = "foo(a)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            Token::new(
                TokenPos::new(0, 0, 3, 0),
                TokenKind::Identifier("foo".into())
            ),
            vec![Expression::new_primary(Token::new(
                TokenPos::new(0, 4, 5, 4),
                TokenKind::Identifier("a".into())
            ))
            .into()],
            TokenPos::new(0, 5, 6, 5)
        )
        .into()
    );

    let code = "foo(a,b,c)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            Token::new(
                TokenPos::new(0, 0, 3, 0),
                TokenKind::Identifier("foo".into())
            ),
            vec![
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 4, 5, 4),
                    TokenKind::Identifier("a".into())
                ))
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 6, 7, 6),
                    TokenKind::Identifier("b".into())
                ))
                .into(),
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 8, 9, 8),
                    TokenKind::Identifier("c".into())
                ))
                .into(),
            ],
            TokenPos::new(0, 9, 10, 9)
        )
        .into()
    );

    let code = "foo(a,bar(b,c))";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            Token::new(
                TokenPos::new(0, 0, 3, 0),
                TokenKind::Identifier("foo".into())
            ),
            vec![
                Expression::new_primary(Token::new(
                    TokenPos::new(0, 4, 5, 4),
                    TokenKind::Identifier("a".into())
                ))
                .into(),
                Expression::new_call(
                    Token::new(
                        TokenPos::new(0, 6, 9, 6),
                        TokenKind::Identifier("bar".into())
                    ),
                    vec![
                        Expression::new_primary(Token::new(
                            TokenPos::new(0, 10, 11, 10),
                            TokenKind::Identifier("b".into())
                        ))
                        .into(),
                        Expression::new_primary(Token::new(
                            TokenPos::new(0, 12, 13, 12),
                            TokenKind::Identifier("c".into())
                        ))
                        .into()
                    ],
                    TokenPos::new(0, 13, 14, 13)
                )
                .into()
            ],
            TokenPos::new(0, 14, 15, 14)
        )
        .into()
    );
}

#[test]
fn check_dot_with_calls() {
    let code = "foo().bar(a,b).z";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_dot(vec![
            Expression::new_call(
                Token::new(
                    TokenPos::new(0, 0, 3, 0),
                    TokenKind::Identifier("foo".into())
                ),
                vec![],
                TokenPos::new(0, 4, 5, 4)
            )
            .into(),
            Expression::new_call(
                Token::new(
                    TokenPos::new(0, 6, 9, 6),
                    TokenKind::Identifier("bar".into())
                ),
                vec![
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 10, 11, 10),
                        TokenKind::Identifier("a".into())
                    ))
                    .into(),
                    Expression::new_primary(Token::new(
                        TokenPos::new(0, 12, 13, 12),
                        TokenKind::Identifier("b".into())
                    ))
                    .into(),
                ],
                TokenPos::new(0, 13, 14, 13)
            )
            .into(),
            Expression::new_primary(Token::new(
                TokenPos::new(0, 15, 16, 15),
                TokenKind::Identifier("z".into())
            ))
            .into(),
        ])
        .into()
    );
}

//TODO: check complex expression combination
