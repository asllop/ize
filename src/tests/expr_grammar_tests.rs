//! # EXPRESSION GRAMMAR TESTS

use alloc::vec::Vec;

use crate::{
    ast::{AstNode, BinaryOp, Expression, Literal, Primary, UnaryOp},
    grammar_expr,
    lexer::{self, Token, TokenKind},
    pos::{Pos, RangePos},
};

fn parse(code: &str) -> Vec<AstNode> {
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_ok());
    let tokens = tokens.unwrap();
    let mut input = tokens.as_slice();
    let mut res = vec![];
    while !input.is_empty() {
        let expr = grammar_expr::expr(input);
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

//TODO: test error handling in all kinds of expressions

#[test]
fn check_primary() {
    let code = "100";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::Integer(100)),
            RangePos::inline_new(0, 0, 3, 0)
        )
        .into()
    );

    let code = "100.01";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::Float(100.01)),
            RangePos::inline_new(0, 0, 6, 0),
        )
        .into()
    );

    let code = "true";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::Boolean(true)),
            RangePos::inline_new(0, 0, 4, 0)
        )
        .into()
    );

    let code: &str = "false";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::Boolean(false)),
            RangePos::inline_new(0, 0, 5, 0)
        )
        .into()
    );

    let code = "none";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::None),
            RangePos::inline_new(0, 0, 4, 0)
        )
        .into()
    );

    let code = "null";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::Null),
            RangePos::inline_new(0, 0, 4, 0),
        )
        .into()
    );

    let code = "\"Hello World\"";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Literal(Literal::String("\"Hello World\"".into())),
            RangePos::inline_new(0, 0, 13, 0)
        )
        .into()
    );

    let code = "_my_var";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_primary(
            Primary::Identifier("_my_var".into()),
            RangePos::inline_new(0, 0, 7, 0)
        )
        .into()
    );
}

#[test]
fn check_binary_term() {
    let code = "1 + 2";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            BinaryOp::Plus,
            Expression::new_primary(
                Primary::Literal(Literal::Integer(1)),
                RangePos::inline_new(0, 0, 1, 0)
            )
            .into(),
            Expression::new_primary(
                Primary::Literal(Literal::Integer(2)),
                RangePos::inline_new(0, 4, 5, 4)
            )
            .into()
        )
        .into()
    );

    let code = "1 + 2 - c";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            BinaryOp::Minus,
            Expression::new_binary(
                BinaryOp::Plus,
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(1)),
                    RangePos::inline_new(0, 0, 1, 0)
                )
                .into(),
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(2)),
                    RangePos::inline_new(0, 4, 5, 4)
                )
                .into()
            )
            .into(),
            Expression::new_primary(
                Primary::Identifier("c".into()),
                RangePos::inline_new(0, 8, 9, 8),
            )
            .into()
        )
        .into()
    );
}

#[test]
fn check_binary_factor() {
    let code = "a+b*c";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            BinaryOp::Plus,
            Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 0, 1, 0)
            )
            .into(),
            Expression::new_binary(
                BinaryOp::Star,
                Expression::new_primary(
                    Primary::Identifier("b".into()),
                    RangePos::inline_new(0, 2, 3, 2)
                )
                .into(),
                Expression::new_primary(
                    Primary::Identifier("c".into()),
                    RangePos::inline_new(0, 4, 5, 4)
                )
                .into()
            )
            .into()
        )
        .into()
    );

    let code = "x*-1";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_binary(
            BinaryOp::Star,
            Expression::new_primary(
                Primary::Identifier("x".into()),
                RangePos::inline_new(0, 0, 1, 0),
            )
            .into(),
            Expression::new_unary(
                UnaryOp::Minus,
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(1)),
                    RangePos::inline_new(0, 3, 4, 3)
                )
                .into(),
                Pos::new(0, 2, 2)
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
            BinaryOp::TwoEquals,
            Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 0, 1, 0)
            )
            .into(),
            Expression::new_binary(
                BinaryOp::GreaterThan,
                Expression::new_primary(
                    Primary::Identifier("b".into()),
                    RangePos::inline_new(0, 3, 4, 3)
                )
                .into(),
                Expression::new_binary(
                    BinaryOp::And,
                    Expression::new_primary(
                        Primary::Identifier("c".into()),
                        RangePos::inline_new(0, 5, 6, 5)
                    )
                    .into(),
                    Expression::new_binary(
                        BinaryOp::Plus,
                        Expression::new_primary(
                            Primary::Identifier("d".into()),
                            RangePos::inline_new(0, 7, 8, 7)
                        )
                        .into(),
                        Expression::new_binary(
                            BinaryOp::Star,
                            Expression::new_primary(
                                Primary::Identifier("e".into()),
                                RangePos::inline_new(0, 9, 10, 9)
                            )
                            .into(),
                            Expression::new_primary(
                                Primary::Identifier("f".into()),
                                RangePos::inline_new(0, 11, 12, 11)
                            )
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
            BinaryOp::TwoEquals,
            Expression::new_binary(
                BinaryOp::GreaterThan,
                Expression::new_binary(
                    BinaryOp::And,
                    Expression::new_binary(
                        BinaryOp::Plus,
                        Expression::new_binary(
                            BinaryOp::Star,
                            Expression::new_primary(
                                Primary::Identifier("a".into()),
                                RangePos::inline_new(0, 0, 1, 0)
                            )
                            .into(),
                            Expression::new_primary(
                                Primary::Identifier("b".into()),
                                RangePos::inline_new(0, 2, 3, 2)
                            )
                            .into(),
                        )
                        .into(),
                        Expression::new_primary(
                            Primary::Identifier("c".into()),
                            RangePos::inline_new(0, 4, 5, 4)
                        )
                        .into(),
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("d".into()),
                        RangePos::inline_new(0, 6, 7, 6)
                    )
                    .into(),
                )
                .into(),
                Expression::new_primary(
                    Primary::Identifier("e".into()),
                    RangePos::inline_new(0, 8, 9, 8)
                )
                .into(),
            )
            .into(),
            Expression::new_primary(
                Primary::Identifier("f".into()),
                RangePos::inline_new(0, 11, 12, 11)
            )
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
            UnaryOp::Minus,
            Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 1, 2, 1)
            )
            .into(),
            Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "!a";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_unary(
            UnaryOp::Not,
            Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 1, 2, 1)
            )
            .into(),
            Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "-!-!a";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_unary(
            UnaryOp::Minus,
            Expression::new_unary(
                UnaryOp::Not,
                Expression::new_unary(
                    UnaryOp::Minus,
                    Expression::new_unary(
                        UnaryOp::Not,
                        Expression::new_primary(
                            Primary::Identifier("a".into()),
                            RangePos::inline_new(0, 4, 5, 4)
                        )
                        .into(),
                        Pos::new(0, 3, 3)
                    )
                    .into(),
                    Pos::new(0, 2, 2)
                )
                .into(),
                Pos::new(0, 1, 1)
            )
            .into(),
            Pos::new(0, 0, 0)
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
                BinaryOp::Plus,
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(1)),
                    RangePos::inline_new(0, 1, 2, 1)
                )
                .into(),
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(2)),
                    RangePos::inline_new(0, 5, 6, 5)
                )
                .into()
            )
            .into(),
            Pos::new(0, 7, 7) - Pos::new(0, 0, 0),
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
            "num".into(),
            Expression::new_primary(
                Primary::Literal(Literal::Integer(100)),
                RangePos::inline_new(0, 8, 11, 8)
            )
            .into(),
            Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "let var a+b";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            "var".into(),
            Expression::new_binary(
                BinaryOp::Plus,
                Expression::new_primary(
                    Primary::Identifier("a".into()),
                    RangePos::inline_new(0, 8, 9, 8)
                )
                .into(),
                Expression::new_primary(
                    Primary::Identifier("b".into()),
                    RangePos::inline_new(0, 10, 11, 10)
                )
                .into()
            )
            .into(),
            Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "let a let b 100";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            "a".into(),
            Expression::new_let(
                "b".into(),
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(100)),
                    RangePos::inline_new(0, 12, 15, 12)
                )
                .into(),
                Pos::new(0, 6, 6)
            )
            .into(),
            Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "let var (100;x)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_let(
            "var".into(),
            Expression::new_group(
                Expression::new_chain(vec![
                    Expression::new_primary(
                        Primary::Literal(Literal::Integer(100)),
                        RangePos::inline_new(0, 9, 12, 9)
                    ),
                    Expression::new_primary(
                        Primary::Identifier("x".into()),
                        RangePos::inline_new(0, 13, 14, 13)
                    )
                ])
                .into(),
                Pos::new(0, 15, 15) - Pos::new(0, 8, 8)
            )
            .into(),
            Pos::new(0, 0, 0)
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
            Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 0, 1, 0)
            ),
            Expression::new_primary(
                Primary::Identifier("b".into()),
                RangePos::inline_new(0, 2, 3, 2)
            ),
            Expression::new_primary(
                Primary::Identifier("c".into()),
                RangePos::inline_new(0, 4, 5, 4)
            )
        ])
        .into()
    );

    let code = "let var 100; var";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_chain(vec![
            Expression::new_let(
                "var".into(),
                Expression::new_primary(
                    Primary::Literal(Literal::Integer(100)),
                    RangePos::inline_new(0, 8, 11, 8)
                )
                .into(),
                Pos::new(0, 0, 0)
            ),
            Expression::new_primary(
                Primary::Identifier("var".into()),
                RangePos::inline_new(0, 13, 16, 13)
            )
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
            Expression::new_primary(
                Primary::Literal(Literal::Boolean(true)),
                RangePos::inline_new(0, 4, 8, 4)
            )
            .into(),
            Expression::new_primary(
                Primary::Literal(Literal::Integer(10)),
                RangePos::inline_new(0, 10, 12, 10)
            )
            .into(),
            Expression::new_primary(
                Primary::Literal(Literal::Integer(20)),
                RangePos::inline_new(0, 18, 20, 18)
            )
            .into(),
            Pos::new(0, 0, 0)
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
            Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 0, 1, 0)
            )
            .into(),
            Expression::new_group(
                Expression::new_binary(
                    BinaryOp::Plus,
                    Expression::new_primary(
                        Primary::Identifier("b".into()),
                        RangePos::inline_new(0, 3, 4, 3)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("c".into()),
                        RangePos::inline_new(0, 5, 6, 5)
                    )
                    .into(),
                )
                .into(),
                Pos::new(0, 7, 7) - Pos::new(0, 2, 2),
            )
            .into(),
            Expression::new_primary(
                Primary::Identifier("d".into()),
                RangePos::inline_new(0, 8, 9, 8)
            )
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
        Expression::new_call("foo".into(), vec![], Pos::new(0, 5, 5) - Pos::new(0, 0, 0)).into()
    );

    let code = "foo(a)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            "foo".into(),
            vec![Expression::new_primary(
                Primary::Identifier("a".into()),
                RangePos::inline_new(0, 4, 5, 4)
            )
            .into()],
            Pos::new(0, 6, 6) - Pos::new(0, 0, 0),
        )
        .into()
    );

    let code = "foo(a,b,c)";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            "foo".into(),
            vec![
                Expression::new_primary(
                    Primary::Identifier("a".into()),
                    RangePos::inline_new(0, 4, 5, 4)
                )
                .into(),
                Expression::new_primary(
                    Primary::Identifier("b".into()),
                    RangePos::inline_new(0, 6, 7, 6)
                )
                .into(),
                Expression::new_primary(
                    Primary::Identifier("c".into()),
                    RangePos::inline_new(0, 8, 9, 8)
                )
                .into(),
            ],
            Pos::new(0, 10, 10) - Pos::new(0, 0, 0),
        )
        .into()
    );

    let code = "foo(a,bar(b,c))";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_call(
            "foo".into(),
            vec![
                Expression::new_primary(
                    Primary::Identifier("a".into()),
                    RangePos::inline_new(0, 4, 5, 4)
                )
                .into(),
                Expression::new_call(
                    "bar".into(),
                    vec![
                        Expression::new_primary(
                            Primary::Identifier("b".into()),
                            RangePos::inline_new(0, 10, 11, 10)
                        )
                        .into(),
                        Expression::new_primary(
                            Primary::Identifier("c".into()),
                            RangePos::inline_new(0, 12, 13, 12)
                        )
                        .into()
                    ],
                    Pos::new(0, 14, 14) - Pos::new(0, 6, 6),
                )
                .into()
            ],
            Pos::new(0, 15, 15) - Pos::new(0, 0, 0),
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
            Expression::new_call("foo".into(), vec![], Pos::new(0, 5, 5) - Pos::new(0, 0, 0),)
                .into(),
            Expression::new_call(
                "bar".into(),
                vec![
                    Expression::new_primary(
                        Primary::Identifier("a".into()),
                        RangePos::inline_new(0, 10, 11, 10)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("b".into()),
                        RangePos::inline_new(0, 12, 13, 12)
                    )
                    .into(),
                ],
                Pos::new(0, 14, 14) - Pos::new(0, 6, 6),
            )
            .into(),
            Expression::new_primary(
                Primary::Identifier("z".into()),
                RangePos::inline_new(0, 15, 16, 15)
            )
            .into(),
        ])
        .into()
    );
}

#[test]
fn check_type() {
    let code = "List[String]";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_type(
            Token::new(RangePos::inline_new(0, 0, 4, 0), TokenKind::List),
            vec![Expression::new_type(
                Token::new(
                    RangePos::inline_new(0, 5, 11, 5),
                    TokenKind::Identifier("String".into())
                ),
                vec![],
                Pos::new(0, 11, 11)
            )
            .into()],
            Pos::new(0, 12, 12)
        )
        .into()
    );

    let code = "Map[String, Integer]";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_type(
            Token::new(RangePos::inline_new(0, 0, 3, 0), TokenKind::Map),
            vec![
                Expression::new_type(
                    Token::new(
                        RangePos::inline_new(0, 4, 10, 4),
                        TokenKind::Identifier("String".into())
                    ),
                    vec![],
                    Pos::new(0, 10, 10)
                )
                .into(),
                Expression::new_type(
                    Token::new(
                        RangePos::inline_new(0, 12, 19, 12),
                        TokenKind::Identifier("Integer".into())
                    ),
                    vec![],
                    Pos::new(0, 19, 19)
                )
                .into()
            ],
            Pos::new(0, 20, 20)
        )
        .into()
    );

    let code = "Tuple[Mux[Float, Integer], String]";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_type(
            Token::new(RangePos::inline_new(0, 0, 5, 0), TokenKind::Tuple),
            vec![
                Expression::new_type(
                    Token::new(RangePos::inline_new(0, 6, 9, 6), TokenKind::Mux),
                    vec![
                        Expression::new_type(
                            Token::new(
                                RangePos::inline_new(0, 10, 15, 10),
                                TokenKind::Identifier("Float".into())
                            ),
                            vec![],
                            Pos::new(0, 15, 15)
                        )
                        .into(),
                        Expression::new_type(
                            Token::new(
                                RangePos::inline_new(0, 17, 24, 17),
                                TokenKind::Identifier("Integer".into())
                            ),
                            vec![],
                            Pos::new(0, 24, 24)
                        )
                        .into()
                    ],
                    Pos::new(0, 25, 25)
                )
                .into(),
                Expression::new_type(
                    Token::new(
                        RangePos::inline_new(0, 27, 33, 27),
                        TokenKind::Identifier("String".into())
                    ),
                    vec![],
                    Pos::new(0, 33, 33)
                )
                .into()
            ],
            Pos::new(0, 34, 34)
        )
        .into()
    );
}

#[test]
fn check_select_unwrap() {
    let code = r#"select (v as i) (a -> x, b -> y, _ -> z)"#;
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_select_unwrap(
            Token::new(RangePos::inline_new(0, 0, 6, 0), TokenKind::Select),
            Expression::new_primary(
                Primary::Identifier("v".into()),
                RangePos::inline_new(0, 8, 9, 8)
            )
            .into(),
            Some(Token::new(
                RangePos::inline_new(0, 13, 14, 13),
                TokenKind::Identifier("i".into())
            )),
            vec![
                Expression::new_arm(
                    Expression::new_primary(
                        Primary::Identifier("a".into()),
                        RangePos::inline_new(0, 17, 18, 17)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("x".into()),
                        RangePos::inline_new(0, 22, 23, 22)
                    )
                    .into(),
                )
                .into(),
                Expression::new_arm(
                    Expression::new_primary(
                        Primary::Identifier("b".into()),
                        RangePos::inline_new(0, 25, 26, 25)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("y".into()),
                        RangePos::inline_new(0, 30, 31, 30)
                    )
                    .into(),
                )
                .into(),
                Expression::new_arm(
                    Expression::new_primary(
                        Primary::Identifier("_".into()),
                        RangePos::inline_new(0, 33, 34, 33)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("z".into()),
                        RangePos::inline_new(0, 38, 39, 38)
                    )
                    .into(),
                )
                .into(),
            ],
            Pos::new(0, 40, 40)
        )
        .into()
    )
}

#[test]
fn check_pair() {
    let code = r#"name: "Andreu""#;
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_pair(
            Expression::new_primary(
                Primary::Identifier("name".into()),
                RangePos::inline_new(0, 0, 4, 0)
            )
            .into(),
            Expression::new_primary(
                Primary::Literal(Literal::String("\"Andreu\"".into())),
                RangePos::inline_new(0, 6, 14, 6)
            )
            .into(),
        )
        .into()
    );

    let code = "MyKey(): myVal";
    let expr = parse_single_expr(code);
    assert_eq!(
        expr,
        Expression::new_pair(
            Expression::new_call(
                "MyKey".into(),
                vec![],
                Pos::new(0, 7, 7) - Pos::new(0, 0, 0),
            )
            .into(),
            Expression::new_primary(
                Primary::Identifier("myVal".into()),
                RangePos::inline_new(0, 9, 14, 9)
            )
            .into(),
        )
        .into()
    )
}

//TODO: check complex expression combination
