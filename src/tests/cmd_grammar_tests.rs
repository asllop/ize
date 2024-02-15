//! # COMMAND GRAMMAR TESTS

use alloc::vec::Vec;

use crate::{
    ast::{BinaryOp, Command, Expression, Identifier, ImportSymbol, Literal, Primary},
    grammar_cmd, lexer,
    parser::ParseNode,
    pos::{Pos, RangePos},
};

fn parse(code: &str) -> Vec<ParseNode> {
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

fn parse_single_cmd(code: &str) -> ParseNode {
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
        Command::new_model_with_type(
            Identifier::new("X".into(), RangePos::inline_new(0, 6, 7, 6)),
            Expression::new_type(
                Identifier::new("Map".into(), Pos::new(0, 11, 11) - Pos::new(0, 8, 8)),
                vec![
                    Expression::new_type(
                        Identifier::new("Str".into(), Pos::new(0, 15, 15) - Pos::new(0, 12, 12)),
                        vec![],
                        Pos::new(0, 15, 15) - Pos::new(0, 12, 12),
                    ),
                    Expression::new_type(
                        Identifier::new("Int".into(), Pos::new(0, 19, 19) - Pos::new(0, 16, 16)),
                        vec![],
                        Pos::new(0, 19, 19) - Pos::new(0, 16, 16),
                    ),
                ],
                Pos::new(0, 20, 20) - Pos::new(0, 8, 8)
            ),
            Pos::new(0, 20, 20) - Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = r#"model X (one: Str, two as "num.two": Int)"#;
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_model_with_struct(
            Identifier::new("X".into(), RangePos::inline_new(0, 6, 7, 6),),
            vec![
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("one".into()),
                        RangePos::inline_new(0, 9, 12, 9)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("Str".into()),
                        RangePos::inline_new(0, 14, 17, 14)
                    )
                    .into(),
                ),
                Expression::new_pair_with_alias(
                    Expression::new_primary(
                        Primary::Identifier("two".into()),
                        RangePos::inline_new(0, 19, 22, 19)
                    )
                    .into(),
                    Identifier::new("\"num.two\"".into(), RangePos::inline_new(0, 26, 35, 26),),
                    Expression::new_primary(
                        Primary::Identifier("Int".into()),
                        RangePos::inline_new(0, 37, 40, 37)
                    )
                    .into(),
                )
            ],
            Pos::new(0, 41, 41) - Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = r#"model X (one: Str, two as "num.two": Int, rest: ...)"#;
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_model_with_struct(
            Identifier::new("X".into(), RangePos::inline_new(0, 6, 7, 6),),
            vec![
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("one".into()),
                        RangePos::inline_new(0, 9, 12, 9)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("Str".into()),
                        RangePos::inline_new(0, 14, 17, 14)
                    )
                    .into(),
                ),
                Expression::new_pair_with_alias(
                    Expression::new_primary(
                        Primary::Identifier("two".into()),
                        RangePos::inline_new(0, 19, 22, 19)
                    )
                    .into(),
                    Identifier::new("\"num.two\"".into(), RangePos::inline_new(0, 26, 35, 26),),
                    Expression::new_primary(
                        Primary::Identifier("Int".into()),
                        RangePos::inline_new(0, 37, 40, 37)
                    )
                    .into(),
                ),
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("rest".into()),
                        RangePos::inline_new(0, 42, 46, 42)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("...".into()),
                        RangePos::inline_new(0, 48, 51, 48)
                    )
                    .into(),
                ),
            ],
            Pos::new(0, 52, 52) - Pos::new(0, 0, 0)
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
            Identifier::new("X".into(), RangePos::inline_new(0, 6, 7, 6)),
            Literal::String("\"hello world\"".into()),
            Pos::new(0, 21, 21) - Pos::new(0, 0, 0)
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
        Command::new_transfer_with_expr(
            Identifier::new("Suma".into(), RangePos::inline_new(0, 9, 13, 9)),
            vec![
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("a".into()),
                        RangePos::inline_new(0, 14, 15, 14)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("Int".into()),
                        RangePos::inline_new(0, 16, 19, 16),
                    )
                    .into(),
                ),
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("b".into()),
                        RangePos::inline_new(0, 21, 22, 21),
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Identifier("Int".into()),
                        RangePos::inline_new(0, 23, 26, 23)
                    )
                    .into(),
                )
            ],
            Expression::new_type(
                Identifier::new("Int".into(), RangePos::inline_new(0, 30, 33, 30)),
                vec![],
                RangePos::inline_new(0, 30, 33, 30)
            ),
            Expression::new_binary(
                BinaryOp::Plus,
                Expression::new_primary(
                    Primary::Identifier("a".into()),
                    RangePos::inline_new(0, 34, 35, 34)
                )
                .into(),
                Expression::new_primary(
                    Primary::Identifier("b".into()),
                    RangePos::inline_new(0, 38, 39, 38)
                )
                .into(),
            ),
            Pos::new(0, 39, 39) - Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "transfer X -> O (a: 0, b: 1)";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_transfer_with_struct(
            Identifier::new("X".into(), RangePos::inline_new(0, 9, 10, 9)),
            vec![],
            Expression::new_type(
                Identifier::new("O".into(), RangePos::inline_new(0, 14, 15, 14)),
                vec![],
                RangePos::inline_new(0, 14, 15, 14)
            ),
            vec![
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("a".into()),
                        RangePos::inline_new(0, 17, 18, 17)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Literal(Literal::Integer(0)),
                        RangePos::inline_new(0, 20, 21, 20),
                    )
                    .into(),
                ),
                Expression::new_pair(
                    Expression::new_primary(
                        Primary::Identifier("b".into()),
                        RangePos::inline_new(0, 23, 24, 23)
                    )
                    .into(),
                    Expression::new_primary(
                        Primary::Literal(Literal::Integer(1)),
                        RangePos::inline_new(0, 26, 27, 26),
                    )
                    .into(),
                )
            ],
            Pos::new(0, 28, 28) - Pos::new(0, 0, 0)
        )
        .into()
    )
}

#[test]
fn check_import() {
    let code = "import * from com.example";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_import(
            vec![ImportSymbol::new(
                Identifier::new("*".into(), RangePos::inline_new(0, 7, 8, 7)),
                None
            )],
            Expression::new_dot(vec![
                Expression::new_primary(
                    Primary::Identifier("com".into()),
                    RangePos::inline_new(0, 14, 17, 14),
                ),
                Expression::new_primary(
                    Primary::Identifier("example".into()),
                    RangePos::inline_new(0, 18, 25, 18),
                )
            ]),
            Pos::new(0, 25, 25) - Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "import A as X, B as Y from com.example";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_import(
            vec![
                ImportSymbol::new(
                    Identifier::new("A".into(), RangePos::inline_new(0, 7, 8, 7)),
                    Some(Identifier::new(
                        "X".into(),
                        RangePos::inline_new(0, 12, 13, 12)
                    ))
                ),
                ImportSymbol::new(
                    Identifier::new("B".into(), RangePos::inline_new(0, 15, 16, 15)),
                    Some(Identifier::new(
                        "Y".into(),
                        RangePos::inline_new(0, 20, 21, 20)
                    ))
                ),
            ],
            Expression::new_dot(vec![
                Expression::new_primary(
                    Primary::Identifier("com".into()),
                    RangePos::inline_new(0, 27, 30, 27),
                ),
                Expression::new_primary(
                    Primary::Identifier("example".into()),
                    RangePos::inline_new(0, 31, 38, 31),
                )
            ]),
            Pos::new(0, 38, 38) - Pos::new(0, 0, 0)
        )
        .into()
    );
}

#[test]
fn check_pipe() {
    let code = "pipe X (A, B())";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_pipe(
            Identifier::new("X".into(), RangePos::inline_new(0, 5, 6, 5),),
            Expression::new_pipe_body(
                vec![
                    Expression::new_primary(
                        Primary::Identifier("A".into()),
                        RangePos::inline_new(0, 8, 9, 8)
                    ),
                    Expression::new_call(
                        Identifier::new("B".into(), Pos::new(0, 12, 12) - Pos::new(0, 11, 11)),
                        vec![],
                        Pos::new(0, 14, 14) - Pos::new(0, 11, 11),
                    )
                ],
                Pos::new(0, 15, 15) - Pos::new(0, 7, 7)
            ),
            Pos::new(0, 0, 0)
        )
        .into()
    )
}

#[test]
fn check_run() {
    let code = "run X";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_run_with_ident(
            Identifier::new("X".into(), RangePos::inline_new(0, 4, 5, 4),),
            Pos::new(0, 0, 0)
        )
        .into()
    );

    let code = "run (A, B())";
    let cmd = parse_single_cmd(code);

    assert_eq!(
        cmd,
        Command::new_run_with_body(
            Expression::new_pipe_body(
                vec![
                    Expression::new_primary(
                        Primary::Identifier("A".into()),
                        RangePos::inline_new(0, 5, 6, 5)
                    ),
                    Expression::new_call(
                        Identifier::new("B".into(), Pos::new(0, 9, 9) - Pos::new(0, 8, 8)),
                        vec![],
                        Pos::new(0, 11, 11) - Pos::new(0, 8, 8),
                    )
                ],
                Pos::new(0, 12, 12) - Pos::new(0, 4, 4),
            ),
            Pos::new(0, 0, 0)
        )
        .into()
    )
}
