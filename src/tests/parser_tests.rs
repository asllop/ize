//! # PARSER COMPOSER TESTS

use crate::{
    ast::AstNode,
    lexer::{Token, TokenKind, TokenPos},
    parser::*,
};

#[test]
fn check_opt() {
    // Grammar to parse: ID ("->" ID)?
    let grammar = &[
        Parser::Fun(token_ident, 1),
        Parser::Opt(&[
            Parser::Key(TokenKind::Arrow, 2),
            Parser::Fun(token_ident, 3),
        ]),
    ];

    // Input code: x -> y
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("x".into())),
        Token::new(TokenPos::default(), TokenKind::Arrow),
        Token::new(TokenPos::default(), TokenKind::Identifier("y".into())),
    ];
    let res = concat(grammar, input);

    assert!(res.is_ok());
    let (rest, nodes, after_key) = res.unwrap();
    assert_eq!(rest, &[]);
    assert_eq!(after_key, true);
    let nodes = nodes.vec();
    assert!(nodes.is_some());
    let mut nodes = nodes.unwrap();
    assert_eq!(
        nodes.pop(),
        Some(AstNode::Vec(vec![
            Token::new(TokenPos::default(), TokenKind::Arrow).into(),
            Token::new(TokenPos::default(), TokenKind::Identifier("y".into())).into(),
        ]))
    );
    assert_eq!(
        nodes.pop(),
        Some(Token::new(TokenPos::default(), TokenKind::Identifier("x".into())).into())
    );
    assert_eq!(nodes.pop(), None);

    // Input code: x
    let input = &[Token::new(
        TokenPos::default(),
        TokenKind::Identifier("x".into()),
    )];
    let res = concat(grammar, input);

    assert!(res.is_ok());
    let (rest, nodes, after_key) = res.unwrap();
    assert_eq!(rest, &[]);
    assert_eq!(after_key, false);
    let nodes = nodes.vec();
    assert!(nodes.is_some());
    let mut nodes = nodes.unwrap();
    assert_eq!(
        nodes.pop(),
        Some(Token::new(TokenPos::default(), TokenKind::Identifier("x".into())).into())
    );
    assert_eq!(nodes.pop(), None);

    // Input code: x ->
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("x".into())),
        Token::new(TokenPos::default(), TokenKind::Arrow),
    ];
    let res = concat(grammar, input);

    assert!(res.is_err());
    let err = res.err().unwrap();
    assert_eq!(err.id, 3);
    assert_eq!(err.after_key, true);

    // Input code: x -> 10
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("x".into())),
        Token::new(TokenPos::default(), TokenKind::Arrow),
        Token::new(TokenPos::default(), TokenKind::IntegerLiteral(10)),
    ];
    let res = concat(grammar, input);

    assert!(res.is_err());
    let err = res.err().unwrap();
    assert_eq!(err.id, 3);
    assert_eq!(err.after_key, true);

    ////////////////////////////////////////////////////
    // Now use the same grammar without the Key token //
    ////////////////////////////////////////////////////

    let grammar = &[
        Parser::Fun(token_ident, 1),
        Parser::Opt(&[Parser::Tk(TokenKind::Arrow, 2), Parser::Fun(token_ident, 3)]),
    ];

    // Input code: x ->
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("x".into())),
        Token::new(TokenPos::default(), TokenKind::Arrow),
    ];
    let res = concat(grammar, input);

    assert!(res.is_ok());
    let (rest, nodes, after_key) = res.unwrap();
    assert_eq!(rest, &[Token::new(TokenPos::default(), TokenKind::Arrow)]);
    assert_eq!(after_key, false);
    let nodes = nodes.vec();
    assert!(nodes.is_some());
    let mut nodes = nodes.unwrap();
    assert_eq!(
        nodes.pop(),
        Some(Token::new(TokenPos::default(), TokenKind::Identifier("x".into())).into())
    );
    assert_eq!(nodes.pop(), None);

    // Input code: x -> 10
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("x".into())),
        Token::new(TokenPos::default(), TokenKind::Arrow),
        Token::new(TokenPos::default(), TokenKind::IntegerLiteral(10)),
    ];
    let res = concat(grammar, input);

    assert!(res.is_ok());
    let (rest, nodes, after_key) = res.unwrap();
    assert_eq!(
        rest,
        &[
            Token::new(TokenPos::default(), TokenKind::Arrow),
            Token::new(TokenPos::default(), TokenKind::IntegerLiteral(10))
        ]
    );
    assert_eq!(after_key, false);
    let nodes = nodes.vec();
    assert!(nodes.is_some());
    let mut nodes = nodes.unwrap();
    assert_eq!(
        nodes.pop(),
        Some(Token::new(TokenPos::default(), TokenKind::Identifier("x".into())).into())
    );
    assert_eq!(nodes.pop(), None);
}

// Check what if we have an option after a key token.
#[test]
fn check_opt_after_key() {
    // Grammar to parse: "def" ID ("as" STRING)? "=" LITERAL
    let binding = [
            Parser::Key(TokenKind::As, 3),
            Parser::Fun(token_str, 4),
        ];
    let grammar = &[
        Parser::Key(TokenKind::Identifier("def".into()), 1),
        Parser::Fun(token_ident, 2),
        Parser::Opt(&binding),
        Parser::Key(TokenKind::Identifier("=".into()), 5),
        Parser::Sel(&[
            Parser::Fun(token_str, 6),
            Parser::Fun(token_bool, 6),
            Parser::Fun(token_int, 6),
            Parser::Fun(token_flt, 6),
        ])
    ];

    // Input code: def my_var = 100
    //TODO: OK
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("def".into())),
        Token::new(TokenPos::default(), TokenKind::Identifier("my_var".into())),
        Token::new(TokenPos::default(), TokenKind::Identifier("=".into())),
        Token::new(TokenPos::default(), TokenKind::IntegerLiteral(100))
    ];
    assert!(concat(grammar, input).is_ok());

    // Input code: def my_var as "alias" = 100
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("def".into())),
        Token::new(TokenPos::default(), TokenKind::Identifier("my_var".into())),
        Token::new(TokenPos::default(), TokenKind::As),
        Token::new(TokenPos::default(), TokenKind::StringLiteral("alias".into())),
        Token::new(TokenPos::default(), TokenKind::Identifier("=".into())),
        Token::new(TokenPos::default(), TokenKind::IntegerLiteral(100))
    ];
    assert!(concat(grammar, input).is_ok());

    // Input code: def my_var as = 100
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("def".into())),
        Token::new(TokenPos::default(), TokenKind::Identifier("my_var".into())),
        Token::new(TokenPos::default(), TokenKind::As),
        Token::new(TokenPos::default(), TokenKind::Identifier("=".into())),
        Token::new(TokenPos::default(), TokenKind::IntegerLiteral(100))
    ];
    let res = concat(grammar, input);
    assert!(res.is_err());
    assert_eq!(res.err().unwrap().id, 4);

    // Input code: def my_var 100
    let input = &[
        Token::new(TokenPos::default(), TokenKind::Identifier("def".into())),
        Token::new(TokenPos::default(), TokenKind::Identifier("my_var".into())),
        Token::new(TokenPos::default(), TokenKind::IntegerLiteral(100))
    ];
    let res = concat(grammar, input);
    assert!(res.is_err());
    assert_eq!(res.err().unwrap().id, 5);

    // let res = concat(grammar, input);
    // if res.is_ok() {
    //     let (_, node, _) = res.unwrap();
    //     todo!("{:#?}", node);
    // } else {
    //     let err = res.err().unwrap();
    //     todo!("{:#?}", err);
    // }
}