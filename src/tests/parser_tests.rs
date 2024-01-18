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
