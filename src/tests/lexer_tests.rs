//! LEXER TESTS

use crate::{
    lexer::{self, Token, TokenKind},
    pos::RangePos,
};

#[test]
fn check_valid_tokens() {
    let code = "hello -100.0 \n \t 100 \"Hello world\" [)+-";
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_ok());
    let mut tokens = tokens.unwrap();
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(1, 24, 25, 38),
            TokenKind::Minus
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(1, 23, 24, 37),
            TokenKind::Plus
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(1, 22, 23, 36),
            TokenKind::ClosingParenth
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(1, 21, 22, 35),
            TokenKind::OpenClause
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(1, 7, 20, 21),
            TokenKind::StringLiteral("\"Hello world\"".into())
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(1, 3, 6, 17),
            TokenKind::IntegerLiteral(100)
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(0, 7, 12, 7),
            TokenKind::FloatLiteral(100.0)
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(0, 6, 7, 6),
            TokenKind::Minus
        ))
    );
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(0, 0, 5, 0),
            TokenKind::Identifier("hello".into())
        ))
    );
    assert_eq!(tokens.pop(), None);
}

#[test]
fn check_complex_string() {
    let code = r#" "Hello\t\n\x00\"world" "#;
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_ok());
    let mut tokens = tokens.unwrap();
    assert_eq!(
        tokens.pop(),
        Some(Token::new(
            RangePos::inline_new(0, 1, 23, 1),
            TokenKind::StringLiteral("\"Hello\\t\\n\\x00\\\"world\"".into())
        ))
    );
    assert_eq!(tokens.pop(), None);
}

#[test]
fn check_invalid_tokens() {
    // Unknown token ^
    let code = "hello ^100";
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_err());
    let err = tokens.err().unwrap();
    assert_eq!(err.pos, RangePos::inline_new(0, 6, 7, 6));

    // Unfinished string
    let code = "100 \"Hello world";
    let tokens = lexer::tokenize(code);
    assert!(tokens.is_err());
    let err = tokens.err().unwrap();
    assert_eq!(err.pos, RangePos::inline_new(0, 4, 16, 4));
}
