//! # Parser
//!
//! Parser combinator infrastructure. Contains the primitives used to define the parser for the IZE language. Inspired by [Nom](https://github.com/rust-bakery/nom).

use alloc::vec::Vec;

use crate::{
    ast::AstNode,
    err::IzeErr,
    lexer::{Token, TokenKind, TokenPos},
};

#[derive(Debug, Default)]
/// Parser error.
pub struct ParseErr {
    /// Actual error.
    pub err: IzeErr,
    /// Error happened at parser with ID.
    pub id: u16,
    /// Failed after a key.
    pub after_key: bool,
}

impl ParseErr {
    /// new parse error.
    pub fn new(err: IzeErr, id: u16, after_key: bool) -> Self {
        Self { err, id, after_key }
    }

    /// Into parts
    pub fn into_parts(self) -> (IzeErr, u16) {
        (self.err, self.id)
    }

    /// Convert error into an eror with after_key = true.
    pub fn into_key(mut self) -> Self {
        self.after_key = true;
        self
    }
}

impl From<IzeErr> for ParseErr {
    fn from(value: IzeErr) -> Self {
        ParseErr::new(value, 0, false)
    }
}

impl From<ParseErr> for IzeErr {
    fn from(value: ParseErr) -> Self {
        value.err
    }
}

/// A key token was already parsed.
type AfterKey = bool;

/// Result type alias for parsers.
pub type IzeResult<'a> = Result<(&'a [Token], AstNode, AfterKey), ParseErr>;

/// Result type alias for parsers with optional result.
pub type IzeOptResult<'a> = Result<(Option<(&'a [Token], AstNode)>, AfterKey), ParseErr>;

/// Convert [IzeResult](crate::parser::IzeResult) into [IzeOptResult](crate::parser::IzeOptResult).
pub fn into_opt_res(value: IzeResult) -> IzeOptResult {
    let (rest, node, after_key) = value?;
    Ok((Some((rest, node)), after_key))
}

/// Define a grammar.
pub fn def_grammar<'a>(
    input: &'a [Token],
    parsers: &'a [Parser<'a>],
    success: fn(Vec<AstNode>) -> AstNode,
    error: fn(&'a [Token], ParseErr) -> IzeResult<'a>,
) -> IzeResult<'a> {
    match concat(parsers, input) {
        Ok((rest, node, after_key)) => {
            let result_node = success(node.vec().unwrap());
            Ok((rest, result_node, after_key))
        }
        Err(e) => error(input, e),
    }
}

/// Parser element.
#[derive(Clone)]
pub enum Parser<'a> {
    /// Key token.
    Key(TokenKind, u16),
    /// Generic parser function.
    Fun(fn(&[Token]) -> IzeResult, u16),
    /// Token.
    Tk(TokenKind, u16),
    /// Select parser composer. Executes one from a list of parsers, the first that matches.
    Sel(&'a [Parser<'a>]),
    /// Concatenate parser composers. Executes a list of parsers.
    Con(&'a [Parser<'a>]),
    /// Optional parser composer. Optionally executes a concat of parsers.
    Opt(&'a [Parser<'a>]),
    /// Zero-plus parser composer. Executes a concat of parsers zero or more times.
    Zero(&'a [Parser<'a>]),
    /// One-plus parser composer. Executes a concat of parsers one or more times.
    One(&'a [Parser<'a>]),
}

impl<'a> Parser<'a> {
    /// Run a parser element.
    pub fn run(&self, input: &'a [Token]) -> IzeOptResult {
        let (res, id) = match self {
            Self::Key(token_kind, id) => {
                let r = match token(token_kind, input) {
                    Ok((rest, node, _)) => Ok((Some((rest, node)), true)),
                    Err(e) => Err(e),
                };
                (r, Some(*id))
            }
            Self::Fun(parser_fn, id) => (into_opt_res(parser_fn(input)), Some(*id)),
            Self::Tk(token_kind, id) => (into_opt_res(token(token_kind, input)), Some(*id)),
            Self::Sel(parsers) => (into_opt_res(select(parsers, input)), None),
            Self::Con(parsers) => (into_opt_res(concat(parsers, input)), None),
            Self::Opt(parsers) => (optional(parsers, input), None),
            Self::Zero(parsers) => (into_opt_res(zero_plus(parsers, input)), None),
            Self::One(parsers) => (into_opt_res(one_plus(parsers, input)), None),
        };
        if let Err(mut e) = res {
            if let Some(id) = id {
                e.id = id;
            }
            Err(e)
        } else {
            res
        }
    }
}

/// Select a parser from a list, the first that succeeds.
pub fn select<'a>(parsers: &'a [Parser], input: &'a [Token]) -> IzeResult<'a> {
    let mut last_failed_id = 0;
    for parser in parsers {
        match parser.run(input) {
            Ok((Some((rest, node)), after_key)) => return Ok((rest, node, after_key)),
            Ok((None, _)) => {}
            Err(e) => last_failed_id = e.id,
        }
    }
    // None of the parsers succeeded, return an error
    let pos = if let Some(t) = input.first() {
        t.pos
    } else {
        Default::default()
    };
    let e = ParseErr::new(
        IzeErr::new(
            "None of the parsers passed to 'select' succeeded".into(),
            pos,
        ),
        last_failed_id,
        false,
    );
    Err(e)
}

//TODO: should it return an empty vec when not matched? It will make collecting easier and more consistent.
//      in this case, IzeOptResult won't be necessary anymore, and we can have only one single return type: IzeResult.
/// Optionally execute a parser.
pub fn optional<'a>(parsers: &'a [Parser], input: &'a [Token]) -> IzeOptResult<'a> {
    match into_opt_res(concat(parsers, input)) {
        Ok(r) => Ok(r),
        Err(e) => {
            if e.after_key {
                Err(e)
            } else {
                // TODO: if we could report the "e.id" in an Ok, we could improve parser error generation
                Ok((None, false))
            }
        }
    }
}

/// Execute an array of parsers and return the result in a vector. It fails if any of the parsers fail.
pub fn concat<'a>(parsers: &'a [Parser], mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    let mut did_parse_key = false;
    for parser in parsers {
        match parser.run(input) {
            Ok((Some((result, node)), after_key)) => {
                results.push(node);
                input = result;
                if after_key {
                    did_parse_key = true;
                }
            }
            Ok((None, after_key)) => {
                if after_key {
                    did_parse_key = true;
                }
            }
            Err(e) => {
                if did_parse_key {
                    return Err(e.into_key());
                } else {
                    return Err(e);
                }
            }
        }
    }
    Ok((input, results.into(), did_parse_key))
}

/// Execute a parser zero or more times and return the result in a vector.
pub fn zero_plus<'a>(parsers: &'a [Parser], mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    let mut did_parse_key = false;
    loop {
        match into_opt_res(concat(parsers, input)) {
            Ok((r, after_key)) => {
                if after_key {
                    did_parse_key = true;
                }
                if let Some((rest, node)) = r {
                    results.push(node);
                    input = rest;
                } else {
                    //TODO: Remove into_opt_res and this case. This can't actually happen, because concat always returns something.
                    break;
                }
            }
            Err(e) => {
                if e.after_key {
                    return Err(e);
                } else {
                    break;
                }
            }
        }
    }
    Ok((input, results.into(), did_parse_key))
}

/// Execute a parser one or more times and return the result in a vector.
pub fn one_plus<'a>(parsers: &'a [Parser], mut input: &'a [Token]) -> IzeResult<'a> {
    let mut did_parse_key = false;
    let mut results = vec![];
    let last_failed_id;
    loop {
        let (rest, node, after_key) = match concat(parsers, input) {
            Ok(result) => result,
            Err(e) => {
                if e.after_key {
                    return Err(e);
                } else {
                    last_failed_id = e.id;
                    break;
                }
            }
        };
        if after_key {
            did_parse_key = true;
        }
        results.push(node);
        input = rest;
    }
    if results.len() > 0 {
        Ok((input, results.into(), did_parse_key))
    } else {
        // The parsers must succeed at least once, return an error
        let pos = if let Some(t) = input.first() {
            t.pos
        } else {
            Default::default()
        };
        let e = ParseErr::new(
            IzeErr::new(
                "A parser passed to 'one_more' must succeed at least once".into(),
                pos,
            ),
            last_failed_id,
            did_parse_key,
        );
        Err(e)
    }
}

/// Matches one token usign a function to match. Used by [token_ident](crate::parser::token_ident), [token_int](crate::parser::token_int), etc.
fn token_match<'a>(
    matches: fn(&TokenKind) -> bool,
    err_msg: &'a str,
    input: &'a [Token],
) -> IzeResult<'a> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if matches(&input[0].kind) {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token.into(), false))
        } else {
            Err(IzeErr::new(err_msg.into(), pos).into())
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()).into())
    }
}

/// Parse a token matching a [TokenKind](crate::lexer::TokenKind).
pub fn token<'a>(token_kind: &'a TokenKind, input: &'a [Token]) -> IzeResult<'a> {
    if !input.is_empty() {
        let pos: TokenPos = input[0].pos;
        if token_kind == &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token.into(), false))
        } else {
            Err(IzeErr::new(format!("Expected token {:?}", token_kind), pos).into())
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()).into())
    }
}

/// Parse an identifier token.
pub fn token_ident(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::Identifier(_)),
        "Expected token identifier",
        input,
    )
}

/// Parse an integer literal token.
pub fn token_int(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::IntegerLiteral(_)),
        "Expected token integer",
        input,
    )
}

/// Parse a float literal token.
pub fn token_flt(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::FloatLiteral(_)),
        "Expected token float",
        input,
    )
}

/// Parse a boolean literal token.
pub fn token_bool(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::BooleanLiteral(_)),
        "Expected token boolean",
        input,
    )
}

/// Parse a string literal token.
pub fn token_str(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::StringLiteral(_)),
        "Expected token string",
        input,
    )
}

/// Parse a literal token.
pub fn token_literal(input: &[Token]) -> IzeResult {
    token_match(
        |t| {
            matches!(
                t,
                TokenKind::IntegerLiteral(_)
                    | TokenKind::FloatLiteral(_)
                    | TokenKind::BooleanLiteral(_)
                    | TokenKind::StringLiteral(_)
                    | TokenKind::NullLiteral
                    | TokenKind::NoneLiteral
            )
        },
        "Expected token string",
        input,
    )
}
