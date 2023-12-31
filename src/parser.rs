use crate::{
    ast::AstNode,
    common::TokenPos,
    err::IzeErr,
    lexer::{Token, TokenKind},
};

/// Result type alias for parsers.
pub type IzeResult<'a> = Result<(&'a [Token], AstNode), IzeErr>;

/// Result type alias for parsers with optional result.
pub type IzeOptResult<'a> = Result<Option<(&'a [Token], AstNode)>, IzeErr>;

/// Convert [`IzeResult`] into [`IzeOptResult`].
pub fn into_opt_res(value: IzeResult) -> IzeOptResult {
    let res_tuple = value?;
    Ok(Some(res_tuple))
}

/// Parser element.
#[derive(Clone)]
pub enum Parser<'a> {
    /// Generic parser function.
    Fn(fn(&[Token]) -> IzeResult),
    /// Token parser.
    Tk(TokenKind),
    /// Select parser composer. Executes one from a list of parsers, the first that matches.
    Sel(&'a [Parser<'a>]),
    /// Concatenate parser composers. Executes a list of parsers.
    Con(&'a [Parser<'a>]),
    /// Optional parser composer. Optionally executes a parser.
    Opt(&'a Parser<'a>),
    /// Zero-plus parser composer. Executes a parser zero or more times.
    Zpl(&'a Parser<'a>),
    /// One-plus parser composer. Executes a parser one or more times.
    Opl(&'a Parser<'a>),
}

impl<'a> Parser<'a> {
    /// Run a parser element.
    pub fn run(&self, input: &'a [Token]) -> IzeOptResult {
        match self {
            Parser::Fn(parser_fn) => into_opt_res(parser_fn(input)),
            Parser::Tk(token_kind) => into_opt_res(token(token_kind, input)),
            Parser::Sel(parsers) => into_opt_res(select(parsers, input)),
            Parser::Con(parsers) => into_opt_res(concat(parsers, input)),
            Parser::Opt(parser) => optional(parser, input),
            Parser::Zpl(parser) => into_opt_res(zero_plus(parser, input)),
            Parser::Opl(parser) => into_opt_res(one_plus(parser, input)),
        }
    }
}

/// Select a parser from a list, the first that sucseeds.
pub fn select<'a>(parsers: &'a [Parser], input: &'a [Token]) -> IzeResult<'a> {
    for parser in parsers {
        if let Ok(Some((rest, node))) = parser.run(input) {
            return Ok((rest, node));
        }
    }
    // None of the parsers succeeded, return an error
    let pos = if let Some(t) = input.first() {
        t.pos
    } else {
        Default::default()
    };
    Err(IzeErr::new(
        "None of the parsers passed to 'select' succeeded".into(),
        pos,
    ))
}

/// Optionally execute a parser.
pub fn optional<'a>(parser: &'a Parser<'a>, input: &'a [Token]) -> IzeOptResult<'a> {
    if let Ok(r) = parser.run(input) {
        Ok(r)
    } else {
        Ok(None)
    }
}

/// Execute an array of parsers and return the result in a vector. It fails if any of the parsers fail.
pub fn concat<'a>(parsers: &'a [Parser], mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    for parser in parsers {
        if let Some((result, node)) = parser.run(input)? {
            results.push(node);
            input = result;
        }
    }
    Ok((input, results.into()))
}

/// Execute a parser zero or more times and return the result in a vector.
pub fn zero_plus<'a>(parser: &'a Parser<'a>, mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    while let Ok(result) = parser.run(input) {
        if let Some((rest, node)) = result {
            results.push(node);
            input = rest;
        } else {
            break;
        }
    }
    Ok((input, results.into()))
}

/// Execute a parser one or more times and return the result in a vector.
pub fn one_plus<'a>(parser: &'a Parser<'a>, mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    while let Ok(result) = parser.run(input) {
        if let Some((rest, node)) = result {
            results.push(node);
            input = rest;
        } else {
            break;
        }
    }
    if results.len() > 0 {
        Ok((input, results.into()))
    } else {
        // The parsers must succeed at least once, return an error
        let pos = if let Some(t) = input.first() {
            t.pos
        } else {
            Default::default()
        };
        Err(IzeErr::new(
            "A parser passed to 'one_more' must succeed at least once".into(),
            pos,
        ))
    }
}

pub fn token_match<'a>(
    matches: fn(&TokenKind) -> bool,
    err_msg: &'a str,
    input: &'a [Token],
) -> IzeResult<'a> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if matches(&input[0].kind) {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token.into()))
        } else {
            Err(IzeErr::new(err_msg.into(), pos))
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()))
    }
}

pub fn token<'a>(token_kind: &'a TokenKind, input: &'a [Token]) -> IzeResult<'a> {
    if !input.is_empty() {
        let pos: TokenPos = input[0].pos;
        if token_kind == &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token.into()))
        } else {
            Err(IzeErr::new(format!("Expected token {:?}", token_kind), pos))
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()))
    }
}

pub fn token_ident(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::Identifier(_)),
        "Expected token identifier",
        input,
    )
}

pub fn token_int(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::IntegerLiteral(_)),
        "Expected token integer",
        input,
    )
}

pub fn token_flt(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::FloatLiteral(_)),
        "Expected token float",
        input,
    )
}

pub fn token_bool(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::BooleanLiteral(_)),
        "Expected token boolean",
        input,
    )
}

pub fn token_str(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::StringLiteral(_)),
        "Expected token string",
        input,
    )
}
