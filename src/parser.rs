//! # Parser
//!
//! Parser combinator infrastructure. Contains the primitives used to define the parser for the IZE language. Inspired by [Nom](https://github.com/rust-bakery/nom).

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
    /// Error happend after a Key parser.
    pub after_key: bool,
}

impl ParseErr {
    /// new parse error.
    pub fn new(err: IzeErr, after_key: bool) -> Self {
        Self { err, after_key }
    }

    /// Into parts
    pub fn into_parts(self) -> (IzeErr, bool) {
        (self.err, self.after_key)
    }

    /// Convert error into an eror with after_key = true.
    pub fn into_key(mut self) -> Self {
        self.after_key = true;
        self
    }
}

impl From<IzeErr> for ParseErr {
    fn from(value: IzeErr) -> Self {
        ParseErr::new(value, false)
    }
}

impl From<ParseErr> for IzeErr {
    fn from(value: ParseErr) -> Self {
        value.err
    }
}

/* TODO: OPTION 1
    How to generate more detailed errors. For example, in an If-Else expression, we want to know if the error happened while trying to
    match the closing parenthesis, or the "else" token, etc. We need more information about where the parser failed.

    We need a new return type, one that contains 3 possibilities:
    - Parsed, return the node.
    - Fail after a key parser. Return an error plus the correctly parsed part (if node is a vector).
    - Failed before or during a key parser. Return an error.

    We should also unify IzeResult and IzeOptResult. A type alias is not enough, we need a custom enum.

    enum ParseResult {
        Ok(&'a [Token], AstNode, bool),
        Partial(&'a [Token], AstNode, IzeErr),
        Err(IzeErr),
    }

    Potser no calen els Key parsers. Simplement ens cal retornar el resultat parcial sempre que hi hagi un error. I és el codi
    que tracta el cas d'error qui decideix si hem de cridar precedència o generar un error.

    Per tant només calen dos casos: Ok i Err. I l'Err retorna el resultat parcial del parsing.

    enum ParseResult {
        Ok(&'a [Token], AstNode),
        Err(IzeErr, AstNode),
    }

    Potser podem seguir l'alias:

    pub type IzeResult<'a> = Result<Option<(&'a [Token], AstNode)>, ParseErr>;

    Redefinir: ParseErr {
        err: IzeErr,
        partial: AstNode,
    }
*/

/* TODO: OPTION 2

    Farem que cada Parser tingui un ID (int) associat. Quan falli, l'error contindrà aquest ID que serà enviat a un callback
    error handdler. Amb això ja no calen els key parsers.

    struct ParseErr {
        err: IzeErr,
        id: usize,
    }

    pub enum Parser<'a> {
        Fn(fn(&[Token]) -> IzeResult, usize),
        Tk(TokenKind, usize),
        Sel(&'a [Parser<'a>], usize),
        Con(&'a [Parser<'a>], usize),
        Opt(&'a Parser<'a>),
        Zero(&'a Parser<'a>),
        One(&'a Parser<'a>, usize),
    }
*/

/// Result type alias for parsers.
pub type IzeResult<'a> = Result<(&'a [Token], AstNode, bool), ParseErr>;

/// Result type alias for parsers with optional result.
pub type IzeOptResult<'a> = Result<Option<(&'a [Token], AstNode, bool)>, ParseErr>;

/// Convert [IzeResult](crate::parser::IzeResult) into [IzeOptResult](crate::parser::IzeOptResult).
pub fn into_opt_res(value: IzeResult) -> IzeOptResult {
    Ok(Some(value?))
}

/// Define a grammar. Version 1
pub fn grammar<'a>(
    parser: &'a Parser<'a>,
    input: &'a [Token],
    success: fn(AstNode) -> AstNode,
    error: fn(ParseErr) -> IzeResult<'a>,
    precedence: fn(&'a [Token]) -> IzeResult<'a>,
) -> IzeResult<'a> {
    match parser.run(input) {
        Ok(Some((rest, node, _))) => {
            let result_node = success(node);
            Ok((rest, result_node, false))
        }
        Ok(None) => precedence(input),
        Err(e) => {
            if e.after_key {
                // Error parsing expression
                error(e)
            } else {
                // Precedence
                precedence(input)
            }
        }
    }
}

/// Define a grammar. Version 2
pub fn def_grammar<'a>(
    parsers: &'a [Parser<'a>],
    input: &'a [Token],
    collector: fn(input: &'a [Token], AstNode) -> IzeResult<'a>,
) -> IzeResult<'a> {
    match concat(parsers, input) {
        Ok((rest, node, _)) => collector(rest, node),
        Err(e) => Err(e)
    }
}

/// Parser element.
#[derive(Clone)]
pub enum Parser<'a> {
    /// Key parser.
    Key(&'a Parser<'a>),
    /// Generic parser function.
    Fn(fn(&[Token]) -> IzeResult),
    /// Token parser.
    Tk(TokenKind),
    /// Select parser composer. Executes one from a list of parsers, the first that matches.
    Sel(&'a [Parser<'a>]),
    /// Concatenate parser composers. Executes a list of parsers.
    Con(&'a [Parser<'a>]),

    //TODO: Opt, Zero and One should hold a parser array.

    /// Optional parser composer. Optionally executes a parser.
    Opt(&'a Parser<'a>),
    /// Zero-plus parser composer. Executes a parser zero or more times.
    Zero(&'a Parser<'a>),
    /// One-plus parser composer. Executes a parser one or more times.
    One(&'a Parser<'a>),
}

impl<'a> Parser<'a> {
    /// Run a parser element.
    pub fn run(&self, input: &'a [Token]) -> IzeOptResult {
        match self {
            Parser::Key(parser) => match parser.run(input) {
                Ok(Some((rest, node, _))) => Ok(Some((rest, node, true))),
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            },
            Parser::Fn(parser_fn) => into_opt_res(parser_fn(input)),
            Parser::Tk(token_kind) => into_opt_res(token(token_kind, input)),
            Parser::Sel(parsers) => into_opt_res(select(parsers, input)),
            Parser::Con(parsers) => into_opt_res(concat(parsers, input)),
            Parser::Opt(parser) => optional(parser, input),
            Parser::Zero(parser) => into_opt_res(zero_plus(parser, input)),
            Parser::One(parser) => into_opt_res(one_plus(parser, input)),
        }
    }
}

/// Select a parser from a list, the first that succeeds.
pub fn select<'a>(parsers: &'a [Parser], input: &'a [Token]) -> IzeResult<'a> {
    let mut did_parse_key = false;
    for parser in parsers {
        if let Ok(Some((rest, node, is_key))) = parser.run(input) {
            if is_key {
                did_parse_key = is_key;
            }
            return Ok((rest, node, did_parse_key));
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
        did_parse_key,
    );
    Err(e)
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
    let mut did_parse_key = false;
    for parser in parsers {
        match parser.run(input) {
            Ok(Some((result, node, is_key))) => {
                results.push(node);
                input = result;
                if is_key {
                    did_parse_key = is_key;
                }
            }
            Ok(None) => {}
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
pub fn zero_plus<'a>(parser: &'a Parser<'a>, mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    let mut did_parse_key = false;
    while let Ok(result) = parser.run(input) {
        if let Some((rest, node, is_key)) = result {
            results.push(node);
            input = rest;
            if is_key {
                did_parse_key = is_key;
            }
        } else {
            break;
        }
    }
    Ok((input, results.into(), did_parse_key))
}

/// Execute a parser one or more times and return the result in a vector.
pub fn one_plus<'a>(parser: &'a Parser<'a>, mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    let mut did_parse_key = false;
    while let Ok(result) = parser.run(input) {
        if let Some((rest, node, is_key)) = result {
            results.push(node);
            input = rest;
            if is_key {
                did_parse_key = is_key;
            }
        } else {
            break;
        }
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
