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

/// Result type alias for parsers.
pub type IzeResult<'a> = Result<(&'a [Token], AstNode, bool), ParseErr>;

/// Result type alias for parsers with optional result.
pub type IzeOptResult<'a> = Result<Option<(&'a [Token], AstNode, bool)>, ParseErr>;

/// Convert [IzeResult](crate::parser::IzeResult) into [IzeOptResult](crate::parser::IzeOptResult).
pub fn into_opt_res(value: IzeResult) -> IzeOptResult {
    Ok(Some(value?))
}

/* TODO:
    Crea una nova variant de Parser que podríem anomenar "Key". Aquesta variant fa que si aquesta part del parser
    funciona però la resta no, vol dir que és l'expressió correcta però amb un error i per tant cal no seguir amb
    la precedència, sinó generar un error adient.

    Per exemple, "let var 100". La key aquí és el token "let", si hi és, però la resta del parser falla, hem de generar
    un error d'expressió let mal formada.

    Un altre exemple, "100 + var". Aquí la keu és el token "+".
*/

/* TODO:
    Si el parser falla després d'un key (que ha funcionat), hem de retornar un error, però marcar que ha fallat després d'un key.
    Així podem distingir el cas ha fallat després d'un key i hem d'abortar, del cas ha fallat i no és aquest parser.

*/

/* Usage:
    Grammar::new(&[...]).run(input, ...)
*/

/* Exemple complex amb multiples keys niuats:

unwrap expr as ident (
    expr => expr,
    expr => expr,
    expr => expr
)

concat(
    Key(Tk("unwrap")), Fn(expr), Tk("as"), Identifier,
    Tk("("),
    Concat(Fn(expr), Key(Tk("=>")), Fn(expr))
    ZeroPlus(
        Concat(Tk(","), Fn(expr), Key(Tk("=>")), Fn(expr))
    ),
    Tk(")")
)
*/

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
            Parser::Zpl(parser) => into_opt_res(zero_plus(parser, input)),
            Parser::Opl(parser) => into_opt_res(one_plus(parser, input)),
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
    let e = ParseErr::new(IzeErr::new(
        "None of the parsers passed to 'select' succeeded".into(),
        pos,
    ), did_parse_key);
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
            },
            Ok(None) => {},
            Err(e) => {
                if did_parse_key {
                    return Err(e.into_key());
                } else {
                    return Err(e);
                }
            },
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
        let e = ParseErr::new(IzeErr::new(
            "A parser passed to 'one_more' must succeed at least once".into(),
            pos,
        ), did_parse_key);
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
