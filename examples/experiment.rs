//! # Nom Experiment
//! 
//! Parser experiment, using `nom` instead of a manual recursive descendant parser.
//! 

use std::{
    str::FromStr,
    fmt::Debug,
};
use nom::{
    IResult,
    multi::{many1, many0},
    combinator::recognize,
    character::complete::{one_of, multispace0, alpha1},
    bytes::complete::{tag, is_not},
};
use logos::{Lexer, Logos, Skip};

const CODE: &str = r#"
    // The program starts here
    
    100
    "Hello world"
    myVar true false

    let salute "Hello World"

    let A let B let C 111

    let num 1234 ; num ; A ; let A let B let C 111

    let num ("hello";100)
"#;

fn parse_callback<T>(lex: &mut Lexer<TokenKind>) -> T where T: FromStr + Debug {
    lex.slice().parse().ok().unwrap()
}

fn newline_callback(lex: &mut Lexer<TokenKind>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

#[derive(Default, Debug, Clone, Copy)]
struct TokenPos {
    line: usize,
    column: usize,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = TokenPos, skip r"[ \t]+", skip r"//.*")]
/// List of recognized tokens, requiered by [logos].
enum TokenKind {
    #[regex(r"\n", newline_callback)]
    Newline,

    #[token("(")]
    OpenParenth,
    #[token(")")]
    ClosingParenth,
    #[token("[")]
    OpenClause,
    #[token("]")]
    ClosingClause,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    LesserThan,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GtEqual,
    #[token("<=")]
    LtEqual,
    #[token("&")]
    And,
    #[token("&&")]
    TwoAnds,
    #[token("|")]
    Or,
    #[token("||")]
    TwoOrs,
    #[token("!")]
    Not,
    #[token("==")]
    TwoEquals,
    #[token("!=")]
    NotEqual,
    #[token(".")]
    Dot,
    #[token("...")]
    ThreeDots,
    #[token("as")]
    As,
    #[token("->")]
    Arrow,
    #[token("let")]
    Let,

    // Primaries

    #[regex("-?[0-9]+", parse_callback::<i64>)]
    IntegerLiteral(i64),
    //TODO: scientific notation: -9.09E-3, 9.09E+3
    #[regex(r#"-?[0-9]+\.[0-9]+"#, parse_callback::<f64>)]
    FloatLiteral(f64),
    #[regex("true", parse_callback::<bool>)]
    #[regex("false", parse_callback::<bool>)]
    BooleanLiteral(bool),
    #[token("none")]
    NoneLiteral,
    #[token("null")]
    NullLiteral,
    #[regex(r#""([^"\\]|\\"|\\)*""#, parse_callback::<String>)]
    StringLiteral(String),
    #[regex(r#"[\p{Alphabetic}_]([\p{Alphabetic}_0-9]+)?"#, parse_callback::<String>)]
    Ident(String),

    // Types

    #[token("Integer")]
    IntegerType,
    #[token("Float")]
    FloatType,
    #[token("Boolean")]
    BooleanType,
    #[token("String")]
    StringType,
    #[token("Null")]
    NullType,
    #[token("None")]
    NoneType,
    #[token("Map")]
    MapType,
    #[token("List")]
    ListType,
    #[token("Mux")]
    MuxType,
    #[token("Tuple")]
    TupleType,

    //TODO: can we scan complex types like Map[String, Integer] or List[Map[String, Integer]] ?

    // Commands

    #[token("model")]
    Model,
    #[token("transfer")]
    Transfer,
    #[token("pipe")]
    Pipe,
    #[token("run")]
    Run,
    #[token("import")]
    Import,

    // Decision

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("select")]
    Select,
    #[token("unwrap")]
    Unwrap,
}

fn next_token(lex: &mut Lexer<TokenKind>) -> Option<Result<(TokenPos, TokenKind), &'static str>> {
    match lex.next() {
        Some(r) => match r {
            Ok(token_kind) => {
                Some(Result::Ok((lex.extras, token_kind)))
            },
            Err(_) => {
                Some(Result::Err("Unrecognized token"))
            },
        },
        None => {
            None
        },
    }
}

fn build_err(msg: &str) -> nom::Err<nom::error::Error<&str>> {
    let e = nom::error::Error::new(msg, nom::error::ErrorKind::Fail);
    nom::Err::Error(e)
}

fn finished_scanning_tokens(input: &str) -> bool {
    let mut lex = TokenKind::lexer(input);
    lex.next().is_none()
}

/// Alternative to `next_token`, can be used as a `nom` parser function.
fn scan_token(input: &str, current_pos: TokenPos) -> IResult<&str, (TokenPos, TokenKind)> {
    let mut lex = TokenKind::lexer_with_extras(input, current_pos);
    match lex.next() {
        Some(r) => match r {
            Ok(token_kind) => IResult::Ok((lex.remainder(), (lex.extras, token_kind))),
            Err(_) => Err(build_err("Bad token")),
        },
        None => {
            Err(build_err("No more tokens"))
        },
    }
}

#[derive(Debug)]
enum Token {
    Semicolon,
    OpenParenth,
    CloseParenth,
    Let,
    Int(i64),
    Id(String),
    Str(String),
}

impl Token {
    fn as_id(self) -> String {
        if let Token::Id(s) = self {
            s
        } else {
            panic!("Token is not an Id")
        }
    }
}

#[derive(Debug)]
enum Expression {
    Primary(Token),
    Chain(Vec<Expression>),
    Group(Box<Expression>),
    Let {
        id: String,
        expr: Box<Expression>,
    },
}

// Skip spaces, tabs and newlines
fn trim(input: &str) -> IResult<&str, &str> {
    //TODO: update pos while scan chars
    multispace0(input)
}

//TODO: use Logos to parse tokens in a more generic way. Instead of generatic a custom method to parse wach token,
//      use the lexer to generate a Token enum for each recognized token automatically.

fn token_semicolon(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    match tag::<&str, &str, _>(";")(rest) {
        Ok((rest, _)) => IResult::Ok((rest, Token::Semicolon)),
        Err(e) => Err(e),
    }
}

fn token_open_parenth(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    match tag::<&str, &str, _>("(")(rest) {
        Ok((rest, _)) => IResult::Ok((rest, Token::OpenParenth)),
        Err(e) => Err(e),
    }
}

fn token_close_parenth(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    match tag::<&str, &str, _>(")")(rest) {
        Ok((rest, _)) => IResult::Ok((rest, Token::CloseParenth)),
        Err(e) => Err(e),
    }
}

fn token_let(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    match tag::<&str, &str, _>("let")(rest) {
        Ok((rest, _)) => IResult::Ok((rest, Token::Let)),
        Err(e) => Err(e),
    }
}

// naive ID, only ascii chars
fn token_id(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    match alpha1(rest) {
        Ok((rest, matched)) => IResult::Ok((rest, Token::Id(matched.into()))),
        Err(e) => Err(e),
    }
}

fn token_int(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    match recognize(
        many1(
            one_of("0123456789")
        )
    )(rest) {
        Ok((rest, matched)) => {
            let i: i64 = matched.parse().unwrap();
            IResult::Ok((rest, Token::Int(i)))
        }
        Err(e) => Err(e),
    }
}

// naive str, without scape sequences
fn token_str(input: &str) -> IResult<&str, Token> {
    let (rest, _) = trim(input)?;
    let (rest, _) = tag("\"")(rest)?;
    let (rest, the_str) = many0(is_not("\""))(rest)?;
    let (rest, _) = tag("\"")(rest)?;
    IResult::Ok((rest, Token::Str(the_str.into_iter().collect())))
}

fn expr(input: &str) -> IResult<&str, Expression> {
    expr_chain(input)
}

fn expr_chain(mut input: &str) -> IResult<&str, Expression> {
    let mut expressions = vec![];
    let rest: &str = loop {
        let (rest, expr) = expr_let(input)?;
        expressions.push(expr);
        match token_semicolon(rest) {
            Ok((rest, _)) => input = rest,
            Err(_) => break rest,
        }
    };
    if expressions.len() == 1 {
        IResult::Ok((rest, expressions.pop().unwrap()))
    } else {
        Result::Ok((rest, Expression::Chain(expressions)))
    }
}

fn expr_let(input: &str) -> IResult<&str, Expression> {
    match token_let(input) {
        Ok((rest, _)) => {
            let (rest, id_token) = token_id(rest)?;
            let (rest, expr) = expr_let(rest)?;
            let let_expr = Expression::Let { id: id_token.as_id(), expr: Box::new(expr) };
            IResult::Ok((rest, let_expr))
        },
        Err(_) => {
            expr_group(input)
        },
    }
}

fn expr_group(input: &str) -> IResult<&str, Expression> {
    match token_open_parenth(input) {
        Ok((rest, _)) => {
            let (rest, expr) = expr(rest)?;
            let group_expr = Expression::Group(Box::new(expr));
            let (rest, _) = token_close_parenth(rest)?;
            IResult::Ok((rest, group_expr))
        },
        Err(_) => {
            expr_primary(input)
        },
    }
}

fn expr_primary(input: &str) -> IResult<&str, Expression> {
    if let Ok((rest, token)) = token_id(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else if let Ok((rest, token)) = token_int(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else if let Ok((rest, token)) = token_str(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else {
        let e = nom::error::Error::new("Error parsing primary expr'", nom::error::ErrorKind::Fail);
        Err(nom::Err::Error(e))
    }
}

fn _main() {
    let rest = CODE;

    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
    let (rest, matched) = expr(rest).expect("Error parsing expr");
    dbg!(rest, matched);
}

fn __main() {
    let input = CODE;

    let mut lex = TokenKind::lexer(input);
    loop {
        match next_token(&mut lex) {
            Some(r) => {
                let (token_pos, token_kind) = r.expect("Bad token");
                println!("{:?} at {:?}", token_kind , token_pos);
            },
            None => break,
        }
    }
}

fn main() {
    let mut input = CODE;
    let mut current_pos = TokenPos::default();

    while !finished_scanning_tokens(input) {
        let token_kind;
        (input, (current_pos, token_kind)) = scan_token(input, current_pos).expect("Error scanning token");
        println!("{:?} at {:?}", token_kind, current_pos);
    }
}
