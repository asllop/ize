//! # Nom Experiment
//! 
//! Parser experiment, using `nom` instead of a manual recursive descendant parser.
//! 

use std::{
    str::FromStr,
    str::{self},
    fmt::Debug, fs::File,
    io::{prelude::*, BufReader},
};
use nom::{
    IResult,
    multi::{many1, many0},
    combinator::recognize,
    character::complete::{one_of, multispace0, alpha1},
    bytes::complete::{tag, is_not},
};
use logos::{Lexer, Logos, Skip};

fn parse_callback<T>(lex: &mut Lexer<TokenKind>) -> T where T: FromStr + Debug {
    lex.slice().parse().ok().unwrap()
}

//PROBLEM:  because we construct multiple lexers, we don't have a common reference for the input, the origin
//          of &str changes every time we build a new lexer object.
// We have to use a different type for extras, something that carries the offset of the current &str to be able to calculate columns.

fn newline_callback(lex: &mut Lexer<TokenKind>) -> Skip {
    lex.extras.line += 1;
    //TODO: compute pos
    //lex.extras.start_col = lex.span().end;
    Skip
}

#[derive(Default, Debug, Clone, Copy)]
struct Pos {
    line: usize,
    start_col: usize,
    end_col: usize,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = Pos, skip r"[ \t]+", skip r"//.*")]
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

fn build_err(msg: &str) -> nom::Err<nom::error::Error<&str>> {
    let e = nom::error::Error::new(msg, nom::error::ErrorKind::Fail);
    nom::Err::Error(e)
}

fn finished_scanning_tokens(input: &str) -> bool {
    let mut lex = TokenKind::lexer(input);
    lex.next().is_none()
}

/// Alternative to `next_token`, can be used as a `nom` parser function.
fn scan_token(input: &str, current_pos: Pos) -> IResult<&str, (Pos, TokenKind)> {
    let mut lex = TokenKind::lexer_with_extras(input, current_pos);
    match lex.next() {
        Some(r) => match r {
            Ok(token_kind) => {
                //TODO: compute pos
                IResult::Ok((lex.remainder(), (lex.extras, token_kind)))
            },
            Err(_) => Err(build_err("Bad token")),
        },
        None => {
            Err(build_err("No more tokens"))
        },
    }
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    pos: Pos,
}

impl Token {
    fn new(pos: Pos, kind: TokenKind) -> Self {
        Self {
            kind, pos
        }
    }

    fn as_id(self) -> String {
        if let TokenKind::Ident(s) = self.kind {
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

fn token_semicolon(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::Semicolon))) => IResult::Ok((rest, Token::new(pos, TokenKind::Semicolon))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_let(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::Let))) => IResult::Ok((rest, Token::new(pos, TokenKind::Let))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_open_parenth(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::OpenParenth))) => IResult::Ok((rest, Token::new(pos, TokenKind::OpenParenth))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_close_parenth(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::ClosingParenth))) => IResult::Ok((rest, Token::new(pos, TokenKind::ClosingParenth))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_id(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::Ident(id)))) => IResult::Ok((rest, Token::new(pos, TokenKind::Ident(id)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_int(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::IntegerLiteral(i)))) => IResult::Ok((rest, Token::new(pos, TokenKind::IntegerLiteral(i)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_bool(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::BooleanLiteral(b)))) => IResult::Ok((rest, Token::new(pos, TokenKind::BooleanLiteral(b)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_str(input: &str) -> IResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (pos, TokenKind::StringLiteral(s)))) => IResult::Ok((rest, Token::new(pos, TokenKind::StringLiteral(s)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
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
    } else if let Ok((rest, token)) = token_bool(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else {
        let e = nom::error::Error::new("Error parsing primary expr'", nom::error::ErrorKind::Fail);
        Err(nom::Err::Error(e))
    }
}

fn main() {
    let file_path = "izeware/experiment2.iz";
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let mut input = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");
    //let mut current_pos = Pos::default();

    while !finished_scanning_tokens(input) {
        let (rest, matched) = expr(input).expect("Error parsing expr");
        dbg!(rest, matched);
        input = rest;

        // let token_kind;
        // (input, (current_pos, token_kind)) = scan_token(input, current_pos).expect("Error scanning token");
        // println!("{:?} at {:?}", token_kind, current_pos);
    }
}