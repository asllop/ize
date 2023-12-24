//! # Nom Experiment
//! 
//! Parser experiment, using `nom` instead of a manual recursive descendant parser.
//! 

use nom::{
    IResult,
    multi::{many1, many0},
    combinator::recognize,
    character::complete::{one_of, multispace0, alpha1},
    bytes::complete::{tag, is_not},
};

const CODE: &str = r#"
    100
    "Hello world"
    myVar

    let salute "Hello World"

    let A let B let C 111

    let num 1234 ; num ; A ; let A let B let C 111

    let num ("hello";100)
"#;

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

fn main() {
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
