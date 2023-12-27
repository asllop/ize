//! # Parse Composer Experiment
//! 
//! Use a parser composed inspired by `nom`.
//! 

use std::{
    str::FromStr,
    str::{self},
    fmt::Debug, fs::File,
    io::{prelude::*, BufReader},
};
use ize::IzeErr;
use logos::{Lexer, Logos, Skip};

pub type IzeResult<I, O> = Result<(I, O), IzeErr>;

fn parse_callback<T>(lex: &mut Lexer<TokenKind>) -> T where T: FromStr + Debug {
    lex.slice().parse().ok().unwrap()
}

//PROBLEM:  because we construct multiple lexers, we don't have a common reference for the input, the origin
//          of &str changes every time we build a new lexer object.
// We have to use a different type for extras, something that carries the offset of the current &str to be able to calculate columns.

fn newline_callback(lex: &mut Lexer<TokenKind>) -> Skip {
    lex.extras.line += 1;
    lex.extras.last_token_end = lex.span().end + lex.extras.offset;
    //println!("Update extras = {:?}", lex.extras);
    //TODO: compute pos
    //lex.extras.start_col = lex.span().end;
    Skip
}

#[derive(Default, Debug, Clone, Copy)]
struct LexExtras {
    line: usize,
    offset: usize,
    last_token_end: usize,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexExtras, skip r"[ \t]+", skip r"//.*")]
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

//TODO: token pos
fn build_err(msg: &str) -> IzeErr {
    IzeErr { message: msg.into(), pos: Default::default() }
}

fn finished_scanning_tokens(input: &str) -> bool {
    let mut lex = TokenKind::lexer(input);
    lex.next().is_none()
}

fn scan_token(input: &str, extras: LexExtras) -> IzeResult<&str, (LexExtras, TokenKind)> {
    let mut lex = TokenKind::lexer_with_extras(input, extras);
    match lex.next() {
        Some(r) => match r {
            Ok(token_kind) => {
                //TODO: compute pos
                IzeResult::Ok((lex.remainder(), (lex.extras, token_kind)))
            },
            Err(_) => Err(build_err("Bad token")),
        },
        None => {
            Err(build_err("No more tokens"))
        },
    }
}

#[derive(Default, Debug, Clone, Copy)]
struct Pos {
    line: usize,
    start_col: usize,
    end_col: usize,
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

    fn as_ident(self) -> Option<String> {
        if let TokenKind::Ident(s) = self.kind {
            Some(s)
        } else {
            None
        }
    }
}

/* TODO: Grammar type for recursive descendent parsers (RDP)

Els parsers d'expressions RDP es poden dividir en dos tipus:
1. Els que tenen un (o més) token a l'inici i si no hi és, fallback a la següent en precedència. Exemples: let, if-else, select, group.
2. Els parsen una expressió (la següent en precedència) i després esperen algun token i si no hi és, retornen l'expressió parsada com a fallback. Exemples: chain, binary operators.

Podem crear un tipus Grammar per definir parsers d'expressions d'aquests dos tipus. Aquest tipus ens ha de permetre:
- Definir quina mena de parse és, dels enumerats anterioirment.
- Definir una gramàtica que ens permeti parsar una expressió. Definint els camins esmentats segons el tipus.
- Definir la següent expressió en precedència.
- Definir un col·lector, que genera l'expressió a partir dels tokens llegits.
*/

#[derive(Debug)]
enum Expression {
    Primary(Token),
    Chain(Vec<Expression>),
    Group(Box<Expression>),
    Let {
        ident: String,
        expr: Box<Expression>,
    },
}

fn token(token_kind: TokenKind, input: &str) -> IzeResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (extras, t))) => if token_kind == t {
            IzeResult::Ok((rest, Token::new(Default::default(), TokenKind::Semicolon)))
        } else {
            Err(build_err("Incorrect token"))
        },
        Err(e) => Err(e),
    }
}

fn token_ident(input: &str) -> IzeResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (extras, TokenKind::Ident(id)))) => IzeResult::Ok((rest, Token::new(Default::default(), TokenKind::Ident(id)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_int(input: &str) -> IzeResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (extras, TokenKind::IntegerLiteral(i)))) => IzeResult::Ok((rest, Token::new(Default::default(), TokenKind::IntegerLiteral(i)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_bool(input: &str) -> IzeResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (extras, TokenKind::BooleanLiteral(b)))) => IzeResult::Ok((rest, Token::new(Default::default(), TokenKind::BooleanLiteral(b)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn token_str(input: &str) -> IzeResult<&str, Token> {
    //TODO: get actual pos and pass it to scan_token
    match scan_token(input, Default::default()) {
        Ok((rest, (extras, TokenKind::StringLiteral(s)))) => IzeResult::Ok((rest, Token::new(Default::default(), TokenKind::StringLiteral(s)))),
        Err(e) => Err(e),
        _ => {
            Err(build_err("Incorrect token"))
        }
    }
}

fn expr(input: &str) -> IzeResult<&str, Expression> {
    expr_chain(input)
}

fn expr_chain(mut input: &str) -> IzeResult<&str, Expression> {
    let mut expressions = vec![];
    let rest: &str = loop {
        let (rest, expr) = expr_let(input)?;
        expressions.push(expr);
        match token(TokenKind::Semicolon, rest) {
            Ok((rest, _)) => input = rest,
            Err(_) => break rest,
        }
    };
    if expressions.len() == 1 {
        IzeResult::Ok((rest, expressions.pop().unwrap()))
    } else {
        Result::Ok((rest, Expression::Chain(expressions)))
    }
}

fn expr_let(input: &str) -> IzeResult<&str, Expression> {
    match token(TokenKind::Let, input) {
        Ok((rest, _)) => {
            let (rest, ident_token) = token_ident(rest)?;
            let (rest, expr) = expr_let(rest)?;
            let let_expr = Expression::Let { ident: ident_token.as_ident().unwrap(), expr: Box::new(expr) };
            IzeResult::Ok((rest, let_expr))
        },
        Err(_) => {
            expr_group(input)
        },
    }
}

fn expr_group(input: &str) -> IzeResult<&str, Expression> {
    match token(TokenKind::OpenParenth, input) {
        Ok((rest, _)) => {
            let (rest, expr) = expr(rest)?;
            let group_expr = Expression::Group(Box::new(expr));
            let (rest, _) = token(TokenKind::ClosingParenth, rest)?;
            IzeResult::Ok((rest, group_expr))
        },
        Err(_) => {
            expr_primary(input)
        },
    }
}

fn expr_primary(input: &str) -> IzeResult<&str, Expression> {
    //TODO: parse the rest of literals (None, Null).
    if let Ok((rest, token)) = token_ident(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else if let Ok((rest, token)) = token_int(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else if let Ok((rest, token)) = token_str(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else if let Ok((rest, token)) = token_bool(input) {
        Result::Ok((rest, Expression::Primary(token)))
    } else {
        Err(build_err("Error parsing primary expr"))
    }
}

//TODO: use ParseInput instead of &str to hold the parser input. This way we can keep track of the offset respect to the original str
//      every time we slice it.

struct ParserInput<'a> {
    slice: &'a str,
    offset: usize,
}

fn main() {
    let file_path = "izeware/experiment2.iz";
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let mut input = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");

    while !finished_scanning_tokens(input) {
        let (rest, matched) = expr(input).expect("Error parsing expr");
        println!("Expression = {:#?}", matched);
        input = rest;
    }
}
