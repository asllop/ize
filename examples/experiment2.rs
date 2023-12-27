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
use logos::{Lexer, Logos, Skip};

type IzeResult<I, O> = Result<(I, O), IzeErr>;

#[derive(Debug)]
/// Compiler error.
struct IzeErr {
    /// Error message.
    message: String,
    /// Position where the error was found.
    pos: Pos,
}

fn parse_callback<T>(lex: &mut Lexer<TokenKind>) -> T where T: FromStr + Debug {
    lex.slice().parse().ok().unwrap()
}

fn newline_callback(lex: &mut Lexer<TokenKind>) -> Skip {
    lex.extras.line += 1;
    lex.extras.pos_last_eol = lex.span().end;
    Skip
}

#[derive(Default, Debug, Clone, Copy)]
struct LexExtras {
    line: usize,
    pos_last_eol: usize,
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

fn build_err(msg: &str, pos: Pos) -> IzeErr {
    IzeErr { message: msg.into(), pos: pos }
}

#[derive(Default, Debug, Clone, Copy)]
struct Pos {
    line: usize,
    start_col: usize,
    end_col: usize,
}

impl Pos {
    fn new(line: usize, start_col: usize, end_col: usize) -> Self {
        Self { line, start_col, end_col }
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

fn token(token_kind: TokenKind, input: &[Token]) -> IzeResult<&[Token], Token> {
    if !input.is_empty() {
        let pos: Pos = input[0].pos;
        if token_kind == input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(build_err(format!("Token is not {:?}", token_kind).as_str(), pos))
        }
    } else {
        Err(build_err("Input is empty", Default::default()))
    }
}

fn token_ident(input: &[Token]) -> IzeResult<&[Token], Token> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if let TokenKind::Ident(_) = &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(build_err("Token is not an identifier", pos))
        }
    } else {
        Err(build_err("Input is empty", Default::default()))
    }
}

fn token_int(input: &[Token]) -> IzeResult<&[Token], Token> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if let TokenKind::IntegerLiteral(_) = &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(build_err("Token is not an integer", pos))
        }
    } else {
        Err(build_err("Input is empty", Default::default()))
    }
}

fn token_bool(input: &[Token]) -> IzeResult<&[Token], Token> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if let TokenKind::BooleanLiteral(_) = &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(build_err("Token is not a boolean", pos))
        }
    } else {
        Err(build_err("Input is empty", Default::default()))
    }
}

fn token_str(input: &[Token]) -> IzeResult<&[Token], Token> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if let TokenKind::StringLiteral(_) = &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(build_err("Token is not a string", pos))
        }
    } else {
        Err(build_err("Input is empty", Default::default()))
    }
}

fn expr(input: &[Token]) -> IzeResult<&[Token], Expression> {
    expr_chain(input)
}

fn expr_chain(mut input: &[Token]) -> IzeResult<&[Token], Expression> {
    let mut expressions = vec![];
    let rest: &[Token] = loop {
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

fn expr_let(input: &[Token]) -> IzeResult<&[Token], Expression> {
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

fn expr_group(input: &[Token]) -> IzeResult<&[Token], Expression> {
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

fn expr_primary(input: &[Token]) -> IzeResult<&[Token], Expression> {
    if !input.is_empty() {
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
            Err(build_err("Error parsing primary expr", input[0].pos))
        }
    } else {
        Err(build_err("Input is empty", Default::default()))
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, IzeErr> {
    let mut lex = TokenKind::lexer(input);
    let mut tokens = vec![];
    while let Some(r) = lex.next() {
        if let Ok(token_kind) = r {
            let line = lex.extras.line;
            let start_col = lex.span().start - lex.extras.pos_last_eol;
            let end_col = lex.span().end - lex.extras.pos_last_eol;
            let pos = Pos::new(line, start_col, end_col);
            tokens.push(Token::new(pos, token_kind));
        } else {
            //TODO: build Pos for failed lexeme
            return Err(build_err("Bad token", Default::default()));
        }
    }
    Ok(tokens)
}

fn main() {
    let file_path = "izeware/experiment2.iz";
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let code = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");
    let tokens = tokenize(code).expect("Bad token");

    let mut input = tokens.as_slice();

    while !input.is_empty() {
        let (rest, matched) = expr(input).expect("Error parsing expr");
        println!("Expression = {:#?}", matched);
        input = rest;
    }
}
