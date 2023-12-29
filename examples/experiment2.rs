//! # Parse Composer Experiment
//!
//! Use a parser composed inspired by `nom`.
//!

use logos::{Lexer, Logos, Skip};
use std::{
    fmt::Debug,
    fs::File,
    io::{prelude::*, BufReader},
    str::FromStr,
    str::{self},
};

/// Result type alias for parsers.
type IzeResult<'a, O> = Result<(&'a [Token], O), IzeErr>;

#[derive(Debug, Default)]
/// Compiler error.
struct IzeErr {
    /// Error message.
    message: String,
    /// Position where the error was found.
    pos: TokenPos,
}

impl IzeErr {
    fn new(message: String, pos: TokenPos) -> Self {
        IzeErr {
            message, pos,
        }
    }
}

fn parse_callback<T>(lex: &mut Lexer<TokenKind>) -> T
where
    T: FromStr + Debug,
{
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
    Identifier(String),

    // Types
    // TODO: do we really need this? We could just have Ident and check for types during the semantic analysis.
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

#[derive(Default, Debug, Clone, Copy)]
/// Position of a token in the code.
struct TokenPos {
    /// Line.
    line: usize,
    /// Starting column.
    start_col: usize,
    /// Ending column.
    end_col: usize,
    /// Absolute position from start of file.
    seek: usize,
}

impl TokenPos {
    fn new(line: usize, start_col: usize, end_col: usize, seek: usize) -> Self {
        Self {
            line,
            start_col,
            end_col,
            seek,
        }
    }
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    pos: TokenPos,
}

impl Token {
    fn new(pos: TokenPos, kind: TokenKind) -> Self {
        Self { kind, pos }
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
struct Expression {
    expr: Expr,
    start_pos: TokenPos,
    end_pos: TokenPos,
}

impl Expression {
    fn new_ifelse(cond: Expression, if_expr: Expression, else_expr: Expression, start_pos: TokenPos) -> Self {
        let end_pos = else_expr.end_pos;
        Self {
            expr: Expr::IfElse { cond_expr: Box::new(cond), if_expr: Box::new(if_expr), else_expr: Box::new(else_expr) },
            start_pos,
            end_pos,
        }
    }

    fn new_let(ident: Token, expr: Expression, start_pos: TokenPos) -> Self {
        let end_pos = expr.end_pos;
        Self {
            expr: Expr::Let {
                ident,
                expr: Box::new(expr),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_binary(op: Token, left_expr: Expression, right_expr: Expression) -> Self {
        let start_pos = left_expr.start_pos;
        let end_pos = right_expr.end_pos;
        Self {
            expr: Expr::Binary {
                op,
                left_expr: Box::new(left_expr),
                right_expr: Box::new(right_expr),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_chain(chain: Vec<Expression>) -> Self {
        let start_pos = chain.first().unwrap().start_pos;
        let end_pos = chain.last().unwrap().end_pos;
        Self {
            expr: Expr::Chain(chain),
            start_pos,
            end_pos,
        }
    }

    fn new_group(expr: Expression, start_pos: TokenPos, end_pos: TokenPos) -> Self {
        Self {
            expr: Expr::Group(Box::new(expr)),
            start_pos,
            end_pos,
        }
    }

    fn new_primary(token: Token) -> Self {
        let start_pos = token.pos;
        let end_pos = token.pos;
        Self {
            expr: Expr::Primary(token),
            start_pos,
            end_pos,
        }
    }
}

#[derive(Debug)]
enum Expr {
    Primary(Token),
    Chain(Vec<Expression>),
    Group(Box<Expression>),
    IfElse {
        cond_expr: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    Binary {
        op: Token,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
    },
    Let {
        ident: Token,
        expr: Box<Expression>,
    },
}

enum AnyToken {
    Ident,
    Str,
    Int,
    Flt,
    Bool,
    Any(TokenKind),
}

//TODO: build combinators: one_of, either, optional, zero_or_more, once_at_least, concat, etc.
//  - Create a generic type AstNode that can be either a Token or a Expression. This way combinators can be generic.
//  - Expr will contain AstNode instead of Expression and Token.

// enum AstNode {
//     Token(Token),
//     Expression(Expression),
//     //Statement(Statement),
//     Array(Vec<AstNode>),
// }

fn token_match<'a>(
    matches: fn(&TokenKind) -> bool,
    err_msg: &'a str,
    input: &'a [Token],
) -> IzeResult<'a, Token> {
    if !input.is_empty() {
        let pos = input[0].pos;
        if matches(&input[0].kind) {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(IzeErr::new(err_msg.into(), pos))
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()))
    }
}

fn token<'a>(token_kind: &'a TokenKind, input: &'a [Token]) -> IzeResult<'a, Token> {
    if !input.is_empty() {
        let pos: TokenPos = input[0].pos;
        if token_kind == &input[0].kind {
            let token = Token::new(pos, input[0].kind.clone());
            let rest = &input[1..];
            Ok((rest, token))
        } else {
            Err(IzeErr::new(
                format!("Expected token {:?}", token_kind),
                pos,
            ))
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()))
    }
}

fn token_ident(input: &[Token]) -> IzeResult<Token> {
    token_match(
        |t| matches!(t, TokenKind::Identifier(_)),
        "Expected token identifier",
        input,
    )
}

fn token_int(input: &[Token]) -> IzeResult<Token> {
    token_match(
        |t| matches!(t, TokenKind::IntegerLiteral(_)),
        "Expected token integer",
        input,
    )
}

fn token_flt(input: &[Token]) -> IzeResult<Token> {
    token_match(
        |t| matches!(t, TokenKind::FloatLiteral(_)),
        "Expected token float",
        input,
    )
}

fn token_bool(input: &[Token]) -> IzeResult<Token> {
    token_match(
        |t| matches!(t, TokenKind::BooleanLiteral(_)),
        "Expected token boolean",
        input,
    )
}

fn token_str(input: &[Token]) -> IzeResult<Token> {
    token_match(
        |t| matches!(t, TokenKind::StringLiteral(_)),
        "Expected token string",
        input,
    )
}

fn one_of_kinds<'a>(tokens: &'a [TokenKind], input: &'a [Token]) -> IzeResult<'a, Token> {
    let mut err = Default::default();
    for t in tokens {
        match token(t, input) {
            Ok(r) => return Ok(r),
            Err(e) => err = e,
        }
    }
    Err(err)
}

fn one_of<'a>(list: &'a [AnyToken], input: &'a [Token]) -> IzeResult<'a, Token> {
    let mut err = IzeErr {
        message: "None of the tokens matched".into(),
        pos: Default::default(),
    };
    for any_token in list {
        match match any_token {
            AnyToken::Ident => token_ident(input),
            AnyToken::Str => token_str(input),
            AnyToken::Int => token_int(input),
            AnyToken::Flt => token_flt(input),
            AnyToken::Bool => token_bool(input),
            AnyToken::Any(t) => token(t, input),
        } {
            Ok(r) => return Ok(r),
            Err(e) => err = e,
        }
    }
    Err(err)
}

fn expr(input: &[Token]) -> IzeResult<Expression> {
    expr_chain(input)
}

fn expr_chain(mut input: &[Token]) -> IzeResult<Expression> {
    let mut expressions = vec![];
    let rest: &[Token] = loop {
        let (rest, expr) = expr_let(input)?;
        expressions.push(expr);
        if let Ok((rest, _)) = token(&TokenKind::Semicolon, rest) {
            input = rest;
        } else {
            break rest;
        }
    };
    // If we are here, `expressions` Vec contains at least one element.
    if expressions.len() == 1 {
        Ok((rest, expressions.pop().unwrap()))
    } else {
        Ok((rest, Expression::new_chain(expressions)))
    }
}

fn expr_let(input: &[Token]) -> IzeResult<Expression> {
    if let Ok((rest, let_token)) = token(&TokenKind::Let, input) {
        let (rest, ident_token) = token_ident(rest)?;
        let (rest, expr) = expr_let(rest)?;
        let let_expr = Expression::new_let(ident_token, expr, let_token.pos);
        IzeResult::Ok((rest, let_expr))
    } else {
        expr_ifelse(input)
    }
}

fn expr_ifelse(input: &[Token]) -> IzeResult<Expression> {
    if let Ok((rest, if_token)) = token(&TokenKind::If, input) {
        let (rest, _) = token(&TokenKind::OpenParenth, rest)?;
        let (rest, cond_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::ClosingParenth, rest)?;
        let (rest, if_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::Else, rest)?;
        let (rest, else_expr) = expr(rest)?;
        let ifelse_expr = Expression::new_ifelse(cond_expr, if_expr, else_expr, if_token.pos);
        Ok((rest, ifelse_expr))
    } else {
        expr_term(input)
    }
}

fn expr_term(mut input: &[Token]) -> IzeResult<Expression> {
    let (rest, mut expr) = expr_group(input)?;
    input = rest;
    loop {
        if let Ok((rest, op)) = one_of_kinds(&[TokenKind::Plus, TokenKind::Minus], input) {
            let (rest, right) = expr_group(rest)?;
            expr = Expression::new_binary(op, expr, right);
            input = rest;
        } else {
            break;
        }
    }
    Ok((input, expr))
}


fn expr_group(input: &[Token]) -> IzeResult<Expression> {
    if let Ok((rest, open_parenth_token)) = token(&TokenKind::OpenParenth, input) {
        let (rest, expr) = expr(rest)?;
        let (rest, close_parenth_token) = token(&TokenKind::ClosingParenth, rest)?;
        let start = open_parenth_token.pos;
        let end = close_parenth_token.pos;
        let group_expr = Expression::new_group(expr, start, end);
        IzeResult::Ok((rest, group_expr))
    } else {
        expr_primary(input)
    }
}

fn expr_primary(input: &[Token]) -> IzeResult<Expression> {
    if let Ok((rest, token)) = one_of(
        &[
            AnyToken::Ident,
            AnyToken::Int,
            AnyToken::Flt,
            AnyToken::Str,
            AnyToken::Bool,
            AnyToken::Any(TokenKind::NoneLiteral),
            AnyToken::Any(TokenKind::NullLiteral),
        ],
        input,
    ) {
        Result::Ok((rest, Expression::new_primary(token)))
    } else {
        let pos = if input.len() > 0 {
            input[0].pos
        } else {
            Default::default()
        };
        Err(IzeErr::new("Error parsing primary expr".into(), pos))
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, IzeErr> {
    let mut lex = TokenKind::lexer(input);
    let mut tokens = vec![];
    while let Some(r) = lex.next() {
        let line = lex.extras.line;
        let start_col = lex.span().start - lex.extras.pos_last_eol;
        let end_col = lex.span().end - lex.extras.pos_last_eol;
        let pos = TokenPos::new(line, start_col, end_col, lex.span().start);
        if let Ok(token_kind) = r {
            tokens.push(Token::new(pos, token_kind));
        } else {
            return Err(IzeErr::new("Bad token".into(), pos));
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
        println!("-------------------------\n{:#?}", matched);
        input = rest;
    }
}
