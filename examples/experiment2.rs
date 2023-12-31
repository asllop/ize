//! # Parse Combinator Experiment
//!
//! Use a parser combinator inspired by `nom`.
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
type IzeResult<'a> = Result<(&'a [Token], AstNode), IzeErr>;

/// Result type alias for parsers with optional result.
type IzeOptResult<'a> = Result<Option<(&'a [Token], AstNode)>, IzeErr>;

/// Convert [`IzeResult`] into [`IzeOptResult`].
fn into_opt_res(value: IzeResult) -> IzeOptResult {
    let res_tuple = value?;
    Ok(Some(res_tuple))
}

#[derive(Debug, Default)]
/// Compiler error.
struct IzeErr {
    /// Error message.
    message: String,
    /// Position where the error was found.
    pos: TokenPos,
}

impl IzeErr {
    /// Build a new error from message and the token position that caused the error.
    fn new(message: String, pos: TokenPos) -> Self {
        IzeErr { message, pos }
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
    fn new_ifelse(
        cond: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
        start_pos: TokenPos,
    ) -> Self {
        let end_pos = else_expr.end_pos;
        Self {
            expr: Expr::IfElse {
                cond_expr: cond.into(),
                if_expr: if_expr.into(),
                else_expr: else_expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_let(ident: Token, expr: Box<Expression>, start_pos: TokenPos) -> Self {
        let end_pos = expr.end_pos;
        Self {
            expr: Expr::Let {
                ident_token: ident.into(),
                expr: expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_binary(op: Token, left_expr: Box<Expression>, right_expr: Box<Expression>) -> Self {
        let start_pos = left_expr.start_pos;
        let end_pos = right_expr.end_pos;
        Self {
            expr: Expr::Binary {
                op_token: op.into(),
                left_expr: left_expr.into(),
                right_expr: right_expr.into(),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_chain(chain: Vec<AstNode>) -> Self {
        let start_pos = chain.first().unwrap().expr_ref().unwrap().start_pos;
        let end_pos = chain.last().unwrap().expr_ref().unwrap().end_pos;
        Self {
            expr: Expr::Chain {
                expr_vec: AstNode::Vec(chain),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_group(expr: Box<Expression>, start_pos: TokenPos, end_pos: TokenPos) -> Self {
        Self {
            expr: Expr::Group {
                expr: AstNode::Expression(expr),
            },
            start_pos,
            end_pos,
        }
    }

    fn new_primary(token: Token) -> Self {
        let start_pos = token.pos;
        let end_pos = token.pos;
        Self {
            expr: Expr::Primary {
                expr: AstNode::Token(token),
            },
            start_pos,
            end_pos,
        }
    }
}

#[derive(Debug)]
enum Expr {
    Primary {
        expr: AstNode,
    },
    Chain {
        expr_vec: AstNode,
    },
    Group {
        expr: AstNode,
    },
    IfElse {
        cond_expr: AstNode,
        if_expr: AstNode,
        else_expr: AstNode,
    },
    Binary {
        op_token: AstNode,
        left_expr: AstNode,
        right_expr: AstNode,
    },
    Let {
        ident_token: AstNode,
        expr: AstNode,
    },
}

/// Parser element.
#[derive(Clone)]
enum Parser<'a> {
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
    fn run(&self, input: &'a [Token]) -> IzeOptResult {
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
fn select<'a>(parsers: &'a [Parser], input: &'a [Token]) -> IzeResult<'a> {
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
fn optional<'a>(parser: &'a Parser<'a>, input: &'a [Token]) -> IzeOptResult<'a> {
    if let Ok(r) = parser.run(input) {
        Ok(r)
    } else {
        Ok(None)
    }
}

/// Execute an array of parsers and return the result in a vector. It fails if any of the parsers fail.
fn concat<'a>(parsers: &'a [Parser], mut input: &'a [Token]) -> IzeResult<'a> {
    let mut results = vec![];
    for parser in parsers {
        if let Some((result, node)) = parser.run(input)? {
            results.push(node);
            input = result;
        }
    }
    Ok((input, results.into()))
}

// Execute a parser zero or more times and return the result in a vector.
fn zero_plus<'a>(parser: &'a Parser<'a>, mut input: &'a [Token]) -> IzeResult<'a> {
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

// Execute a parser one or more times and return the result in a vector.
fn one_plus<'a>(parser: &'a Parser<'a>, mut input: &'a [Token]) -> IzeResult<'a> {
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

#[derive(Debug)]
enum AstNode {
    Token(Token),
    //TODO: Command/Statement variant
    Expression(Box<Expression>),
    Vec(Vec<AstNode>),
}

impl AstNode {
    fn token(self) -> Option<Token> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    fn expr(self) -> Option<Box<Expression>> {
        if let Self::Expression(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

    fn vec(self) -> Option<Vec<AstNode>> {
        if let Self::Vec(vec) = self {
            Some(vec)
        } else {
            None
        }
    }

    fn token_ref(&self) -> Option<&Token> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    fn expr_ref(&self) -> Option<&Box<Expression>> {
        if let Self::Expression(expr) = self {
            Some(expr)
        } else {
            None
        }
    }

    fn vec_ref(&self) -> Option<&Vec<AstNode>> {
        if let Self::Vec(vec) = self {
            Some(vec)
        } else {
            None
        }
    }
}

impl From<Token> for AstNode {
    fn from(value: Token) -> Self {
        Self::Token(value)
    }
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        Self::Expression(Box::new(value))
    }
}

impl From<Box<Expression>> for AstNode {
    fn from(value: Box<Expression>) -> Self {
        Self::Expression(value)
    }
}

impl From<Vec<AstNode>> for AstNode {
    fn from(value: Vec<AstNode>) -> Self {
        Self::Vec(value)
    }
}

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
            Ok((rest, token.into()))
        } else {
            Err(IzeErr::new(err_msg.into(), pos))
        }
    } else {
        Err(IzeErr::new("Input is empty".into(), Default::default()))
    }
}

fn token<'a>(token_kind: &'a TokenKind, input: &'a [Token]) -> IzeResult<'a> {
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

fn token_ident(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::Identifier(_)),
        "Expected token identifier",
        input,
    )
}

fn token_int(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::IntegerLiteral(_)),
        "Expected token integer",
        input,
    )
}

fn token_flt(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::FloatLiteral(_)),
        "Expected token float",
        input,
    )
}

fn token_bool(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::BooleanLiteral(_)),
        "Expected token boolean",
        input,
    )
}

fn token_str(input: &[Token]) -> IzeResult {
    token_match(
        |t| matches!(t, TokenKind::StringLiteral(_)),
        "Expected token string",
        input,
    )
}

fn expr(input: &[Token]) -> IzeResult {
    expr_chain(input)
}

fn expr_chain(mut input: &[Token]) -> IzeResult {
    let mut expressions = vec![];
    //TODO: aquí podem emprar el composer "zero_more"
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
        Ok((rest, Expression::new_chain(expressions).into()))
    }
}

fn expr_let(input: &[Token]) -> IzeResult {
    //TODO: aquí podem emprar el composer "concat"
    if let Ok((rest, let_token)) = token(&TokenKind::Let, input) {
        let (rest, ident_token) = token_ident(rest)?;
        let (rest, expr) = expr_let(rest)?;
        let let_expr = Expression::new_let(
            ident_token.token().unwrap(),
            expr.expr().unwrap(),
            let_token.token().unwrap().pos,
        );
        IzeResult::Ok((rest, let_expr.into()))
    } else {
        expr_ifelse(input)
    }
}

fn expr_ifelse(input: &[Token]) -> IzeResult {
    //TODO: aquí podem emprar el composer "concat"
    if let Ok((rest, if_token)) = token(&TokenKind::If, input) {
        let (rest, _) = token(&TokenKind::OpenParenth, rest)?;
        let (rest, cond_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::ClosingParenth, rest)?;
        let (rest, if_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::Else, rest)?;
        let (rest, else_expr) = expr(rest)?;
        let ifelse_expr = Expression::new_ifelse(
            cond_expr.expr().unwrap(),
            if_expr.expr().unwrap(),
            else_expr.expr().unwrap(),
            if_token.token().unwrap().pos,
        );
        Ok((rest, ifelse_expr.into()))
    } else {
        expr_term(input)
    }
}

fn expr_term(mut input: &[Token]) -> IzeResult {
    let (rest, mut expr) = expr_group(input)?;
    input = rest;
    //TODO: aquí podem emprar el composer "zero_more"
    loop {
        if let Ok((rest, op)) = select(
            &[Parser::Tk(TokenKind::Plus), Parser::Tk(TokenKind::Minus)],
            input,
        ) {
            let (rest, right) = expr_group(rest)?;
            expr = Expression::new_binary(
                op.token().unwrap(),
                expr.expr().unwrap(),
                right.expr().unwrap(),
            )
            .into();
            input = rest;
        } else {
            break;
        }
    }
    Ok((input, expr))
}

fn expr_group(input: &[Token]) -> IzeResult {
    let grammar = concat(
        &[
            Parser::Tk(TokenKind::OpenParenth),
            Parser::Fn(expr),
            Parser::Tk(TokenKind::ClosingParenth),
        ],
        input,
    );
    if let Ok((rest, node_vec)) = grammar {
        // Collector
        let mut node_vec = node_vec.vec().unwrap();
        let end = node_vec.pop().unwrap().token().unwrap().pos; // Token ")"
        let expr = node_vec.pop().unwrap().expr().unwrap();
        let start = node_vec.pop().unwrap().token().unwrap().pos; // Token "("

        let group_expr = Expression::new_group(expr, start, end);
        Ok((rest, group_expr.into()))
    } else {
        // Precedence
        expr_primary(input)
    }
}

fn expr_primary(input: &[Token]) -> IzeResult {
    let grammar = select(
        &[
            Parser::Fn(token_ident),
            Parser::Fn(token_int),
            Parser::Fn(token_flt),
            Parser::Fn(token_str),
            Parser::Fn(token_bool),
            Parser::Tk(TokenKind::NoneLiteral),
            Parser::Tk(TokenKind::NullLiteral),
        ],
        input,
    );
    if let Ok((rest, node)) = grammar {
        // Collector
        let token = node.token().unwrap();
        let primary_expr = Expression::new_primary(token);
        Ok((rest, primary_expr.into()))
    } else {
        // Precedence
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
