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
type IzeResult<'a> = Result<(&'a [Token], AstNode), IzeErr>;

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
        let start_pos = chain.first().unwrap().unwrap_expr_ref().start_pos;
        let end_pos = chain.last().unwrap().unwrap_expr_ref().end_pos;
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

//TODO: build combinators: select, either, optional, zero_more, one_more, concat, etc.

#[derive(Clone)]
enum Parser<'a> {
    Fn(fn(&[Token]) -> IzeResult),
    Tk(TokenKind),
    Sel(&'a [Parser<'a>]),
    Opt(&'a Parser<'a>),
}

impl<'a> Parser<'a> {
    fn run(&self, input: &'a [Token]) -> IzeResult {
        match self {
            Parser::Fn(parser_fn) => parser_fn(input),
            Parser::Tk(token_kind) => token(token_kind, input),
            Parser::Sel(parsers) => select(parsers, input),
            Parser::Opt(parser) => optional(parser, input),
        }
    }
}

/// Select a parser from a list, the first that sucseeds.
fn select<'a>(parsers: &'a [Parser], input: &'a [Token]) -> IzeResult<'a> {
    for parser in parsers {
        if let Ok(r) = parser.run(input) {
            return Ok(r);
        }
    }
    // None of the parsers succeed, return an error
    let pos = if let Some(t) = input.first() {
        t.pos
    } else {
        Default::default()
    };
    Err(IzeErr::new(
        "None of the parsers passed to 'one_of' succeed".into(),
        pos,
    ))
}

fn optional<'a>(parser: &'a Parser<'a>, input: &'a [Token]) -> IzeResult<'a> {
    if let Ok(r) = parser.run(input) {
        Ok(r)
    } else {
        // TODO: Com fem per no retornar res quan el parser retorna Err?
        Err(IzeErr::default())
    }
}

#[derive(Debug)]
enum AstNode {
    Token(Token),
    //TODO: Statement variant
    Expression(Box<Expression>),
    Vec(Vec<AstNode>),
}

impl AstNode {
    fn unwrap_token(self) -> Token {
        if let Self::Token(token) = self {
            token
        } else {
            panic!("AstNode is not a token")
        }
    }

    fn unwrap_expr(self) -> Box<Expression> {
        if let Self::Expression(expr) = self {
            expr
        } else {
            panic!("AstNode is not an expression")
        }
    }

    fn unwrap_vec(self) -> Vec<AstNode> {
        if let Self::Vec(vec) = self {
            vec
        } else {
            panic!("AstNode is not a vector")
        }
    }

    fn unwrap_token_ref(&self) -> &Token {
        if let Self::Token(token) = self {
            token
        } else {
            panic!("AstNode ref is not a token")
        }
    }

    fn unwrap_expr_ref(&self) -> &Box<Expression> {
        if let Self::Expression(expr) = self {
            expr
        } else {
            panic!("AstNode ref is not an expression")
        }
    }

    fn unwrap_vec_ref(&self) -> &Vec<AstNode> {
        if let Self::Vec(vec) = self {
            vec
        } else {
            panic!("AstNode ref is not a vector")
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
    if let Ok((rest, let_token)) = token(&TokenKind::Let, input) {
        let (rest, ident_token) = token_ident(rest)?;
        let (rest, expr) = expr_let(rest)?;
        let let_expr = Expression::new_let(
            ident_token.unwrap_token(),
            expr.unwrap_expr(),
            let_token.unwrap_token().pos,
        );
        IzeResult::Ok((rest, let_expr.into()))
    } else {
        expr_ifelse(input)
    }
}

fn expr_ifelse(input: &[Token]) -> IzeResult {
    if let Ok((rest, if_token)) = token(&TokenKind::If, input) {
        let (rest, _) = token(&TokenKind::OpenParenth, rest)?;
        let (rest, cond_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::ClosingParenth, rest)?;
        let (rest, if_expr) = expr(rest)?;
        let (rest, _) = token(&TokenKind::Else, rest)?;
        let (rest, else_expr) = expr(rest)?;
        let ifelse_expr = Expression::new_ifelse(
            cond_expr.unwrap_expr(),
            if_expr.unwrap_expr(),
            else_expr.unwrap_expr(),
            if_token.unwrap_token().pos,
        );
        Ok((rest, ifelse_expr.into()))
    } else {
        expr_term(input)
    }
}

fn expr_term(mut input: &[Token]) -> IzeResult {
    let (rest, mut expr) = expr_group(input)?;
    input = rest;
    loop {
        if let Ok((rest, op)) = select(
            &[Parser::Tk(TokenKind::Plus), Parser::Tk(TokenKind::Minus)],
            input,
        ) {
            let (rest, right) = expr_group(rest)?;
            expr =
                Expression::new_binary(op.unwrap_token(), expr.unwrap_expr(), right.unwrap_expr())
                    .into();
            input = rest;
        } else {
            break;
        }
    }
    Ok((input, expr))
}

fn expr_group(input: &[Token]) -> IzeResult {
    if let Ok((rest, open_parenth_token)) = token(&TokenKind::OpenParenth, input) {
        let (rest, expr) = expr(rest)?;
        let (rest, close_parenth_token) = token(&TokenKind::ClosingParenth, rest)?;
        let start = open_parenth_token.unwrap_token().pos;
        let end = close_parenth_token.unwrap_token().pos;
        let group_expr = Expression::new_group(expr.unwrap_expr(), start, end);
        IzeResult::Ok((rest, group_expr.into()))
    } else {
        expr_primary(input)
    }
}

fn expr_primary(input: &[Token]) -> IzeResult {
    if let Ok((rest, token)) = select(
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
    ) {
        Result::Ok((rest, Expression::new_primary(token.unwrap_token()).into()))
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
