use crate::{
    ast::{Expr, ExprSet, Literal},
    parser::common::FromToken,
    lexer::{
        TokenKind::{self, *},
        TokenStream,
    },
    BuildErr, IzeErr, Pos,
};
use alloc::{string::String, vec::Vec};
use Elem::*;

type GrammarFn = fn() -> Grammar;

/// Grammar elements.
#[derive(Debug)]
pub enum Elem {
    /// One token.
    Tk(TokenKind),
    /// Multiple possible tokens.
    OrTk(&'static [TokenKind]),
    /// Identifier.
    Identifier,
    /// Literal.
    Literal,
    /// A grammar sequence present zero or more times.
    Mul(&'static [Elem]),
    /// A grammar sequence present one or more times.
    Plu(&'static [Elem]),
    /// A grammar sequence present zero or one time.
    Opt(&'static [Elem]),
    /// Select one out of multiple possible grammar sequences. The first that matches.
    Sel(&'static [&'static [Elem]]),
    /// Expression defined by function.
    Expr(GrammarFn),
    /// This element makes the parsing of current Grammar unavoidable. Once found we can't fallaback to the next expression.
    Must,
}

pub struct Grammar {
    id: &'static str,
    grammar: &'static [Elem],
    next: Option<GrammarFn>,
    collector: fn(&mut ParseResult, &mut TokenStream) -> Result<Expr, IzeErr>,
}

impl Grammar {
    /// Create new grammar to parse an expression.
    /// - grammar: Array of grammar elements.
    /// - next: Next expression to parse in precedence order in case 'grammar' doesn't match.
    /// - collector: Collector closure, to collect grammar elements and build expression.
    pub fn new(
        id: &'static str,
        grammar: &'static [Elem],
        next: Option<GrammarFn>,
        collector: fn(&mut ParseResult, &mut TokenStream) -> Result<Expr, IzeErr>,
    ) -> Self {
        Self {
            id,
            grammar,
            next,
            collector,
        }
    }

    /// Check if a grammar can be parsed given the available tokens.
    pub fn check(&self, token_stream: &TokenStream, index: usize) -> Result<(bool, usize), IzeErr> {
        let (result, new_index) = self.check_grammar(token_stream, self.grammar, index)?;
        if result {
            Ok((result, new_index))
        } else {
            // Try next in precedence
            match self.next {
                Some(next) => self.check_grammar(token_stream, &[Expr(next)], index),
                None => Ok((false, index)),
            }
        }
    }

    /// Check a gramamar on a token stream starting at index.
    /// Returns (result, new_index):
    /// - result: Indicates whether the check was sucessful or not.
    /// - new_index: Is the current index on the token stream after the check, only if result == true.
    fn check_grammar(
        &self,
        token_stream: &TokenStream,
        grammar: &[Elem],
        index: usize,
    ) -> Result<(bool, usize), IzeErr> {
        let mut must = false;
        let mut index = index;
        for elem in grammar {
            match elem {
                Elem::Tk(token_kind) => {
                    if token_stream.is_token(*token_kind, index) {
                        index += 1;
                    } else {
                        return if must {
                            Result::ize_err(
                                format!(
                                    "Expression {} error, expected token {:?}",
                                    self.id, token_kind
                                ),
                                token_stream.pos_at(index),
                            )
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::OrTk(tokens) => {
                    if token_stream.is_any_of_tokens(*tokens, index) {
                        index += 1;
                    } else {
                        return if must {
                            Result::ize_err(
                                format!("Expression {} error, one of token {:?}", self.id, tokens),
                                token_stream.pos_at(index),
                            )
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::Identifier => {
                    if token_stream.is_ident(index) {
                        index += 1;
                    } else {
                        return if must {
                            Result::ize_err(
                                format!("Expression {} error, expected identifier", self.id),
                                token_stream.pos_at(index),
                            )
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::Literal => {
                    if token_stream.is_literal(index) {
                        index += 1;
                    } else {
                        return if must {
                            Result::ize_err(
                                format!("Expression {} error, expected literal", self.id),
                                token_stream.pos_at(index),
                            )
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::Mul(grammar) => loop {
                    let (check_result, new_index) =
                        self.check_grammar(token_stream, grammar, index)?;
                    if !check_result {
                        break;
                    }
                    index = new_index;
                },
                Elem::Plu(grammar) => {
                    let mut i = 0;
                    loop {
                        let (check_result, new_index) =
                            self.check_grammar(token_stream, grammar, index)?;
                        if !check_result {
                            break;
                        }
                        index = new_index;
                        i += 1;
                    }
                    if i == 0 {
                        return if must {
                            Result::ize_err(
                                format!(
                                    "Expression {} error, expected sequence: {:?}",
                                    self.id, grammar
                                ),
                                token_stream.pos_at(index),
                            )
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::Opt(grammar) => {
                    let (check_result, new_index) =
                        self.check_grammar(token_stream, grammar, index)?;
                    if check_result {
                        index = new_index;
                    }
                }
                Elem::Sel(grammars) => {
                    let mut grammar_result = false;
                    for &grammar in grammars.iter() {
                        let (check_result, new_index) =
                            self.check_grammar(token_stream, grammar, index)?;
                        if check_result {
                            index = new_index;
                            grammar_result = true;
                            break;
                        }
                    }
                    // None of the grammars matched
                    if !grammar_result {
                        return if must {
                            Result::ize_err(format!("Expression {} error, expected one of the following sequences:\n{:?}", self.id, grammars), token_stream.pos_at(index))
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::Expr(expr) => {
                    let grammar = expr();
                    let (check_result, new_index) = grammar.check(token_stream, index)?;
                    if check_result {
                        index = new_index;
                    } else {
                        return if must {
                            Result::ize_err(
                                format!("Expression {} error, expected expression", self.id),
                                token_stream.pos_at(index),
                            )
                        } else {
                            Ok((false, index))
                        };
                    }
                }
                Elem::Must => {
                    // Mark current grammar as unskipable: after this we must succeed parsing or generate an error and abort.
                    must = true;
                }
            }
        }
        Ok((true, index))
    }

    /// Parse using grammar rule set.
    pub fn parse(&mut self, token_stream: &mut TokenStream) -> Result<(), IzeErr> {
        let mut parse_result = ParseResult::default();
        if self.exec_grammar(self.grammar, token_stream, &mut parse_result) {
            Ok(())
        } else {
            Ok(())
            //Result::ize_err("Exec returned false".into(), token_stream.last_pos())
        }
    }

    //TODO: should this return a bool?? We probably need an IzeErr too.
    /// It can panic.
    fn exec_grammar(&mut self, grammar: &[Elem], token_stream: &mut TokenStream, result: &mut ParseResult) -> bool {
        println!("EXEC GRAMMAR = {:?}", grammar);

        for elem in grammar {
            match elem {
                Elem::Tk(token_kind) => {
                    if token_stream.is_token(*token_kind, 0) {
                        println!("TK {:?}", token_kind);
                        let (_, pos) = token_stream
                            .consume_token()
                            .into_particle()
                            .expect("The stream should contain a particle");
                        result.add_token(pos, *token_kind);
                    } else {
                        println!("Not TK {:?}", token_kind);
                        return false;
                    }
                }
                Elem::OrTk(tokens) => {
                    if token_stream.is_any_of_tokens(*tokens, 0) {
                        let (token_kind, pos) = token_stream
                            .consume_token()
                            .into_particle()
                            .expect("The stream should contain a particle");
                        println!("ORTK {:?}", token_kind);
                        result.add_token(pos, token_kind);
                    } else {
                        println!("Not ORTK {:?}", tokens);
                        return false;
                    }
                }
                Elem::Identifier => {
                    if token_stream.is_ident(0) {
                        let (id, pos) = token_stream
                            .consume_token()
                            .into_ident()
                            .expect("The stream should contain an identifier");
                        println!("ID {:?}", id);
                        result.add_ident(pos, id);
                    } else {
                        println!("Not ID");
                        return false;
                    }
                }
                Elem::Literal => {
                    if token_stream.is_literal(0) {
                        let (lit, pos) = token_stream
                            .consume_token()
                            .into_literal()
                            .expect("The stream should contain a literal");
                        println!("LIT {:?}", lit);
                        match lit {
                            Literal::Integer(i) => {
                                result.add_lit_int(pos, i);
                            }
                            Literal::Float(f) => {
                                result.add_lit_flt(pos, f);
                            }
                            Literal::String(s) => {
                                result.add_lit_str(pos, s);
                            }
                            Literal::Boolean(b) => {
                                result.add_lit_bool(pos, b);
                            }
                            Literal::None => {
                                result.add_lit_none(pos);
                            }
                            Literal::Null => {
                                result.add_lit_null(pos);
                            }
                        }
                    } else {
                        println!("Not LIT");
                        return false;
                    }
                }
                //TODO: Next step implement this
                Elem::Mul(grammar) => {
                    println!("MUL {:?}", grammar);
                    /*
                    - Call check_grammar
                    - If it fails, do nothing and return ok
                    - If it works, parse grammar (call exec_grammar) and try again from the begining
                     */
                    loop {
                        match self.check_grammar(token_stream, grammar, 0) {
                            Ok((false, _)) | Err(_) => break,
                            Ok((true, _)) => {
                                self.exec_grammar(grammar, token_stream, result);
                            },
                        }
                    }
                },
                Elem::Plu(grammar) => {
                    println!("PLU {:?}", grammar);
                    /*
                    - Call check_grammar
                    - If it fails, if it's the first time, return error. If not first time, do nothing and return ok.
                    - If it works, parse grammar (call exec_grammar) and try again from the begining
                     */
                    let mut counter = 0;
                    loop {
                        match self.check_grammar(token_stream, grammar, 0) {
                            Ok((false, _)) | Err(_) => if counter == 0 {
                                println!("Plu Not found");
                                return false
                            } else {
                                break
                            },
                            Ok((true, _)) => {
                                println!("Plu found");
                                self.exec_grammar(grammar, token_stream, result);
                            },
                        }
                        counter += 1;
                    }
                },
                Elem::Opt(grammar) => {
                    println!("OPT {:?}", grammar);
                    /*
                    - Call check_grammar
                    - If it fails, do nothing and return ok.
                    - If it works, parse grammar (call exec_grammar) and return ok
                     */
                    match self.check_grammar(token_stream, grammar, 0) {
                        Ok((false, _)) | Err(_) => {},
                        Ok((true, _)) => {
                            println!("Opt found");
                            self.exec_grammar(grammar, token_stream, result);
                        },
                    }
                },
                Elem::Sel(grammars) => {
                    println!("SEL {:?}", grammars);
                    /*
                    - Iterate over all grammars.
                    - Call check_grammar
                    - If it works, parse grammar and return ok
                    - If iteration ends without match, return error
                     */
                    let mut did_match_grammar = false;
                    for &grammar in grammars.iter() {
                        match self.check_grammar(token_stream, grammar, 0) {
                            Ok((false, _)) | Err(_) => {},
                            Ok((true, _)) => {
                                println!("SEL found {:?}", grammar);
                                did_match_grammar = true;
                                self.exec_grammar(grammar, token_stream, result);
                            },
                        }
                    }
                    if !did_match_grammar {
                        println!("Not SEL");
                        return false;
                    }
                },
                Elem::Expr(expr_fn) => {
                    let res = self.exec_expr(expr_fn, token_stream, result);
                    if res {
                        println!("Expr executed");
                    } else {
                        println!("Expr Not executed");
                        return  false;
                    }
                },
                Elem::Must => {
                    println!("MUST");
                }
            }
            print!("TOKEN STREAM = ");
            print_token_range(token_stream, 0, token_stream.len());
        }
        true
    }

    fn exec_expr(&mut self, expr_fn: &GrammarFn, token_stream: &mut TokenStream, result: &mut ParseResult) -> bool {
        println!("EXPR {:?}", expr_fn);
        let mut grammar = expr_fn();
        if grammar.exec_grammar(grammar.grammar, token_stream, result) {
            if let Ok(expr) = (grammar.collector)(result, token_stream) {
                result.add_expr(expr.pos, expr);
            }
            true
        } else {
            if let Some(next) = grammar.next {
                self.exec_expr(&next, token_stream, result)
            } else {
                println!("Expr grammar didn't parse");
                false
            }
        }
    }
}

#[derive(Default, Debug)]
/// Parsing results object.
pub struct ParseResult {
    /// Actual parsing results.
    pub atoms: Vec<Atom>,
}

impl ParseResult {
    pub fn add_token(&mut self, pos: Pos, token_kind: TokenKind) {
        self.atoms.push(Atom::Token { pos, val: token_kind })
    }

    pub fn add_ident(&mut self, pos: Pos, ident: String) {
        self.atoms.push(Atom::Ident { pos, val: ident })
    }

    pub fn add_lit_int(&mut self, pos: Pos, literal: i64) {
        self.atoms.push(Atom::IntLiteral { pos, val: literal })
    }

    pub fn add_lit_flt(&mut self, pos: Pos, literal: f64) {
        self.atoms.push(Atom::FltLiteral { pos, val: literal })
    }

    pub fn add_lit_str(&mut self, pos: Pos, literal: String) {
        self.atoms.push(Atom::StrLiteral { pos, val: literal })
    }

    pub fn add_lit_bool(&mut self, pos: Pos, literal: bool) {
        self.atoms.push(Atom::BoolLiteral { pos, val: literal })
    }

    pub fn add_expr(&mut self, pos: Pos, expr: Expr) {
        self.atoms.push(Atom::Expr { pos, val: expr })
    }

    pub fn add_lit_none(&mut self, pos: Pos) {
        self.atoms.push(Atom::NoneLiteral { pos })
    }

    pub fn add_lit_null(&mut self, pos: Pos) {
        self.atoms.push(Atom::NullLiteral { pos })
    }
}

//TODO: convert Atom into a struct that contains Pos and the actual atom that is an enum.
/// Parsing results component.
#[derive(Debug)]
pub enum Atom {
    Ident { pos: Pos, val: String },
    Token { pos: Pos, val: TokenKind },
    StrLiteral { pos: Pos, val: String },
    IntLiteral { pos: Pos, val: i64 },
    FltLiteral { pos: Pos, val: f64 },
    BoolLiteral { pos: Pos, val: bool },
    NoneLiteral { pos: Pos },
    NullLiteral { pos: Pos },
    Expr { pos: Pos, val: Expr },
}

impl Atom {
    pub fn as_ident(self) -> Result<String, IzeErr> {
        if let Atom::Ident { val, .. } = self {
            Ok(val)
        } else {
            Result::ize_err("Expected an identifier Atom".into(), self.pos())
        }
    }

    pub fn as_expr(self) -> Result<Expr, IzeErr> {
        if let Atom::Expr { val, .. } = self {
            Ok(val)
        } else {
            Result::ize_err("Expected an expression Atom".into(), self.pos())
        }
    }

    pub fn as_token(self) -> Result<TokenKind, IzeErr> {
        if let Atom::Token { val, .. } = self {
            Ok(val)
        } else {
            Result::ize_err("Expected an expression Atom".into(), self.pos())
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            Atom::Ident { pos, .. }
            | Atom::Token { pos, .. }
            | Atom::StrLiteral { pos, .. }
            | Atom::IntLiteral { pos, .. }
            | Atom::FltLiteral { pos, .. }
            | Atom::BoolLiteral { pos, .. }
            | Atom::NoneLiteral { pos }
            | Atom::NullLiteral { pos }
            | Atom::Expr { pos, .. } => *pos,
        }
    }
}

///////// TEST expression parsing \\\\\\\\\

pub fn parse_expression(token_stream: &mut TokenStream) -> Result<(), IzeErr> {
    expr().parse(token_stream)
}

pub fn check_expression(
    token_stream: &mut TokenStream,
    index: usize,
) -> Result<(bool, usize), IzeErr> {
    let (result, new_index) = expr().check(token_stream, index)?;

    if result {
        print!("\n---> CHECKED EXPR: ");
        print_token_range(token_stream, index, new_index);
        println!("Check Result = {} , Index = {}", result, index);
        Ok((result, new_index))
    } else {
        let lexeme = &token_stream.at(index).unwrap().lexeme;
        println!("\n---> CHECK FAILED, next token: {:?}\n", lexeme);
        // TODO:
        // We have to identify specific error cases to generate better error messages.
        // For example "a+b)" will correctly parse "a+b" and then will fail on ")" because there is no expression that starts with it.
        // In this case we should say "Maybe you forgot to open the parenthesis?".
        Result::ize_err(
            format!("Couldn't parse a valid expression with {:?}", lexeme),
            token_stream.pos_at(index),
        )
    }
}

fn print_token_range(token_stream: &TokenStream, mut start: usize, end: usize) {
    while start < end {
        let lexeme = &token_stream.at(start).unwrap().lexeme;
        print!("{:?} ", lexeme);
        start += 1;
    }
    println!("\n");
}

fn expr() -> Grammar {
    chain_expr()
}

// next_expr (";" next_expr)+
fn chain_expr() -> Grammar {
    Grammar::new(
        "CHAIN",
        &[Expr(let_expr), Plu(&[Tk(Semicolon), Must, Expr(let_expr)])],
        Some(let_expr),
        |mut result, token_stream| {
            println!("CHAIN collector {:?}", result);
            todo!("Chain collect results")
        },
    )
}

// "let" ID let_expr
fn let_expr() -> Grammar {
    Grammar::new(
        "LET",
        &[Tk(Let), Must, Identifier, Expr(let_expr)],
        Some(primary_expr),
        //Some(ifelse_expr),
        |mut result, token_stream| {
            println!("LET collector {:?}", result);
            todo!("Let collect results")
        },
    )
}

// "if" "(" expr ")" expr "else" expr
fn ifelse_expr() -> Grammar {
    Grammar::new(
        "IF-ELSE",
        &[
            Tk(If),
            Must,
            Tk(OpenParenth),
            Expr(expr),
            Tk(ClosingParenth),
            Expr(expr),
            Tk(Else),
            Expr(expr),
        ],
        Some(equality_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "!=" | "==" ) next_expr )+
fn equality_expr() -> Grammar {
    Grammar::new(
        "EQUALITY-BINARY",
        &[
            Expr(comparison_expr),
            Plu(&[
                OrTk(&[TokenKind::NotEqual, TokenKind::TwoEquals]),
                Must,
                Expr(comparison_expr),
            ]),
        ],
        Some(comparison_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( ">" | "<" | "<=" | ">=" | "&&" | "||" ) next_expr )+
fn comparison_expr() -> Grammar {
    Grammar::new(
        "COMPARISON-BINARY",
        &[
            Expr(logic_expr),
            Plu(&[
                OrTk(&[
                    TokenKind::GreaterThan,
                    TokenKind::LesserThan,
                    TokenKind::GtEqual,
                    TokenKind::LtEqual,
                    TokenKind::TwoAnds,
                    TokenKind::TwoOrs,
                ]),
                Must,
                Expr(logic_expr),
            ]),
        ],
        Some(logic_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "&" | "|" ) next_expr )+
fn logic_expr() -> Grammar {
    Grammar::new(
        "LOGIC-BINARY",
        &[
            Expr(term_expr),
            Plu(&[
                OrTk(&[TokenKind::And, TokenKind::Or]),
                Must,
                Expr(term_expr),
            ]),
        ],
        Some(term_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "+" | "-" ) next_expr )+
fn term_expr() -> Grammar {
    Grammar::new(
        "TERM-BINARY",
        &[
            Expr(factor_expr),
            Plu(&[
                OrTk(&[TokenKind::Plus, TokenKind::Minus]),
                Must,
                Expr(factor_expr),
            ]),
        ],
        Some(factor_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "*" | "/" | "%" ) next_expr )+
fn factor_expr() -> Grammar {
    Grammar::new(
        "FACTOR-BINARY",
        &[
            Expr(unary_expr),
            Plu(&[
                OrTk(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]),
                Must,
                Expr(unary_expr),
            ]),
        ],
        Some(unary_expr),
        |mut result, token_stream| todo!(),
    )
}

// ( "!" | "-" )+ next_expr
fn unary_expr() -> Grammar {
    Grammar::new(
        "UNARY",
        &[
            Plu(&[OrTk(&[TokenKind::Minus, TokenKind::Not])]),
            Must,
            Expr(call_expr),
        ],
        Some(call_expr),
        |mut result, token_stream| todo!(),
    )
}

// ID "(" (expr ("," expr)*)? ")"
fn call_expr() -> Grammar {
    Grammar::new(
        "CALL",
        &[
            Identifier,
            Tk(OpenParenth),
            Must,
            Opt(&[Expr(expr), Mul(&[Tk(Comma), Must, Expr(expr)])]),
            Tk(ClosingParenth),
        ],
        Some(group_expr),
        |mut result, token_stream| todo!(),
    )
}

// "(" expr ")"
fn group_expr() -> Grammar {
    Grammar::new(
        "GROUP",
        &[Tk(OpenParenth), Must, Expr(expr), Tk(ClosingParenth)],
        Some(primary_expr),
        |mut result, token_stream| todo!(),
    )
}

// LITERAL | ID | type_expr
fn primary_expr() -> Grammar {
    Grammar::new(
        "PRIMARY",
        &[Sel(&[
            &[Literal],
            &[Identifier],
            //TODO: type expression
        ])],
        None,
        |result, token_stream| {
            // Build primary expression
            let primary_atom = result.atoms.pop().unwrap();
            let pos = primary_atom.pos();

            let expr = match primary_atom {
                Atom::Ident { pos, val } => Ok(Expr::new(ExprSet::Identifier(val), pos)),
                Atom::StrLiteral { pos, val } => {
                    Ok(Expr::new(ExprSet::Literal(Literal::String(val)), pos))
                }
                Atom::IntLiteral { pos, val } => {
                    Ok(Expr::new(ExprSet::Literal(Literal::Integer(val)), pos))
                }
                Atom::FltLiteral { pos, val } => {
                    Ok(Expr::new(ExprSet::Literal(Literal::Float(val)), pos))
                }
                Atom::BoolLiteral { pos, val } => {
                    Ok(Expr::new(ExprSet::Literal(Literal::Boolean(val)), pos))
                }
                Atom::NoneLiteral { pos } => {
                    Ok(Expr::new(ExprSet::Literal(Literal::None), pos))
                }
                Atom::NullLiteral { pos } => {
                    Ok(Expr::new(ExprSet::Literal(Literal::Null), pos))
                }
                _ => Result::ize_err("Expected a primary expression".into(), pos),
            }?;

            println!("PRIMARY collector result {:?}", expr);

            Ok(expr)
        },
    )
}
