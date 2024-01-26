//! # Expression Grammars
//!
//! Contains the grammar rules to parse IZE expressions. From lower to higher precedence:
//!
//! - Chain: `a;b;c`
//! - Let: `let var a`
//! - Binary - Equality: `a == b`
//! - Binary - Comparison: `a > b`
//! - Binary - Logic: `a & b`
//! - Binary - Term: `a + b`
//! - Binary - Factor: `a * b`
//! - Unary: `!a`
//! - Dot: `a.b.c`
//! - Select/Unwrap: `select (a) (b -> c)`
//! - If-Else: `if (a) b else c`
//! - Call: `foo(a,b)`
//! - Group: `(a)`
//! - Pair: `id : a`
//! - Type: `Mux[A,B,C]`
//! - Primary: Literals and identifiers.
//!
//! By calling an expression parser function, only that or higher precedence expressions can be parsed. For instance, calling [expr_pair](crate::grammar_expr::expr_pair)
//! will only parse expressions Pair, Type and Primary. The top level function is [expr](crate::grammar_expr::expr) which can parse any kind of expression.

use alloc::{boxed::Box, vec::Vec};

use crate::{
    ast::{AstNode, Expression, ExpressionKind},
    err::IzeErr,
    lexer::{Token, TokenKind},
    parser::{Parser::*, *},
};

////////////////////////
// Expression parsers //
////////////////////////

/// Parse an expression.
pub fn expr(input: &[Token]) -> IzeResult {
    expr_chain(input)
}

/// Parse a Chain expression.
pub fn expr_chain(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_let, 1),
            Zero(&[Key(TokenKind::Semicolon, 2), Fun(expr_let, 3)]),
        ],
        |mut node_vec| {
            let chain_vec = node_vec.pop().unwrap().vec().unwrap();
            let let_expr = node_vec.pop().unwrap();

            if !chain_vec.is_empty() {
                let mut expr_vec = vec![let_expr];
                for chain_pair in chain_vec {
                    let mut chain_pair = chain_pair.vec().unwrap();
                    let expr = chain_pair.pop().unwrap();
                    expr_vec.push(expr);
                }
                Expression::new_chain(expr_vec).into()
            } else {
                // Precedence
                let_expr
            }
        },
        |_, e| {
            if e.id == 3 {
                Err(IzeErr::new(
                    "Chain expression, expected expression after semicolon".into(),
                    e.err.pos,
                )
                .into())
            } else {
                // Propagate the error we received.
                Err(e)
            }
        },
    )
}

/// Parse a Let expression.
pub fn expr_let(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Key(TokenKind::Let, 1),
            Fun(token_ident, 2),
            Fun(expr_let, 3),
        ],
        |mut node_vec| {
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let ident = node_vec.pop().unwrap().token().unwrap();
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos;
            Expression::new_let(ident, expr, start_pos).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => expr_equality(input),
                // Errors
                2 => Err(IzeErr::new(
                    "Let expression, expected identifier after 'let'".into(),
                    e.err.pos,
                )
                .into()),
                3 => Err(IzeErr::new(
                    "Let expression, expected expression after variable name".into(),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Equality Binary expression.
pub fn expr_equality(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_comparison, 1),
            Zero(&[
                Sel(&[Key(TokenKind::TwoEquals, 2), Key(TokenKind::NotEqual, 2)]),
                Fun(expr_comparison, 3),
            ]),
        ],
        binary_expr_success,
        binary_expr_error,
    )
}

/// Parse a Comparison Binary expression.
pub fn expr_comparison(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_logic, 1),
            Zero(&[
                Sel(&[
                    Key(TokenKind::GreaterThan, 2),
                    Key(TokenKind::LesserThan, 2),
                    Key(TokenKind::GtEqual, 2),
                    Key(TokenKind::LtEqual, 2),
                    Key(TokenKind::TwoAnds, 2),
                    Key(TokenKind::TwoOrs, 2),
                ]),
                Fun(expr_logic, 3),
            ]),
        ],
        binary_expr_success,
        binary_expr_error,
    )
}

/// Parse a Logic Binary expression.
pub fn expr_logic(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_term, 1),
            Zero(&[
                Sel(&[Key(TokenKind::And, 2), Key(TokenKind::Or, 2)]),
                Fun(expr_term, 3),
            ]),
        ],
        binary_expr_success,
        binary_expr_error,
    )
}

/// Parse a Term Binary expression.
pub fn expr_term(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_factor, 1),
            Zero(&[
                Sel(&[Key(TokenKind::Plus, 2), Key(TokenKind::Minus, 2)]),
                Fun(expr_factor, 3),
            ]),
        ],
        binary_expr_success,
        binary_expr_error,
    )
}

/// Parse a Factor Binary expression.
pub fn expr_factor(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_unary, 1),
            Zero(&[
                Sel(&[
                    Key(TokenKind::Star, 2),
                    Key(TokenKind::Slash, 2),
                    Key(TokenKind::Percent, 2),
                ]),
                Fun(expr_unary, 3),
            ]),
        ],
        binary_expr_success,
        binary_expr_error,
    )
}

/// Parse a Unary expression.
pub fn expr_unary(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Sel(&[Tk(TokenKind::Minus, 1), Tk(TokenKind::Not, 1)]),
            Fun(expr_unary, 2),
        ],
        |mut node_vec| {
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let op = node_vec.pop().unwrap().token().unwrap();
            Expression::new_unary(op, expr).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => expr_dot(input),
                // Errors
                2 => Err(
                    IzeErr::new("Expected expression after unary operator".into(), e.err.pos)
                        .into(),
                ),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Dot expression.
pub fn expr_dot(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(expr_select_unwrap, 1),
            Zero(&[Key(TokenKind::Dot, 2), Fun(expr_select_unwrap, 3)]),
        ],
        |mut node_vec| {
            let dot_vec = node_vec.pop().unwrap().vec().unwrap();
            let next_expr = node_vec.pop().unwrap();

            if !dot_vec.is_empty() {
                let mut expr_vec = vec![next_expr];
                for dot_pair in dot_vec {
                    let mut dot_pair = dot_pair.vec().unwrap();
                    let expr = dot_pair.pop().unwrap();
                    expr_vec.push(expr);
                }
                Expression::new_dot(expr_vec).into()
            } else {
                // Precedence
                next_expr
            }
        },
        |_, e| {
            if e.id == 3 {
                Err(IzeErr::new("Expected expression after dot".into(), e.err.pos).into())
            } else {
                // Propagate the error we received.
                Err(e)
            }
        },
    )
}

/// Parse a Select/Unwrap expressions
pub fn expr_select_unwrap(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Sel(&[Tk(TokenKind::Select, 1), Tk(TokenKind::Unwrap, 1)]),
            Tk(TokenKind::OpenParenth, 2),
            Fun(expr, 3),
            Opt(&[Key(TokenKind::As, 4), Fun(token_ident, 5)]),
            Tk(TokenKind::ClosingParenth, 6),
            Tk(TokenKind::OpenParenth, 7),
            Fun(arm_expr, 8),
            Zero(&[Key(TokenKind::Comma, 9), Fun(arm_expr, 10)]),
            Tk(TokenKind::ClosingParenth, 11),
        ],
        |mut node_vec| {
            let end_pos = node_vec.pop().unwrap().token().unwrap().pos; // token ")"
            let arms = node_vec.pop().unwrap().vec().unwrap();
            let first_arm = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // token "("
            node_vec.pop().unwrap().token().unwrap(); // token ")"
            let maybe_as_ident = node_vec.pop().unwrap();
            let (alias, expr) = if let AstNode::Vec(mut as_ident) = maybe_as_ident {
                // Is the alias
                let alias = as_ident.pop().unwrap().token().unwrap();
                let expr = node_vec.pop().unwrap().expr().unwrap();
                (Some(alias), expr)
            } else {
                // Is the expression
                let expr = maybe_as_ident.expr().unwrap();
                (None, expr)
            };
            node_vec.pop().unwrap().token().unwrap(); // token "("
            let op = node_vec.pop().unwrap().token().unwrap(); // "select" or "unwrap" token

            // Collect arms
            let mut arm_expressions = vec![first_arm.into()];
            for arm in arms {
                let mut arm = arm.vec().unwrap();
                let arm_expr = arm.pop().unwrap();
                arm_expressions.push(arm_expr);
            }

            // Generate expression
            Expression::new_select_unwrap(op, expr, alias, arm_expressions, end_pos).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => expr_ifelse(input),
                // Errors
                2 => Err(IzeErr::new(
                    "Expected '(' after 'select' or 'unwrap' keyword".into(),
                    e.err.pos,
                )
                .into()),
                3 => Err(IzeErr::new("Expected expression after '('".into(), e.err.pos).into()),
                5 => Err(IzeErr::new("Expected identifier after 'as'".into(), e.err.pos).into()),
                6 => Err(IzeErr::new("Expected ')'".into(), e.err.pos).into()),
                7 => Err(IzeErr::new("Expected '('".into(), e.err.pos).into()),
                8 => Err(IzeErr::new("Expected arm expression after '('".into(), e.err.pos).into()),
                10 => {
                    Err(IzeErr::new("Expected arm expression after comma".into(), e.err.pos).into())
                }
                11 => Err(IzeErr::new(
                    "Expected ')' or comma after arm expression".into(),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse an If-Else expression.
pub fn expr_ifelse(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Key(TokenKind::If, 1),
            Tk(TokenKind::OpenParenth, 2),
            Fun(expr, 3),
            Tk(TokenKind::ClosingParenth, 4),
            Fun(expr, 5),
            Tk(TokenKind::Else, 6),
            Fun(expr, 7),
        ],
        |mut node_vec| {
            let else_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token "else"
            let if_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token ")"
            let cond_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Token "("
            let start_pos = node_vec.pop().unwrap().token().unwrap().pos; // Token "if"
            Expression::new_ifelse(cond_expr, if_expr, else_expr, start_pos).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => expr_call(input),
                // Errors
                2 => Err(IzeErr::new(
                    "If-Else expression, expected '(' after 'if'".into(),
                    e.err.pos,
                )
                .into()),
                3 => Err(IzeErr::new(
                    "If-Else expression, expected condition expression".into(),
                    e.err.pos,
                )
                .into()),
                4 => Err(IzeErr::new(
                    "If-Else expression, expected ')' after condition".into(),
                    e.err.pos,
                )
                .into()),
                5 | 7 => Err(IzeErr::new(
                    "If-Else expression, expected branch expression".into(),
                    e.err.pos,
                )
                .into()),
                6 => Err(IzeErr::new(
                    "If-Else expression, expected 'else' after branch expression".into(),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Call expression.
pub fn expr_call(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(token_ident, 1),
            Tk(TokenKind::OpenParenth, 2),
            Tk(TokenKind::ClosingParenth, 3),
        ],
        |mut node_vec| {
            let end_pos = node_vec.pop().unwrap().token().unwrap().pos;
            node_vec.pop().unwrap().token().unwrap(); // Discard token "("
            let ident = node_vec.pop().unwrap().token().unwrap();
            Expression::new_call(ident, Default::default(), end_pos).into()
        },
        |input, e| {
            match e.id {
                // Is not a call, precedence to Group expression
                1 | 2 => expr_group(input),
                // It's a call, but not empty, precedence to the next Call parser
                _ => expr_call_with_args(input),
            }
        },
    )
}

/// Parse a Call expression with arguments.
fn expr_call_with_args(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(token_ident, 1),
            Tk(TokenKind::OpenParenth, 2),
            Fun(expr, 3),
            Zero(&[Key(TokenKind::Comma, 4), Fun(expr, 5)]),
            Tk(TokenKind::ClosingParenth, 6),
        ],
        |mut node_vec| {
            let end_pos = node_vec.pop().unwrap().token().unwrap().pos; // ")" token
            let arg_pairs_vec = node_vec.pop().unwrap().vec().unwrap();
            let first_arg = node_vec.pop().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // discard "(" token
            let ident = node_vec.pop().unwrap().token().unwrap();

            let mut args = vec![first_arg];
            for pair in arg_pairs_vec {
                let mut pair = pair.vec().unwrap();
                let arg = pair.pop().unwrap();
                args.push(arg);
            }

            Expression::new_call(ident, args, end_pos).into()
        },
        |_, e| match e.id {
            3 => Err(IzeErr::new("Expected expression after '('".into(), e.err.pos).into()),
            5 => Err(IzeErr::new("Expected expression after comma".into(), e.err.pos).into()),
            6 => Err(IzeErr::new("Expected ')' after expression".into(), e.err.pos).into()),
            _ => Err(e),
        },
    )
}

/// Parse a Group expression.
pub fn expr_group(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Tk(TokenKind::OpenParenth, 1),
            Fun(expr, 2),
            Tk(TokenKind::ClosingParenth, 3),
        ],
        |mut node_vec| {
            let end = node_vec.pop().unwrap().token().unwrap().pos; // Token ")"
            let expr = node_vec.pop().unwrap().expr().unwrap();
            let start = node_vec.pop().unwrap().token().unwrap().pos; // Token "("
            Expression::new_group(expr, start, end).into()
        },
        |input, e| {
            match e.id {
                // Precedence
                1 => expr_pair(input),
                // Errors
                2 => Err(IzeErr::new(
                    "Group expression expected expression after open parenthesis".into(),
                    e.err.pos,
                )
                .into()),
                3 => Err(IzeErr::new(
                    "Group expression expected a closing parenthesis".into(),
                    e.err.pos,
                )
                .into()),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Pair expression.
pub fn expr_pair(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Fun(token_ident, 1), Tk(TokenKind::Colon, 2), Fun(expr, 3)],
        |mut node_vec| {
            let right_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // colon token
            let left_ident = node_vec.pop().unwrap().token().unwrap();
            let left_expr = Box::new(Expression::new_primary(left_ident));
            Expression::new_pair(left_expr, right_expr).into()
        },
        |input, e| match e.id {
            // Precedence
            1 | 2 => expr_type(input),
            // Errors
            3 => Err(IzeErr::new("Expected expression after colon".into(), e.err.pos).into()),
            _ => Err(e),
        },
    )
}

/// Parse a Type expression.
pub fn expr_type(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[
            Fun(type_name, 1),
            Tk(TokenKind::OpenClause, 2),
            Fun(expr_type, 3), // if subtype is not List, Map, Mux or Tuple, it will be parsed as a primary expr.
            Zero(&[Key(TokenKind::Comma, 4), Fun(expr_type, 5)]),
            Tk(TokenKind::ClosingClause, 6),
        ],
        collect_type,
        |input, e| {
            match e.id {
                // Precedence
                1 => expr_primary(input),
                // Errors
                2 => Err(
                    IzeErr::new("Expected opening clause after type name".into(), e.err.pos).into(),
                ),
                3 => Err(IzeErr::new("Expected a subtype".into(), e.err.pos).into()),
                5 => Err(IzeErr::new("Expected subtype after comma".into(), e.err.pos).into()),
                6 => Err(
                    IzeErr::new("Expected closing clause after subtype".into(), e.err.pos).into(),
                ),
                _ => Err(e),
            }
        },
    )
}

/// Parse a Primary expression.
pub fn expr_primary(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Sel(&[
            Fun(token_ident, 1),
            Fun(token_int, 1),
            Fun(token_flt, 1),
            Fun(token_str, 1),
            Fun(token_bool, 1),
            Tk(TokenKind::NoneLiteral, 1),
            Tk(TokenKind::NullLiteral, 1),
        ])],
        |mut node_vec| {
            let token = node_vec.pop().unwrap().token().unwrap();
            Expression::new_primary(token).into()
        },
        |_, e| Err(IzeErr::new("Error parsing primary expr".into(), e.err.pos).into()),
    )
}

///////////////////////
// Support functions //
///////////////////////

/// Parse an Arm expression.
fn arm_expr(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Fun(expr, 1), Tk(TokenKind::Arrow, 2), Fun(expr, 3)],
        |mut node_vec| {
            let right_expr = node_vec.pop().unwrap().expr().unwrap();
            node_vec.pop().unwrap().token().unwrap(); // Arrow token
            let left_expr = node_vec.pop().unwrap().expr().unwrap();
            Expression::new_arm(left_expr, right_expr).into()
        },
        |_, e| Err(e),
    )
}

/// Parse a type name.
fn type_name(input: &[Token]) -> IzeResult {
    def_grammar(
        input,
        &[Sel(&[
            Tk(TokenKind::List, 1),
            Tk(TokenKind::Map, 2),
            Tk(TokenKind::Mux, 3),
            Tk(TokenKind::Tuple, 4),
        ])],
        |mut node_vec| node_vec.pop().unwrap(),
        |_, e| Err(e),
    )
}

/// Collect type expression results.
fn collect_type(mut node_vec: Vec<AstNode>) -> AstNode {
    let mut subtypes_vec = vec![];

    let end_pos = node_vec.pop().unwrap().token().unwrap().pos; // token "]"
    let following_subtypes_vec = node_vec.pop().unwrap().vec().unwrap(); // Each element is a pair of Comma - Type -> Either a Vec (a type ready to be collected) or a Primary ident.
    let first_subtype = node_vec.pop().unwrap(); // Either a Vec (a type ready to be collected) or a Primary ident
    node_vec.pop().unwrap().token().unwrap(); // token "["
    let ident = node_vec.pop().unwrap().token().unwrap();

    subtypes_vec.push(collect_subtype(first_subtype));

    for subtype in following_subtypes_vec {
        // Ignore comma, get type
        let type_node = subtype.vec().unwrap().pop().unwrap();
        subtypes_vec.push(collect_subtype(type_node.into()));
    }

    Expression::new_type(ident, subtypes_vec, end_pos).into()
}

/// Collect the subtype of a type expression results.
fn collect_subtype(subtype: AstNode) -> AstNode {
    match subtype {
        AstNode::Vec(subtype_vec) => {
            let node = collect_type(subtype_vec);
            node
        }
        AstNode::Expression(expr) => match expr.kind {
            ExpressionKind::Primary { token } => {
                let token = token.token().unwrap();
                let end_pos = token.pos;
                let node = Expression::new_type(token, vec![], end_pos).into();
                node
            }
            ExpressionKind::Type {
                ident_token,
                subtypes_vec,
            } => Expression {
                kind: ExpressionKind::Type {
                    ident_token,
                    subtypes_vec,
                },
                start_pos: expr.start_pos,
                end_pos: expr.end_pos,
            }
            .into(),
            _ => panic!("Unexpected expression while parsing subtype: {:#?}", expr),
        },
        _ => panic!(
            "Unexpected node variant while parsing subtype: {:#?}",
            subtype
        ),
    }
}

/// Collect binary expression
fn binary_expr_success(mut node_vec: Vec<AstNode>) -> AstNode {
    let expr_vec = node_vec.pop().unwrap().vec().unwrap();
    let next_expr = node_vec.pop().unwrap();

    if !expr_vec.is_empty() {
        let mut final_expr = next_expr.expr().unwrap();
        for expr_pair in expr_vec {
            let mut expr_pair = expr_pair.vec().unwrap();
            let right_expr = expr_pair.pop().unwrap().expr().unwrap();
            let op = expr_pair.pop().unwrap().token().unwrap();
            final_expr = Expression::new_binary(op, final_expr, right_expr).into();
        }
        final_expr.into()
    } else {
        // Precedence
        next_expr
    }
}

/// Generate error for binary expression
fn binary_expr_error(_: &[Token], e: ParseErr) -> IzeResult {
    if e.id == 3 {
        Err(IzeErr::new("Expected expression after operator".into(), e.err.pos).into())
    } else {
        Err(e)
    }
}
