use crate::{
    lexer::{Token, TokenData, TokenType},
    parser::{Expr, Stmt},
};
use alloc::{borrow::ToOwned, string::String};
use regex::Regex;
use rustc_hash::FxHashMap;

pub struct EvalErr {
    pub message: String,
    pub line: usize,
    pub offset: usize,
}

enum ElemType {
    Variable,
    Constant,
    //TODO: other types: structs, transfers, etc
}

pub struct Interpreter {
    //TODO: distinguish variables, contants and other things. Scope.
    state: FxHashMap<String, (TokenData, ElemType)>,
    console_log: fn(&str),
}

impl Interpreter {
    pub fn new(console_log: fn(&str)) -> Self {
        Self {
            state: Default::default(),
            console_log,
        }
    }
}

impl Interpreter {
    pub fn eval_stmt(&mut self, stmt: &Stmt, line_num: usize) -> Result<TokenData, EvalErr> {
        match stmt {
            Stmt::ConstDef { const_name, init } => {
                let init_val = self.eval_expr(init, line_num)?;
                self.state
                    .insert(const_name.into(), (init_val, ElemType::Constant));
                Ok(TokenData::None)
            }
            Stmt::Print { args } => {
                let mut res_str = String::from("");
                for arg in args {
                    let res = self.eval_expr(arg, line_num)?;
                    res_str = res_str + format!(" {}", res).as_str();
                }
                (self.console_log)(format!("{}", res_str).as_str());
                Ok(TokenData::None)
            }
            Stmt::Expr(expr) => self.eval_expr(expr, line_num),
        }
    }

    pub fn eval_expr(&mut self, expr: &Expr, line_num: usize) -> Result<TokenData, EvalErr> {
        match expr {
            Expr::Literal(token) => Ok(token.data.clone()),
            Expr::Identifier(Token {
                data: TokenData::String(ident),
                offset,
                ..
            }) => {
                if let Some((v, ElemType::Variable | ElemType::Constant)) = self.state.get(ident) {
                    Ok(v.clone())
                } else {
                    Err(EvalErr {
                        message: format!("Variable/Constant '{}' doesn't exist", ident),
                        line: line_num,
                        offset: *offset,
                    })
                }
            }
            Expr::Group { expr } => self.eval_expr(expr, line_num),
            Expr::UnaryOp { op, child } => {
                let child_data = self.eval_expr(child, line_num)?;
                child_data.unary_op(op, line_num)
            }
            Expr::BinaryOp {
                op,
                left_child,
                right_child,
            } => {
                let left_child_data = self.eval_expr(left_child, line_num)?;
                // lazy AND, OR operators (&& , ||). If Left is false/true don't even evaluate Right.
                match (op.token_type, &left_child_data) {
                    (TokenType::TwoAnds, TokenData::Boolean(false)) => {
                        Ok(TokenData::Boolean(false))
                    }
                    (TokenType::TwoOrs, TokenData::Boolean(true)) => Ok(TokenData::Boolean(true)),
                    _ => {
                        let right_child_data = self.eval_expr(right_child, line_num)?;
                        left_child_data.binary_op(&right_child_data, op, line_num)
                    }
                }
            }
            Expr::AssignOp { dest, value } => {
                if let Expr::Identifier(Token {
                    data: TokenData::String(ident),
                    offset,
                    ..
                }) = dest.as_ref()
                {
                    match self.state.get(ident) {
                        Some((_, ElemType::Variable)) | None => {
                            let expr = self.eval_expr(value, line_num)?;
                            self.state
                                .insert(ident.clone(), (expr.clone(), ElemType::Variable));
                            Ok(expr)
                        }
                        _ => Err(EvalErr {
                            message: format!("Identifier '{}' is not a variable", ident),
                            line: line_num,
                            offset: *offset,
                        }),
                    }
                } else if let Expr::Object { chain: _ } = dest.as_ref() {
                    todo!("Implement object assignment")
                } else {
                    Err(EvalErr {
                        message: format!("Assignment destination must be a variable or an object"),
                        line: line_num,
                        offset: 0,
                    })
                }
            }
            Expr::Empty => Ok(TokenData::None),
            _ => todo!("Implement evaluation of all Expr variants"),
        }
    }
}

trait Operation {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr>;
    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr>;
}

impl Operation for TokenData {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match self {
            TokenData::Integer(i) => i.unary_op(op, line_num),
            TokenData::Float(f) => f.unary_op(op, line_num),
            TokenData::Boolean(b) => b.unary_op(op, line_num),
            TokenData::String(s) => s.unary_op(op, line_num),
            TokenData::Regex(r) => r.unary_op(op, line_num),
            TokenData::None => Err(EvalErr {
                message: format!(
                    "Invalid data type 'None' for unary operation '{}'",
                    op.token_type
                ),
                line: line_num,
                offset: op.offset,
            }),
        }
    }

    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match (self, right) {
            (TokenData::String(left), TokenData::String(right)) => {
                left.binary_op(right, op, line_num)
            }
            (TokenData::Regex(left), TokenData::Regex(right)) => {
                left.binary_op(right, op, line_num)
            }
            (TokenData::Integer(left), TokenData::Integer(right)) => {
                left.binary_op(right, op, line_num)
            }
            (TokenData::Float(left), TokenData::Float(right)) => {
                left.binary_op(right, op, line_num)
            }
            (TokenData::Boolean(left), TokenData::Boolean(right)) => {
                left.binary_op(right, op, line_num)
            }
            _ => Err(EvalErr {
                message: format!("Mismatching types for binary operation '{}'", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }
}

impl Operation for i64 {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::Minus => {
                let res = -self;
                Ok(TokenData::Integer(res))
            }
            TokenType::Exclam => {
                let res = !self;
                Ok(TokenData::Integer(res))
            }
            _ => Err(EvalErr {
                message: format!("Invalid unary operator '{}' for integer", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }

    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::Plus => Ok(TokenData::Integer(self + right)),
            TokenType::Minus => Ok(TokenData::Integer(self - right)),
            TokenType::Star => Ok(TokenData::Integer(self * right)),
            TokenType::Slash => Ok(TokenData::Integer(self / right)),
            TokenType::Percent => Ok(TokenData::Integer(self % right)),
            TokenType::And => Ok(TokenData::Integer(self & right)),
            TokenType::Or => Ok(TokenData::Integer(self | right)),
            TokenType::OpenAngleBrack => Ok(TokenData::Boolean(self < right)),
            TokenType::ClosingAngleBrack => Ok(TokenData::Boolean(self > right)),
            TokenType::LtEqual => Ok(TokenData::Boolean(self <= right)),
            TokenType::GtEqual => Ok(TokenData::Boolean(self >= right)),
            TokenType::TwoEquals => Ok(TokenData::Boolean(self == right)),
            TokenType::NotEqual => Ok(TokenData::Boolean(self != right)),
            _ => Err(EvalErr {
                message: format!("Invalid binary operator '{}' for integer", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }
}

impl Operation for f64 {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::Minus => {
                let res = -self;
                Ok(TokenData::Float(res))
            }
            _ => Err(EvalErr {
                message: format!("Invalid unary operator '{}' for float", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }

    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::Plus => Ok(TokenData::Float(self + right)),
            TokenType::Minus => Ok(TokenData::Float(self - right)),
            TokenType::Star => Ok(TokenData::Float(self * right)),
            TokenType::Slash => Ok(TokenData::Float(self / right)),
            TokenType::Percent => Ok(TokenData::Float(self % right)),
            TokenType::OpenAngleBrack => Ok(TokenData::Boolean(self < right)),
            TokenType::ClosingAngleBrack => Ok(TokenData::Boolean(self > right)),
            TokenType::LtEqual => Ok(TokenData::Boolean(self <= right)),
            TokenType::GtEqual => Ok(TokenData::Boolean(self >= right)),
            TokenType::TwoEquals => Ok(TokenData::Boolean(self == right)),
            TokenType::NotEqual => Ok(TokenData::Boolean(self != right)),
            _ => Err(EvalErr {
                message: format!("Invalid binary operator '{}' for float", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }
}

impl Operation for bool {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::Exclam => {
                let res = !self;
                Ok(TokenData::Boolean(res))
            }
            _ => Err(EvalErr {
                message: format!("Invalid unary operator '{}' for boolean", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }

    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::And => Ok(TokenData::Boolean(self & right)),
            TokenType::Or => Ok(TokenData::Boolean(self | right)),
            TokenType::TwoAnds => Ok(TokenData::Boolean(*self && *right)),
            TokenType::TwoOrs => Ok(TokenData::Boolean(*self || *right)),
            TokenType::OpenAngleBrack => Ok(TokenData::Boolean(self < right)),
            TokenType::ClosingAngleBrack => Ok(TokenData::Boolean(self > right)),
            TokenType::LtEqual => Ok(TokenData::Boolean(self <= right)),
            TokenType::GtEqual => Ok(TokenData::Boolean(self >= right)),
            TokenType::TwoEquals => Ok(TokenData::Boolean(self == right)),
            TokenType::NotEqual => Ok(TokenData::Boolean(self != right)),
            _ => Err(EvalErr {
                message: format!("Invalid binary operator '{}' for boolean", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }
}

impl Operation for String {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        Err(EvalErr {
            message: format!(
                "Invalid data type 'String' for unary operation '{}'",
                op.token_type
            ),
            line: line_num,
            offset: op.offset,
        })
    }

    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::Plus => Ok(TokenData::String(self.to_owned() + right)),
            TokenType::OpenAngleBrack => Ok(TokenData::Boolean(self < right)),
            TokenType::ClosingAngleBrack => Ok(TokenData::Boolean(self > right)),
            TokenType::LtEqual => Ok(TokenData::Boolean(self <= right)),
            TokenType::GtEqual => Ok(TokenData::Boolean(self >= right)),
            TokenType::TwoEquals => Ok(TokenData::Boolean(self == right)),
            TokenType::NotEqual => Ok(TokenData::Boolean(self != right)),
            _ => Err(EvalErr {
                message: format!("Invalid binary operator '{}' for string", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }
}

impl Operation for Regex {
    fn unary_op(&self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        Err(EvalErr {
            message: format!(
                "Invalid data type 'Regex' for unary operation '{}'",
                op.token_type
            ),
            line: line_num,
            offset: op.offset,
        })
    }

    fn binary_op(&self, right: &Self, op: &Token, line_num: usize) -> Result<TokenData, EvalErr> {
        match op.token_type {
            TokenType::OpenAngleBrack => Ok(TokenData::Boolean(self.as_str() < right.as_str())),
            TokenType::ClosingAngleBrack => Ok(TokenData::Boolean(self.as_str() > right.as_str())),
            TokenType::LtEqual => Ok(TokenData::Boolean(self.as_str() <= right.as_str())),
            TokenType::GtEqual => Ok(TokenData::Boolean(self.as_str() >= right.as_str())),
            TokenType::TwoEquals => Ok(TokenData::Boolean(self.as_str() == right.as_str())),
            TokenType::NotEqual => Ok(TokenData::Boolean(self.as_str() != right.as_str())),
            _ => Err(EvalErr {
                message: format!("Invalid binary operator '{}' for string", op.token_type),
                line: line_num,
                offset: op.offset,
            }),
        }
    }
}
