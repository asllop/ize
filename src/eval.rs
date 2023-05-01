use crate::{
    lexer::{Token, TokenData, TokenType},
    parser::Expr,
};
use alloc::string::String;
use regex::Regex;

pub struct EvalErr {
    pub message: String,
    pub line: usize,
    pub offset: usize,
}

pub struct Interpreter {
    //TODO: store context, variables, constants, defined types, etc.
}

impl Interpreter {
    pub fn eval(expr: &Expr, line_num: usize) -> Result<TokenData, EvalErr> {
        match expr {
            Expr::Lit(token) => Ok(token.data.clone()),
            Expr::Group { expression } => Self::eval(expression, line_num),
            Expr::UnaryOp { op, child } => {
                let child_data = Self::eval(child, line_num)?;
                child_data.unary_op(op, line_num)
            }
            Expr::BinaryOp {
                op,
                left_child,
                right_child,
            } => {
                let left_child_data = Self::eval(left_child, line_num)?;
                let right_child_data = Self::eval(right_child, line_num)?;
                left_child_data.binary_op(&right_child_data, op, line_num)
            }
            Expr::Empty => Ok(TokenData::None),
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
        todo!("Implement binary operations for Integer types")
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
        todo!("Implement binary operations for Float types")
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
        todo!("Implement binary operations for Boolean types")
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
        todo!("Implement binary operations for String types")
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
        todo!("Implement binary operations for Regex types")
    }
}
