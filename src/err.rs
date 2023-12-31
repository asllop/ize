use crate::common::TokenPos;
use alloc::string::String;

#[derive(Debug, Default)]
/// Compiler error.
pub struct IzeErr {
    /// Error message.
    pub message: String,
    /// Position where the error was found.
    pub pos: TokenPos,
}

impl IzeErr {
    /// Build a new error from message and the token position that caused the error.
    pub fn new(message: String, pos: TokenPos) -> Self {
        IzeErr { message, pos }
    }
}
