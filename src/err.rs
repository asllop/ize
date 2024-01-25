use crate::lexer::TokenPos;
use alloc::string::String;

//TODO: Implement a backtrace, a vector of IzeErr to view the complete chain of errors, from top to bottom level.
//      Every time the is an error we append a message to the backtrack and pass down the new IzeErr object.

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
