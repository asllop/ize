use crate::pos::RangePos;
use alloc::string::String;

/*
TODO: error backtrace.
    Implement a vector of IzeErr to view the complete chain of errors, from top to bottom level.
    Every time the is an error we append a message to the backtrack and pass down the new IzeErr object.

    Create a method to build a new error, getting an IzeErr as argument.
    This method will crate a new IzeErr adn put the IzeErr passed in the backtrack, itself following by its backtrack.
    This way the error received is always the most top level, but we still have the details in the backtrack. down to the bottom level.

TODO: source file
    The .iz file that generated the error.

TODO: origin.
    Add an attribute to IzeErr signaling the entity that generated the error. For example: PlusExpression, Lexer, ModelSemcheck, etc.
    It should be an enum, so the possible entities are limited. We could add a variant Other(String) for generic entities.
*/

#[derive(Debug, Default)]
/// Compiler error.
pub struct IzeErr {
    /// Error message.
    pub message: String,
    /// Position of code that caused the error.
    pub pos: RangePos,
}

impl IzeErr {
    /// Build a new error from message and the token position that caused the error.
    pub fn new(message: String, pos: RangePos) -> Self {
        IzeErr { message, pos }
    }
}
