use alloc::string::String;

#[derive(Debug)]
/// Compiler error.
pub struct IzeErr {
    /// Error message.
    pub message: String,
    /// Position where the error was found.
    pub pos: Pos,
}

/// Build an ize error for any generic result type.
pub trait BuildErr {
    /// Build ize error.
    fn ize_err(message: String, pos: Pos) -> Self;
}

impl<T> BuildErr for Result<T, IzeErr> {
    fn ize_err(message: String, pos: Pos) -> Self {
        Err(IzeErr { message, pos })
    }
}

#[derive(Debug, Default, Copy, Clone)]
/// Position of language element in the code.
pub struct Pos {
    pub row: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}
