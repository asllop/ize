use alloc::string::String;

#[derive(Debug)]
/// Compiler error.
pub struct IzeErr {
    /// Error message.
    pub message: String,
    /// Position where the error was found.
    pub pos: Pos,
}

#[derive(Debug, Default, Clone)]
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
