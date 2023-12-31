#[derive(Default, Debug, Clone, Copy)]
/// Position of a token in the code.
pub struct TokenPos {
    /// Line.
    pub line: usize,
    /// Starting column.
    pub start_col: usize,
    /// Ending column.
    pub end_col: usize,
    /// Absolute position from start of file.
    pub seek: usize,
}

impl TokenPos {
    pub fn new(line: usize, start_col: usize, end_col: usize, seek: usize) -> Self {
        Self {
            line,
            start_col,
            end_col,
            seek,
        }
    }
}
