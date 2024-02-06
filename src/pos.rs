use core::ops::Sub;

#[derive(Default, Debug, Clone, Copy, PartialEq)]
/// A position in the code.
pub struct Pos {
    /// Line.
    pub line: usize,
    /// Column.
    pub col: usize,
    /// Absolute position of line-col from start of file.
    pub seek: usize,
}

impl Pos {
    /// New position.
    pub fn new(line: usize, col: usize, seek: usize) -> Self {
        Self { line, col, seek }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
/// A position range in the code (start-end).
pub struct RangePos {
    /// Start position.
    pub start: Pos,
    /// End position.
    pub end: Pos,
}

impl RangePos {
    /// Create range position.
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    /// Create range position with start and end positions in the same line.
    pub fn inline_new(line: usize, start_col: usize, end_col: usize, start_seek: usize) -> Self {
        Self::new(
            Pos::new(line, start_col, start_seek),
            Pos::new(line, end_col, start_seek + (end_col - start_col))
        )
    }
}

impl Sub for Pos {
    type Output = RangePos;

    /// **NOTE**: Substract end pos minus start pos to form a correct RangePos.
    fn sub(self, other: Self) -> Self::Output {
        RangePos::new(other, self)
    }
}