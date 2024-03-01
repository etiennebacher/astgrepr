use ast_grep_core::{Doc, Node};
use extendr_api::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

pub type RResult<T> = core::result::Result<T, std::fmt::Error>;

#[derive(Clone, PartialEq, Eq, Hash)]
#[extendr]
pub struct Pos {
    /// line number starting from 0
    line: usize,
    /// column number starting from 0
    column: usize,
    // TODO: this should be char offset
    /// byte offset of the position
    index: usize,
}

impl Display for Pos {
    fn fmt(&self, f: &mut Formatter<'_>) -> RResult<()> {
        write!(f, "({},{})", self.line, self.column)
    }
}

impl Debug for Pos {
    fn fmt(&self, f: &mut Formatter<'_>) -> RResult<()> {
        write!(
            f,
            "Pos(line={}, col={}, index={})",
            self.line, self.column, self.index
        )
    }
}

#[extendr]
impl Pos {
    fn __hash__(&self) -> u64 {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
    }
    // fn __eq__(&self, other: &Self) -> bool {
    //   self == other
    // }
    fn __repr__(&self) -> String {
        format!("{:?}", self)
    }
    fn __str__(&self) -> String {
        self.to_string()
    }
}

fn to_pos(pos: (usize, usize), offset: usize) -> Pos {
    Pos {
        line: pos.0,
        column: pos.1,
        index: offset,
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Range {
    /// starting position of the range
    start: Pos,
    /// ending position of the range
    end: Pos,
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> RResult<()> {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl Debug for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> RResult<()> {
        write!(f, "Range(start={:?}, end={:?})", self.start, self.end)
    }
}

#[extendr]
impl Range {
    // fn __eq__(&self, other: &Self) -> bool {
    //   self == other
    // }
    fn __hash__(&self) -> u64 {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
    }
    fn __repr__(&self) -> String {
        format!("{:?}", self)
    }
    fn __str__(&self) -> String {
        self.to_string()
    }
}

impl Range {
    pub fn from<D: Doc>(node: &Node<D>) -> Self {
        let byte_range = node.range();
        let start_pos = node.start_pos();
        let end_pos = node.end_pos();
        Range {
            start: to_pos(start_pos, byte_range.start),
            end: to_pos(end_pos, byte_range.end),
        }
    }
}
