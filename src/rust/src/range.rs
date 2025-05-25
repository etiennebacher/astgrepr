use crate::unicode_position::UnicodePosition;
use ast_grep_core::{Doc, Node, Position};
use extendr_api::prelude::*;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

pub type RResult<T> = core::result::Result<T, std::fmt::Error>;

#[derive(Clone, PartialEq, Eq, Hash)]
#[extendr]
pub struct Pos {
    /// line number starting from 0
    pub line: usize,
    /// column number starting from 0
    pub column: usize,
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

pub fn to_pos<D: Doc>(node: &Node<D>, pos: Position, offset: usize) -> Pos {
    Pos {
        line: pos.line(),
        column: pos.column(node),
        index: offset,
    }
}

#[extendr]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Range {
    /// starting position of the range
    pub start: Pos,
    /// ending position of the range
    pub end: Pos,
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
    fn __eq__(&self, other: &Self) -> bool {
        self == other
    }
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
    pub fn from<D: Doc>(node: &Node<D>, positioner: &UnicodePosition) -> Self {
        let byte_range = node.range();
        let start_pos = node.start_pos();
        let end_pos = node.end_pos();
        let start = positioner.byte_to_char(byte_range.start);
        let end = positioner.byte_to_char(byte_range.end);
        Range {
            start: to_pos(node, start_pos, start),
            end: to_pos(node, end_pos, end),
        }
    }
}
