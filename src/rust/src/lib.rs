use extendr_api::prelude::*;
mod language;
pub mod node;
pub mod range;
pub mod unicode_position;
use crate::unicode_position::UnicodePosition;
use ast_grep_core::Language;
use ast_grep_core::{AstGrep, NodeMatch, StrDoc};

use language::R;
use node::SgNode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Offset {
    char_offset: usize,
    byte_offset: usize,
    len: u8,
}
#[derive(Clone)]
pub struct SgRoot {
    inner: AstGrep<StrDoc<crate::language::R>>,
    filename: String,
    position: UnicodePosition,
}

#[extendr]
impl SgRoot {
    fn new(src: &str) -> Self {
        let position = UnicodePosition::new(src);
        let inner = R.ast_grep(src);
        Self {
            inner,
            filename: "anonymous".into(),
            position,
        }
    }

    fn root(&self) -> SgNode {
        // Clone the inner data to ensure it can outlive this method
        let tree: Box<AstGrep<_>> = Box::new(self.inner.clone());

        // SAFETY: `Box::leak` makes the data have a `'static` lifetime
        let static_tree: &'static AstGrep<_> = Box::leak(tree);

        let inner = NodeMatch::from(static_tree.root());

        SgNode {
            inner,
            root: self.clone(),
        }
    }

    fn filename(&self) -> &str {
        &self.filename
    }
}

extendr_module! {
    mod astgrepr;
    // fn ast_grep_r;
    impl SgRoot;
    use node;
}
