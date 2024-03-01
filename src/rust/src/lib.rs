use extendr_api::prelude::*;

pub mod node;
mod range;
use node::SgNode;
use range::{Pos, Range};

use ast_grep_core::{AstGrep, Language, NodeMatch, StrDoc};
use ast_grep_language::SupportLang;

// #[extendr]
// fn ast_grep_r(_py: Python, m: &PyModule) -> Result<()> {
//   // m.add_class::SgRoot()?;
//   m.add_class::<SgNode>()?;
//   m.add_class::<Range>()?;
//   m.add_class::<Pos>()?;
//   Ok(())
// }

#[derive(Clone)]
pub struct SgRoot {
    inner: AstGrep<StrDoc<SupportLang>>,
    filename: String,
}

#[extendr]
impl SgRoot {
    fn new(src: &str, lang: &str) -> Self {
        let lang: SupportLang = lang.parse().unwrap();
        let inner = lang.ast_grep(src);
        Self {
            inner,
            filename: "anonymous".into(),
        }
    }

    fn root(&self) -> SgNode {
        let tree = unsafe { &*(&self.inner as *const AstGrep<_>) } as &'static AstGrep<_>;
        let inner = NodeMatch::from(tree.root());
        SgNode {
            inner,
            root: self.clone(),
        }
    }

    fn filename(&self) -> &str {
        &self.filename
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod astgrepr;
    // fn ast_grep_r;
    impl SgRoot;
    use node;
}
