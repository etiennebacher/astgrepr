use extendr_api::prelude::*;
pub mod node;
mod range;
pub mod ser;
use node::SgNode;
// use range::{Pos, Range};

use ast_grep_core::language::TSLanguage;
use ast_grep_core::{AstGrep, Language, NodeMatch, StrDoc};
use ast_grep_language::SupportLang;

#[derive(Clone)]
pub struct SgRoot {
    inner: AstGrep<StrDoc<TSLanguage>>,
    filename: String,
}

#[extendr]
impl SgRoot {
    fn new(src: &str) -> Self {
        let rlang = tree_sitter_r::language();
        let lang = TSLanguage::from(rlang);

        let inner = lang.ast_grep(src);
        Self {
            inner,
            filename: "anonymous".into(),
        }
    }

    fn root(&self) -> SgNode {
        // let foo2 = &self.inner;
        // // let foo = unsafe { &*(&self.inner as *const AstGrep<_>) };
        // // let tree = foo as &'static AstGrep<_>;
        // let inner = NodeMatch::from(foo2.root());
        // SgNode {
        //     inner,
        //     root: self.clone(),
        // }
        let r = self.clone();
        let r2 = r.clone();
        let inner_clone = Box::new(r.inner.clone());
        let static_inner_clone = Box::leak(inner_clone);
        let node_root = static_inner_clone.root().clone();
        SgNode {
            inner: node_root.into(),
            root: r2,
        }
    }

    fn filename(&self) -> &str {
        &self.filename
    }
}

#[extendr]
fn test() {
    let r_lang = tree_sitter_r::language();
    let ast_r_lang = ast_grep_core::language::TSLanguage::from(r_lang);
    rprintln!("{:?}", ast_r_lang);
}
// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod astgrepr;
    // fn ast_grep_r;
    impl SgRoot;
    use node;
    use ser;
    // fn test;
}
