use extendr_api::prelude::*;
pub mod node;
mod range;
pub mod ser;
use node::SgNode;
// use range::{Pos, Range};
use ast_grep_core::language::TSLanguage;
use ast_grep_core::{AstGrep, Language, NodeMatch, StrDoc};
use ast_grep_language::SupportLang;
use std::collections::HashMap;

#[derive(Clone)]
pub struct SgRoot {
    inner: AstGrep<StrDoc<SupportLang>>,
    filename: String,
}

#[extendr]
impl SgRoot {
    fn new(src: &str) -> Self {
        let lang = SupportLang::from("R".parse().unwrap());

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

#[extendr]
fn test() {
    let r_lang: TSLanguage = tree_sitter_r::language().into();
    let ast_r_lang = ast_grep_core::language::TSLanguage::from(r_lang);
    rprintln!("{:?}", ast_r_lang);
}

#[extendr]
fn test2(pattern: Robj) -> SgNode {
    let mut map: HashMap<&str, Robj> = HashMap::new();
    map.insert("pattern", pattern);
    let input = List::from_hashmap(map).unwrap();
    SgRoot::new("plot(iris)").root().find(input)
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
    fn test;
    fn test2;
}
