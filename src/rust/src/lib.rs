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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Offset {
    char_offset: usize,
    byte_offset: usize,
    len: u8,
}

#[derive(Clone)]
pub struct UnicodePosition {
    offsets: Box<[Offset]>,
}

impl UnicodePosition {
    pub fn new(s: &str) -> Self {
        let mut byte_offset = 0;
        let mut offsets = Vec::new();
        for (char_offset, ch) in s.chars().enumerate() {
            let len = ch.len_utf8();
            if len > 1 {
                offsets.push(Offset {
                    char_offset,
                    byte_offset,
                    len: len as u8,
                });
            }
            byte_offset += len;
        }
        Self {
            offsets: offsets.into_boxed_slice(),
        }
    }

    pub fn char_to_byte(&self, char_offset: usize) -> usize {
        // use binary search to find the offsets
        let ret = self
            .offsets
            .binary_search_by(|o| o.char_offset.cmp(&char_offset));
        match ret {
            Ok(i) => self.offsets[i].byte_offset,
            Err(0) => char_offset,
            Err(i) => {
                let last = &self.offsets[i - 1];
                let end_offset = last.byte_offset + last.len as usize;
                let diff = char_offset - last.char_offset;
                end_offset + diff - 1
            }
        }
    }
    pub fn byte_to_char(&self, byte_offset: usize) -> usize {
        let ret = self
            .offsets
            .binary_search_by(|o| o.byte_offset.cmp(&byte_offset));
        match ret {
            Ok(i) => self.offsets[i].char_offset,
            Err(0) => byte_offset,
            Err(i) => {
                let last = &self.offsets[i - 1];
                let end_offset = last.byte_offset + last.len as usize;
                let diff = byte_offset - end_offset;
                last.char_offset + diff + 1
            }
        }
    }
}

#[derive(Clone)]
pub struct SgRoot {
    inner: AstGrep<StrDoc<SupportLang>>,
    filename: String,
    position: UnicodePosition,
}

#[extendr]
impl SgRoot {
    fn new(src: &str) -> Self {
        // let foo = tree_sitter_r::language();
        // let lang = ast_grep_core::language::TSLanguage::from(foo);
        let position = UnicodePosition::new(src);
        let lang = SupportLang::R;

        let inner = lang.ast_grep(src);
        Self {
            inner,
            filename: "anonymous".into(),
            position,
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
