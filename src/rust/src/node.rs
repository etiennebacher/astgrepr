use crate::range::Range;
use crate::SgRoot;
use extendr_api::prelude::*;

use ast_grep_config::{DeserializeEnv, RuleCore, SerializableRuleCore};
use ast_grep_core::language::TSLanguage;
use ast_grep_core::{NodeMatch, StrDoc};
use ast_grep_language::SupportLang;

#[extendr]
pub struct SgNode {
    pub inner: NodeMatch<'static, StrDoc<SupportLang>>,
    // refcount SgRoot
    pub(crate) root: SgRoot,
}

// it is safe to send tree-sitter Node
// because it is refcnt and concurrency safe
unsafe impl Send for SgNode {}

#[extendr]
impl SgNode {
    /*----------  Node Inspection ----------*/
    fn range(&self) -> Range {
        Range::from(&self.inner)
    }

    fn is_leaf(&self) -> bool {
        self.inner.is_leaf()
    }

    fn is_named(&self) -> bool {
        self.inner.is_named()
    }

    fn is_named_leaf(&self) -> bool {
        self.inner.is_named_leaf()
    }

    fn kind(&self) -> String {
        self.inner.kind().to_string()
    }

    fn text(&self) -> String {
        self.inner.text().to_string()
    }

    /*---------- Search Refinement  ----------*/
    fn matches(&self, rule: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        self.inner.matches(matcher)
    }

    fn inside(&self, rule: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        self.inner.inside(matcher)
    }

    fn has(&self, rule: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        self.inner.has(matcher)
    }

    fn precedes(&self, rule: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        self.inner.precedes(matcher)
    }

    fn follows(&self, rule: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        self.inner.follows(matcher)
    }

    fn get_match(&self, meta_var: &str) -> Self {
        self.inner
            .get_env()
            .get_match(meta_var)
            .cloned()
            .map(|n| Self {
                inner: NodeMatch::from(n),
                root: self.root.clone(),
            })
            .unwrap()
    }

    fn get_multiple_matches(&self, meta_var: &str) -> List {
        self.inner
            .get_env()
            .get_multiple_matches(meta_var)
            .into_iter()
            .map(|n| Self {
                inner: NodeMatch::from(n),
                root: self.root.clone(),
            })
            .collect()
    }

    fn get_transformed(&self, meta_var: &str) -> Option<String> {
        self.inner
            .get_env()
            .get_transformed(meta_var)
            .map(|n| String::from_utf8_lossy(n).to_string())
    }

    // /*---------- Tree Traversal  ----------*/
    fn get_root(&self) -> SgRoot {
        self.root.clone()
    }

    pub fn find(&self, rule: List) -> SgNode {
        rprintln!("hi there");
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        rprintln!("second hi");
        let inner = self.inner.find(matcher);
        rprintln!("third hi");
        let inner2 = inner.unwrap();
        Self {
            inner: inner2,
            root: self.root.clone(),
        }
    }

    fn find_all(&self, rule: List) -> List {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule);
        self.inner
            .find_all(matcher)
            .map(|n| Self {
                inner: n,
                root: self.root.clone(),
            })
            .collect()
    }

    fn field(&self, name: &str) -> SgNode {
        self.inner
            .field(name)
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .unwrap()
    }

    fn parent(&self) -> SgNode {
        self.inner
            .parent()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .unwrap()
    }

    fn child(&self, nth: Robj) -> SgNode {
        if nth.len() > 1 {
            panic!("foobar")
        }
        let nth_usize = match nth.rtype() {
            Rtype::Doubles => nth.as_real().unwrap() as usize,
            Rtype::Integers => nth.as_integer().unwrap() as usize,
            _ => {
                panic!("foobar")
            }
        };
        self.inner
            .child(nth_usize)
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .unwrap()
    }

    fn ancestors(&self) -> List {
        self.inner
            .ancestors()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .collect()
    }

    fn children(&self) -> List {
        self.inner
            .children()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .collect()
    }

    fn next_(&self) -> SgNode {
        self.inner
            .next()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .unwrap()
    }

    fn next_all(&self) -> List {
        self.inner
            .next_all()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .collect()
    }

    fn prev(&self) -> SgNode {
        self.inner
            .prev()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .unwrap()
    }

    fn prev_all(&self) -> List {
        self.inner
            .prev_all()
            .map(|inner| Self {
                inner: inner.into(),
                root: self.root.clone(),
            })
            .collect()
    }
}

fn get_matcher_from_rule(lang: SupportLang, patterns: List) -> RuleCore<SupportLang> {
    let rule = crate::ser::new_rule(patterns.into());
    let rule_core = SerializableRuleCore {
        rule,
        constraints: None,
        utils: None,
        transform: None,
        fix: None,
    };

    let env = DeserializeEnv::new(lang);
    rule_core.get_matcher(env).unwrap()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod node;
    // fn ast_grep_r;
    impl SgNode;
}
