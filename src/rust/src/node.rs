use crate::SgRoot;
use extendr_api::prelude::*;

use ast_grep_config::{DeserializeEnv, RuleCore, SerializableRuleCore};
use ast_grep_core::{NodeMatch, StrDoc};
use ast_grep_language::SupportLang;

pub struct Pos {
    /// line number starting from 0
    pub line: u32,
    /// column number starting from 0
    pub column: u32,
    /// byte offset of the position
    pub index: u32,
}

fn to_pos(pos: (usize, usize), offset: usize) -> Pos {
    Pos {
        line: pos.0 as u32,
        // In the pyo3 implementation they have u32 / 2 but for me that
        // doesn't work well
        column: pos.1 as u32,
        index: offset as u32 / 2,
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Edit {
    /// The start position of the edit in character
    pub start_pos: usize,
    /// The end position of the edit in character
    pub end_pos: usize,
    /// The text to be inserted
    pub inserted_text: String,
}

#[extendr]
pub struct SgNode {
    pub inner: NodeMatch<'static, StrDoc<SupportLang>>,
    // refcount SgRoot
    pub(crate) root: SgRoot,
}

// it is safe to send tree-sitter Node
// because it is refcnt and concurrency safe
// unsafe impl Send for SgNode {}

#[extendr]
impl SgNode {
    /*----------  Node Inspection ----------*/
    fn range(&self) -> List {
        let byte_range = self.inner.range();
        let start_pos = self.inner.start_pos();
        let end_pos = self.inner.end_pos();
        let positioner = &self.root.position;
        let start = positioner.byte_to_char(byte_range.start);
        let end = positioner.byte_to_char(byte_range.end);

        let start_pos = to_pos(start_pos, start);
        let end_pos = to_pos(end_pos, end);
        list!(
            vec![start_pos.line, start_pos.column],
            vec![end_pos.line, end_pos.column]
        )
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
    fn matches(&self, rule_params: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        self.inner.matches(matcher)
    }

    fn inside(&self, rule_params: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        self.inner.inside(matcher)
    }

    fn has(&self, rule_params: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        self.inner.has(matcher)
    }

    fn precedes(&self, rule_params: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        self.inner.precedes(matcher)
    }

    fn follows(&self, rule_params: List) -> bool {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        self.inner.follows(matcher)
    }

    fn get_match(&self, meta_var: &str) -> List {
        let inner = self
            .inner
            .get_env()
            .get_match(meta_var)
            .cloned()
            .map(|n| Self {
                inner: NodeMatch::from(n),
                root: self.root.clone(),
            });
        if inner.is_some() {
            list!(inner.unwrap())
        } else {
            List::new(0)
        }
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

    pub fn find(&self, rule_params: List) -> List {
        // let matcher2 = self.get_matcher(config, rule)?;
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        let inner = self.inner.find(matcher);
        if inner.is_some() {
            list!(Self {
                inner: inner.unwrap(),
                root: self.root.clone(),
            })
        } else {
            List::new(0)
        }
    }

    fn find_all(&self, rule_params: List) -> List {
        let matcher = get_matcher_from_rule(*self.inner.lang(), rule_params);
        self.inner
            .find_all(matcher)
            .map(|n| Self {
                inner: n,
                root: self.root.clone(),
            })
            .collect()
    }

    fn field(&self, name: &str) -> List {
        let inner = self.inner.field(name).map(|inner| Self {
            inner: inner.into(),
            root: self.root.clone(),
        });
        if inner.is_some() {
            list!(inner.unwrap())
        } else {
            List::new(0)
        }
    }

    fn parent(&self) -> List {
        let inner = self.inner.parent().map(|inner| Self {
            inner: inner.into(),
            root: self.root.clone(),
        });
        if inner.is_some() {
            list!(inner.unwrap())
        } else {
            List::new(0)
        }
    }

    fn child(&self, nth: Robj) -> List {
        let nth_usize = match nth.rtype() {
            Rtype::Doubles => nth.as_real().unwrap() as usize,
            Rtype::Integers => nth.as_integer().unwrap() as usize,
            _ => {
                panic!("foobar")
            }
        };
        let inner = self.inner.child(nth_usize).map(|inner| Self {
            inner: inner.into(),
            root: self.root.clone(),
        });
        if inner.is_some() {
            list!(inner.unwrap())
        } else {
            List::new(0)
        }
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

    fn next_(&self) -> List {
        let inner = self.inner.next().map(|inner| Self {
            inner: inner.into(),
            root: self.root.clone(),
        });
        if inner.is_some() {
            list!(inner.unwrap())
        } else {
            List::new(0)
        }
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

    fn prev(&self) -> List {
        let inner = self.inner.prev().map(|inner| Self {
            inner: inner.into(),
            root: self.root.clone(),
        });
        if inner.is_some() {
            list!(inner.unwrap())
        } else {
            List::new(0)
        }
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

    // fn get_matcher(&self, config: Option<List>, kwargs: Option<List>) -> RuleCore<SupportLang> {
    //     let lang = self.inner.lang();
    //     let config = if let Some(config) = config {
    //         config_from_dict(config)?
    //     } else if let Some(rule) = kwargs {
    //         config_from_rule(rule)?
    //     } else {
    //         return Err(extendr_api::Error::Other("rule must not be empty"));
    //     };
    //     let env = DeserializeEnv::new(*lang);
    //     let matcher = config.get_matcher(env).context("cannot get matcher")?;
    //     Ok(matcher)
    // }

    /*---------- Edit  ----------*/
    fn replace(&self, text: &str) -> List {
        let byte_range = self.inner.range();
        let root = &self.root;
        let start_pos = root.position.byte_to_char(byte_range.start);
        let end_pos = root.position.byte_to_char(byte_range.end);
        let edit = Edit {
            start_pos,
            end_pos,
            inserted_text: text.to_string(),
        };
        list!(edit.start_pos, edit.end_pos, edit.inserted_text)
    }

    fn commit_edits(&self, edits: List) -> String {
        let mut edits2 = edits
            .values()
            .into_iter()
            .map(|xi| Edit::from(xi.as_list().unwrap()))
            .collect::<Vec<Edit>>();
        edits2.sort_by_key(|edit| edit.start_pos);

        let mut new_content = String::new();
        let old_content = self.text();
        let root = &self.root;
        let conv = &root.position;
        let converted: Vec<_> = edits2
            .into_iter()
            .map(|mut e| {
                e.start_pos = conv.char_to_byte(e.start_pos);
                e.end_pos = conv.char_to_byte(e.end_pos);
                e
            })
            .collect();

        let offset = self.inner.range().start;
        let mut start = 0;
        for diff in converted {
            let pos = diff.start_pos - offset;
            // skip overlapping edits
            if start > pos {
                continue;
            }
            new_content.push_str(&old_content[start..pos]);
            new_content.push_str(&diff.inserted_text);
            start = diff.end_pos - offset;
        }
        // add trailing statements
        new_content.push_str(&old_content[start..]);
        new_content
    }
}

impl From<List> for Edit {
    fn from(input: List) -> Edit {
        Edit {
            start_pos: input.elt(0).unwrap().as_real().unwrap() as usize,
            end_pos: input.elt(1).unwrap().as_real().unwrap() as usize,
            inserted_text: input.elt(2).unwrap().as_str().unwrap().to_string(),
        }
    }
}

// fn config_from_dict(dict: Option<&str>) -> Result<SerializableRuleCore> {
//     use extendr_api::deserializer::from_robj;
//     Ok(SerializableRuleCore {
//         rule: dict.rule,
//         constraints: dict.constraints,
//         utils: dict.utils,
//         transform: dict.transform,
//         fix: dict.constraints,
//     })
// }

// fn config_from_rule(dict: Option<&str>) -> Result<SerializableRuleCore> {
//     Ok(SerializableRuleCore {
//         rule: dict.elt(0),
//         constraints: None,
//         utils: None,
//         transform: None,
//         fix: None,
//     })
// }

fn get_matcher_from_rule(lang: SupportLang, rule_params: List) -> RuleCore<SupportLang> {
    let rule = crate::ser::new_rule(rule_params);

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
