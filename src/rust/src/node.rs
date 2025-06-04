use crate::range::Range;
use crate::SgRoot;
use extendr_api::prelude::*;

use ast_grep_config::{DeserializeEnv, RuleCore, SerializableRuleCore};
use ast_grep_core::{NodeMatch, StrDoc};

pub struct Pos {
    /// line number starting from 0
    pub line: u32,
    /// column number starting from 0
    pub column: u32,
    /// byte offset of the position
    pub index: u32,
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
    pub inner: NodeMatch<'static, StrDoc<crate::language::R>>,
    // refcount SgRoot
    pub(crate) root: SgRoot,
}

// it is safe to send tree-sitter Node
// because it is refcnt and concurrency safe
unsafe impl Send for SgNode {}

#[extendr]
impl SgNode {
    /*----------  Node Inspection ----------*/
    fn range(&self) -> List {
        let range = Range::from(&self.inner, &self.root.position);
        list!(
            vec![range.start.line, range.start.column],
            vec![range.end.line, range.end.column]
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
    fn matches(&self, rule: &str) -> bool {
        let matcher = get_matcher_from_rule(self.inner.lang().clone(), rule);
        self.inner.matches(matcher)
    }

    fn inside(&self, rule: &str) -> bool {
        let matcher = get_matcher_from_rule(self.inner.lang().clone(), rule);
        self.inner.inside(matcher)
    }

    fn has(&self, rule: &str) -> bool {
        let matcher = get_matcher_from_rule(self.inner.lang().clone(), rule);
        self.inner.has(matcher)
    }

    fn precedes(&self, rule: &str) -> bool {
        let matcher = get_matcher_from_rule(self.inner.lang().clone(), rule);
        self.inner.precedes(matcher)
    }

    fn follows(&self, rule: &str) -> bool {
        let matcher = get_matcher_from_rule(self.inner.lang().clone(), rule);
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

    pub fn find(&self, rule: &str) -> List {
        // let matcher2 = self.get_matcher(config, rule)?;
        let matcher = get_matcher_from_rule(self.inner.lang().clone(), rule);
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

    fn find_all(&self, rule: Strings) -> List {
        let list_matchers = rule
            .iter()
            .map(|xi| get_matcher_from_rule(self.inner.lang().clone(), xi))
            .collect::<Vec<RuleCore<crate::language::R>>>();

        list_matchers
            .iter()
            .map(|xi| {
                self.inner
                    .find_all(xi)
                    .map(|n| {
                        list!(Self {
                            inner: n,
                            root: self.root.clone(),
                        })
                    })
                    .collect::<List>()
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

    fn commit_edits(&self, edits: List) -> List {
        let mut edits2 = edits
            .values()
            .map(|xi| Edit::from(xi.as_list().unwrap()))
            .collect::<Vec<Edit>>();

        edits2.sort_by_key(|edit| edit.start_pos);

        let old_content = self.text();
        let mut new_content = old_content.clone();
        let root = &self.root;
        let conv = &root.position;
        let mut has_skipped_fixes = false;
        let converted: Vec<_> = edits2
            .into_iter()
            .map(|mut e| {
                e.start_pos = conv.char_to_byte(e.start_pos);
                e.end_pos = conv.char_to_byte(e.end_pos);
                e
            })
            .collect();

        let mut last_position_modified = 0;
        let offset = self.inner.range().start;

        let old_length = old_content.chars().count() as i32;
        let mut new_length = old_length;

        for diff in converted {
            if diff.start_pos < last_position_modified {
                if !has_skipped_fixes {
                    has_skipped_fixes = true;
                }
                continue;
            }

            let mut start: i32 = (diff.start_pos - offset).try_into().unwrap();
            let mut end: i32 = (diff.end_pos - offset).try_into().unwrap();

            let diff_length = new_length - old_length;

            if diff_length != 0 {
                start += diff_length;
                end += diff_length;
            }

            let start_usize = start as usize;
            let end_usize = end as usize;

            new_content.replace_range(start_usize..end_usize, &diff.inserted_text);
            new_length = new_content.chars().count() as i32;
            last_position_modified = diff.end_pos;
        }

        list!(
            new_content = new_content,
            has_skipped_fixes = has_skipped_fixes
        )
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

fn get_matcher_from_rule(lang: crate::language::R, rule: &str) -> RuleCore<crate::language::R> {
    let rule_core: SerializableRuleCore =
        ast_grep_config::from_str(rule).expect("cannot parse rule");
    let env = DeserializeEnv::new(lang);
    rule_core.get_matcher(env).unwrap()
}

extendr_module! {
    mod node;
    impl SgNode;
}
