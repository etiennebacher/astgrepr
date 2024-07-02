use crate::AstGrep;
use crate::StrDoc;
use ast_grep_core::meta_var::MetaVariable;
use std::borrow::Cow;
use std::iter::repeat;
pub use tree_sitter_facade_sg::Language as TSLanguage;

impl Language for TSLanguage {
    fn get_ts_language(&self) -> TSLanguage {
        self.clone()
    }
}

#[derive(Clone)]
pub struct R;
impl Language for R {
    fn get_ts_language(&self) -> TSLanguage {
        tree_sitter_r::language().into()
    }
    fn expando_char(&self) -> char {
        'λ'
    }
    fn meta_var_char(&self) -> char {
        '$'
    }
    fn pre_process_pattern<'q>(&self, query: &'q str) -> std::borrow::Cow<'q, str> {
        pre_process_pattern(self.expando_char(), query)
    }
}

fn pre_process_pattern(expando: char, query: &str) -> std::borrow::Cow<str> {
    let mut ret = Vec::with_capacity(query.len());
    let mut dollar_count = 0;
    for c in query.chars() {
        if c == '$' {
            dollar_count += 1;
            continue;
        }
        let need_replace = matches!(c, 'A'..='Z' | '_') // $A or $$A or $$$A
      || dollar_count == 3; // anonymous multiple
        let sigil = if need_replace { expando } else { '$' };
        ret.extend(repeat(sigil).take(dollar_count));
        dollar_count = 0;
        ret.push(c);
    }
    // trailing anonymous multiple
    let sigil = if dollar_count == 3 { expando } else { '$' };
    ret.extend(repeat(sigil).take(dollar_count));
    std::borrow::Cow::Owned(ret.into_iter().collect())
}
/// Trait to abstract ts-language usage in ast-grep, which includes:
/// * which character is used for meta variable.
/// * if we need to use other char in meta var for parser at runtime
/// * pre process the Pattern code.
pub trait Language: Clone {
    /// Create an [`AstGrep`] instance for the language
    fn ast_grep<S: AsRef<str>>(&self, source: S) -> AstGrep<StrDoc<TSLanguage>> {
        AstGrep::new(source, tree_sitter_r::language().into())
    }

    /// tree sitter language to parse the source
    fn get_ts_language(&self) -> TSLanguage;
    /// ignore trivial tokens in language matching
    fn skippable_kind_ids(&self) -> &'static [u16] {
        &[]
    }

    /// normalize pattern code before matching
    /// e.g. remove expression_statement, or prefer parsing {} to object over block
    fn pre_process_pattern<'q>(&self, query: &'q str) -> Cow<'q, str> {
        pre_process_pattern(self.expando_char(), query)
    }

    /// Configure meta variable special character
    /// By default $ is the metavar char, but in PHP it can be #
    #[inline]
    fn meta_var_char(&self) -> char {
        '$'
    }

    /// Some language does not accept $ as the leading char for identifiers.
    /// We need to change $ to other char at run-time to make parser happy, thus the name expando.
    /// By default this is the same as meta_var char so replacement is done at runtime.
    #[inline]
    fn expando_char(&self) -> char {
        self.meta_var_char()
    }

    // extract MetaVariable from a given source string
    // At runtime we need to use expand_char
    fn extract_meta_var(&self, source: &str) -> Option<MetaVariable> {
        ast_grep_core::meta_var::extract_meta_var(source, self.expando_char())
    }
}
