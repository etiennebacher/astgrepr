use ast_grep_core::language::*;
use std::iter::repeat;

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
