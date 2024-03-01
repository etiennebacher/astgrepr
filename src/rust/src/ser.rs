use ast_grep_config::SerializableRule;
use extendr_api::prelude::*;

use extendr_api::deserializer::from_robj;

pub fn new_rule(pattern: Robj) -> SerializableRule {
    let r: SerializableRule = from_robj(&pattern).unwrap();
    r
}

extendr_module! {
    mod ser;
}
