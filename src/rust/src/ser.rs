use ast_grep_config::SerializableRule;
use extendr_api::prelude::*;

use extendr_api::deserializer::from_robj;

pub fn new_rule(rule_params: List) -> SerializableRule {
    let rule_params = rule_params.into_hashmap();
    let mut ruleset = SerializableRule::default();

    // TODO: how can I generalize that? Ideally I would do
    // add_if_present("pattern", rule_params, ruleset) but then I'd need
    // something like ruleset["pattern"] = ...
    let foo = &rule_params.get(&"pattern").unwrap();
    if foo.len() > 0 {
        ruleset.pattern = from_robj(foo).unwrap();
    }

    let foo = &rule_params.get(&"kind").unwrap();
    if foo.len() > 0 {
        ruleset.kind = from_robj(foo).unwrap();
    }

    // ruleset.regex = from_robj(&rule_params.get(&"regex").unwrap()).unwrap();
    // ruleset.inside = from_robj(&rule_params.get(&"inside").unwrap()).unwrap();
    // ruleset.has = from_robj(&rule_params.get(&"has").unwrap()).unwrap();
    // ruleset.precedes = from_robj(&rule_params.get(&"precedes").unwrap()).unwrap();
    // ruleset.follows = from_robj(&rule_params.get(&"follows").unwrap()).unwrap();
    // ruleset.all = from_robj(&rule_params.get(&"all").unwrap()).unwrap();
    // ruleset.any = from_robj(&rule_params.get(&"any").unwrap()).unwrap();
    // ruleset.not = from_robj(&rule_params.get(&"not").unwrap()).unwrap();
    // ruleset.matches = from_robj(&rule_params.get(&"matches").unwrap()).unwrap();

    ruleset
}

extendr_module! {
    mod ser;
}
