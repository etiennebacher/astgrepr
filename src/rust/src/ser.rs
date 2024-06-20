use ast_grep_config::SerializableRule;
use extendr_api::prelude::*;

use extendr_api::deserializer::from_robj;

pub fn new_rule(rule_params: List) -> SerializableRule {
    let rule_params = rule_params.into_hashmap();
    let mut ruleset = SerializableRule::default();
    ruleset.pattern = from_robj(&rule_params.get(&"pattern").unwrap()).unwrap();
    ruleset.kind = from_robj(&rule_params.get(&"kind").unwrap()).unwrap();
    ruleset.regex = from_robj(&rule_params.get(&"regex").unwrap()).unwrap();
    ruleset.inside = from_robj(&rule_params.get(&"inside").unwrap()).unwrap();
    ruleset.has = from_robj(&rule_params.get(&"has").unwrap()).unwrap();
    ruleset.precedes = from_robj(&rule_params.get(&"precedes").unwrap()).unwrap();
    ruleset.follows = from_robj(&rule_params.get(&"follows").unwrap()).unwrap();
    ruleset.all = from_robj(&rule_params.get(&"all").unwrap()).unwrap();
    ruleset.any = from_robj(&rule_params.get(&"any").unwrap()).unwrap();
    ruleset.not = from_robj(&rule_params.get(&"not").unwrap()).unwrap();
    ruleset.matches = from_robj(&rule_params.get(&"matches").unwrap()).unwrap();

    // let r = SerializableRule {
    //     pattern: from_robj(&pattern).unwrap(),
    //     kind: from_robj(&kind).unwrap(),
    //     regex: from_robj(&regex).unwrap(),
    //     inside: from_robj(&inside).unwrap(),
    //     has: from_robj(&has).unwrap(),
    //     precedes: from_robj(&precedes).unwrap(),
    //     follows: from_robj(&follows).unwrap(),
    //     all: from_robj(&all).unwrap(),
    //     any: from_robj(&any).unwrap(),
    //     not: from_robj(&not).unwrap(),
    //     matches: from_robj(&matches).unwrap(),
    //     nth_child: ,
    // };
    ruleset
}

extendr_module! {
    mod ser;
}
