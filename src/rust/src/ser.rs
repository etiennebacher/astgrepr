use ast_grep_config::SerializableRule;
use extendr_api::prelude::*;

pub fn new_rule(rule: &str) -> SerializableRule {
    ast_grep_config::from_str(rule).expect("cannot parse rule")
    // let env = DeserializeEnv::new(TypeScript::Tsx);
    // let rule = deserialize_rule(rule, &env).expect("should deserialize");
}

extendr_module! {
    mod ser;
}
