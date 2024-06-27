source("helpers.R")
using("astgrepr")

expect_error(
  ast_rule(),
  "requires at least one non-NULL parameter"
)

expect_snapshot(
  "ast_rule_basic_pattern",
  ast_rule(pattern = "print($A)")
)

expect_snapshot(
  "ast_rule_basic_kind",
  ast_rule(kind = "field_definition")
)

expect_snapshot(
  "ast_rule_basic_regex",
  ast_rule(regex = "\\w+")
)

expect_snapshot(
  "ast_rule_with_pattern_rule",
  ast_rule(
    pattern = pattern_rule(
      selector = "field_definition",
      context = "class A { $FIELD = $INIT }"
    )
  )
)

expect_snapshot(
  "ast_rule_with_pattern_and_relational_rule",
  ast_rule(
    pattern = "any(duplicated($VAR))",
    inside = relational_rule(
        field = "if_statement"
    )
  )
)


