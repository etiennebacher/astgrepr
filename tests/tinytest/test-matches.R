source("helpers.R")

src <- "print(\"hi\")"

root <- src |>
  tree_new() |>
  tree_root()

# TODO: this should be kind = "string" but in the tree-sitter-r grammar the
# argument contains "name" and "value" so only the "value" is of kind "string"
# https://github.com/r-lib/tree-sitter-r/blob/main/src/node-types.json

# TODO: should one be able to compare several rules to several other rules?
# expect_equal(
#   root |>
#     node_find(ast_rule(pattern = "print($A)")) |>
#     node_get_match("A") |>
#     node_matches(ast_rule(kind = "argument")),
#   list(rule_1 = TRUE)
# )
