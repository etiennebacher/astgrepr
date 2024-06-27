source("helpers.R")

expect_error(
  ast_rule(),
  "requires at least one non-NULL parameter"
)
