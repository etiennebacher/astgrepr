source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

expect_equal(
  root |>
    node_find(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text(),
  list(rule_1 = "any(duplicated(y))")
)

expect_error(
  root |>
    node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text(),
  "`x` must be an object of class 'RuleList' or 'SgNode'"
)

expect_equal(
  root |>
    node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text_all(),
  list(
    rule_1 = list(node_1 = "any(duplicated(y))", node_2 = "any(duplicated(x))")
  )
)

expect_equal(
  root |>
    node_find(ast_rule(pattern = "foobar")) |>
    node_text(),
  list(rule_1 = NULL)
)
