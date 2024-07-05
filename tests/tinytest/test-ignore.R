source("helpers.R")

# one match -------------------------------

src <- "
# ast-grep-ignore
any(duplicated(x))
print(1)
"

root <- src |>
  tree_new() |>
  tree_root()

expect_equal(
  root |>
    node_find(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text(),
  list(rule_1 = NULL)
)

expect_equal(
  root |>
    node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text_all(),
  list(rule_1 = list())
)

# multiple matches -------------------------------

src <- "
# ast-grep-ignore
any(duplicated(x))
any(duplicated(foo))
print(1)
"

root <- src |>
  tree_new() |>
  tree_root()

expect_equal(
  root |>
    node_find(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text(),
  list(rule_1 = "any(duplicated(foo))")
)

expect_equal(
  root |>
    node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text_all(),
  list(rule_1 = list(node_1 = "any(duplicated(foo))"))
)


# more nested pattern -------------------------------

src <- "
# ast-grep-ignore
if (any(is.na(x))) {
  any(duplicated(y))
}
print(1)
"

root <- src |>
  tree_new() |>
  tree_root()

expect_equal(
  root |>
    node_find(ast_rule(pattern = "any(is.na($A))")) |>
    node_text(),
  list(rule_1 = NULL)
)

expect_equal(
  root |>
    node_find(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text(),
  list(rule_1 = "any(duplicated(y))")
)
