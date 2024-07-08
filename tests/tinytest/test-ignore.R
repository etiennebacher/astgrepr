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

# other tags to ignore -------------------------------

src <- "
# flint-ignore
any(duplicated(x))
# ast-grep-ignore
any(duplicated(y))
print(1)
"

root <- src |>
  tree_new(ignore_tags = "flint-ignore") |>
  tree_root()

expect_equal(
  root |>
    node_find(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text(),
  list(rule_1 = "any(duplicated(y))")
)

expect_equal(
  root |>
    node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
    node_text_all(),
  list(rule_1 = list(node_1 = "any(duplicated(y))"))
)


root <- src |>
  tree_new(ignore_tags = c("flint-ignore", "ast-grep-ignore")) |>
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

# ignore chunks of code -------------------------------

src <- "
# ast-grep-ignore-start
any(duplicated(x))
any(duplicated(foo))
# ast-grep-ignore-end
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


src <- "
# flint-ignore-start
any(duplicated(x))
any(duplicated(foo))
# flint-ignore-end
print(1)
"

root <- src |>
  tree_new(ignore_tags = "flint-ignore") |>
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


# ignore chunks of code: check mismatches -------------------------------

src <- "
# ast-grep-ignore-start
any(duplicated(x))
any(duplicated(foo))
print(1)
"

expect_error(
  src |> tree_new(),
  "Mismatch: the number of `start` patterns (1) and of `end` patterns (0) must be equal.",
  fixed = TRUE
)

src <- "
any(duplicated(x))
any(duplicated(foo))
# ast-grep-ignore-end
print(1)
"

expect_error(
  src |> tree_new(),
  "Mismatch: the number of `start` patterns (0) and of `end` patterns (1) must be equal.",
  fixed = TRUE
)

