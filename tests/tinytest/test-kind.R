source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    x <- z + 1
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

expect_equal(
  root |>
    node_find(pattern = "any(duplicated($VAR))") |>
    node_kind(),
  "call"
)

expect_equal(
  root |>
    node_find(pattern = "$X + $VALUE") |>
    node_kind(),
  "binary_operator"
)

expect_error(
  root |>
    node_find(pattern = "foobar") |>
    node_kind(),
  "must be an object of class 'SgNode'"
)
