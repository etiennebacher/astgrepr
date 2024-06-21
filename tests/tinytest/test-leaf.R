source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    x <- z + 1
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

expect_false(node_is_leaf(root))

expect_true(
  root |>
    node_find(pattern = "z") |>
    node_is_leaf()
)

# TODO: am I sure about this?
expect_true(
  root |>
    node_find(pattern = "z") |>
    node_is_named()
)
