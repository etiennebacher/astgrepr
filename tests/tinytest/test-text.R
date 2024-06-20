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
    node_find(pattern = "any(duplicated($A))") |>
    node_text(),
  "any(duplicated(y))"
)

expect_error(
  root |>
    node_find_all(pattern = "any(duplicated($A))") |>
    node_text(),
  "`x` must be an object of class 'SgNode'"
)

expect_equal(
  root |>
    node_find_all(pattern = "any(duplicated($A))") |>
    node_text_all(),
  list("any(duplicated(y))", "any(duplicated(x))")
)

expect_length(
  root |>
    node_find(pattern = "foobar") |>
    node_text(),
  0
)
