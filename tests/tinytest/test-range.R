source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    foo <- function() {
  print('hi there')
}
y <- rnorm(100, mean = 2)"

root <- src |>
  tree_new() |>
  tree_root()

# simple example

expect_equal(
  root |>
    node_find(list(
      pattern = "rnorm($$$A)"
    )) |>
    node_range(),
  list(start = c(0, 5), end = c(0, 25))
)

# multiple lines

expect_equal(
  root |>
    node_find(list(
      pattern = "{$$$A}"
    )) |>
    node_range(),
  list(start = c(1, 22), end = c(3, 1))
)

# require node_range_all()

expect_error(
  root |>
    node_find_all(list(
      pattern = "any(duplicated($A))"
    )) |>
    node_range(),
  "`x` must be an object of class 'SgNode'"
)

# multiple matches

expect_equal(
  root |>
    node_find_all(list(
      pattern = "rnorm($$$A)"
    )) |>
    node_range_all(),
  list(
    list(start = c(0, 5), end = c(0, 25)),
    list(start = c(4, 5), end = c(4, 25))
  )
)

# 0 matches

expect_length(
  root |>
    node_find_all(list(
      pattern = "foobar"
    )) |>
    node_range_all(),
  0
)
