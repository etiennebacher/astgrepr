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
    node_get_match("A") |>
    node_text(),
  "y"
)

expect_equal(
  root |>
    node_find(pattern = "rnorm($A, $B)") |>
    node_get_match("B") |>
    node_text(),
  "mean = 2"
)

expect_equal(
  root |>
    node_find(pattern = "rnorm($$$A)") |>
    node_get_multiple_matches("A") |>
    node_text_all(),
  list("100", ",", "mean = 2")
)

expect_length(
  root |>
    node_find(pattern = "rnorm($A, $B)") |>
    node_get_multiple_matches("foo") ,
  0
)

expect_length(
  root |>
    node_find(pattern = "rnorm($$$A)") |>
    node_get_multiple_matches("foo") |>
    node_text_all(),
  0
)

# TODO: should return an empty list
expect_length(
  root |>
    node_find(pattern = "foobar") |>
    node_text(),
  0
)
