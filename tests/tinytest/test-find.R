source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

expect_length(
  root |>
    node_find(list(
      pattern = "any(duplicated($A))"
    )),
  1
)

expect_length(
  root |>
    node_find_all(list(
      pattern = "any(duplicated($A))"
    )),
  2
)

# TODO: should return an empty list
# expect_length(
#   root |>
#     node_find(list(
#       pattern = "foobar"
#     )),
#   0
# )
