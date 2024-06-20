source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated('x'))"

root <- src |>
  tree_new() |>
  tree_root()

expect_length(
  root |>
    node_find(pattern = "any(duplicated($A))"),
  1
)

expect_length(
  root |>
    node_find(kind = "string"),
  1
)

expect_length(
  root |>
    node_find_all(pattern = "any(duplicated($A))"),
  2
)

# TODO: should work I guess?
# expect_length(
#   root |>
#     node_find_all(pattern = "any(duplicated($A))", kind = "string"),
#   1
# )

# TODO: should return an empty list
# expect_length(
#   root |>
#     node_find(#       pattern = "foobar"     )),
#   0
# )
