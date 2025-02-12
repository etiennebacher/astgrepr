source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    # a comment
    #' a roxygen comment
    'plot(iris)'
    any(duplicated('x'))"

root <- src |>
  tree_new() |>
  tree_root()

expect_length(
  node_find(root, ast_rule(pattern = "any(duplicated($A))"))[[1]],
  1
)

expect_length(
  node_find(root, ast_rule(kind = "string"))[[1]],
  1
)

expect_length(
  node_find(root, ast_rule(kind = "comment"))[[1]],
  1
)

expect_length(
  node_find_all(root, ast_rule(kind = "comment"))[[1]],
  2
)

expect_length(
  node_find_all(root, ast_rule(pattern = "any(duplicated($A))"))[[1]],
  2
)

expect_equal(
  root |>
    node_find_all(ast_rule(pattern = "plot($A)", kind = "call")) |>
    node_text_all(),
  list(rule_1 = list(node_1 = "plot(mtcars)"))
)

# TODO: should work I guess?
# expect_length(
#   root |>
#     node_find_all(pattern = "any(duplicated($A))", kind = "string"),
#   1
# )

expect_equal(
  root |>
    node_find(ast_rule(pattern = "foobar")),
  list(rule_1 = NULL),
  check.attributes = FALSE
)

src <- "
mtcars$drat
mtcars$cyl
"

root <- src |>
  tree_new() |>
  tree_root()

expect_length(
  node_find_all(root, ast_rule(pattern = "mtcars$µVAR"))[[1]],
  2
)

expect_error(
  root |>
    node_find(
      ast_rule(id = "foo", pattern = "any(duplicated($A))"),
      ast_rule(id = "foo", pattern = "any(duplicated($A))"),
      ast_rule(id = "foo2", pattern = "any(duplicated($A))"),
      ast_rule(id = "foo2", pattern = "any(duplicated($A))")
    ),
  "Rule IDs must be unique. The following are duplicated: foo (2), foo2 (2).",
  fixed = TRUE
)

expect_error(
  root |>
    node_find_all(
      ast_rule(id = "foo", pattern = "any(duplicated($A))"),
      ast_rule(id = "foo", pattern = "any(duplicated($A))"),
      ast_rule(id = "foo2", pattern = "any(duplicated($A))"),
      ast_rule(id = "foo2", pattern = "any(duplicated($A))")
    ),
  "Rule IDs must be unique. The following are duplicated: foo (2), foo2 (2).",
  fixed = TRUE
)

# Weird case where having a trailing empty line or not influences the number of
# nodes found. Added a newline manually when creating the tree and removed it
# when rewriting the tree.

root <- "y <- 1\ny\nx <- 1\nx" |>
  tree_new() |>
  tree_root()

expect_length(
  node_find_all(root, files = file.path("examples-yaml/unused-object.yml"))[[
    1
  ]],
  2
)

root <- "y <- 1\ny\nx <- 1\nx\n" |>
  tree_new() |>
  tree_root()

expect_length(
  node_find_all(root, files = file.path("examples-yaml/unused-object.yml"))[[
    1
  ]],
  2
)
