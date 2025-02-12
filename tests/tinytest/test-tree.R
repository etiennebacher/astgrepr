source("helpers.R")

expect_error(
  tree_new(),
  "Must pass either `txt` or `file`."
)

expect_error(
  tree_new(1),
  "`txt` must a be a string of length 1."
)

expect_error(
  tree_new(c("a", "b")),
  "`txt` must a be a string of length 1."
)

expect_error(
  tree_new(TRUE),
  "`txt` must a be a string of length 1."
)

expect_error(
  tree_root("a"),
  "`x` must be an object of class 'SgRoot'."
)
