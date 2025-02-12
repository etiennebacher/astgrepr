source("helpers.R")

src <- "
print('hi')
fn <- function() {
  print('hello')
}
"
root <- src |>
  tree_new() |>
  tree_root()

expect_equal(
  root |>
    node_find(ast_rule("print($A)")) |>
    node_get_root() |>
    tree_root() |>
    node_text(),
  root |>
    node_text()
)
