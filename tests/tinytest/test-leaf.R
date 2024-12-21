source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    x <- z + 1
    any(duplicated(x))"

root <- src |>
	tree_new() |>
	tree_root()

expect_false(node_is_leaf(root)[[1]])

expect_equal(
	root |>
		node_find(ast_rule(pattern = "z")) |>
		node_is_leaf(),
	list(rule_1 = TRUE)
)

# TODO: am I sure about this?
expect_equal(
	root |>
		node_find(ast_rule(pattern = "z")) |>
		node_is_named(),
	list(rule_1 = TRUE)
)
