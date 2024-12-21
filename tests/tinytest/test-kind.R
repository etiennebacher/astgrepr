source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    x <- z + 1
    any(duplicated(x))"

root <- src |>
	tree_new() |>
	tree_root()

expect_equal(
	root |>
		node_find(ast_rule(pattern = "any(duplicated($VAR))")) |>
		node_kind(),
	list(rule_1 = "call")
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "$X + $VALUE")) |>
		node_kind(),
	list(rule_1 = "binary_operator")
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "foobar")) |>
		node_kind(),
	list(rule_1 = NULL)
)
