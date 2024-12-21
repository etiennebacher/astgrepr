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
		node_find(ast_rule(pattern = "any(duplicated($A))")) |>
		node_get_match("A") |>
		node_text(),
	list(rule_1 = "y")
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "any(duplicated($A))")) |>
		node_get_match("foobar") |>
		node_text(),
	list(rule_1 = NULL)
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "rnorm($A, $B)")) |>
		node_get_match("B") |>
		node_text(),
	list(rule_1 = "mean = 2")
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "rnorm($$$A)")) |>
		node_get_multiple_matches("A") |>
		node_text_all(),
	list(rule_1 = list("100", ",", "mean = 2"))
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "rnorm($A, $B)")) |>
		node_get_multiple_matches("foo"),
	list(rule_1 = list()),
	check.attributes = FALSE
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "rnorm($$$A)")) |>
		node_get_multiple_matches("foo") |>
		node_text_all(),
	list(rule_1 = list())
)

expect_equal(
	root |>
		node_find(ast_rule(pattern = "foobar")) |>
		node_text(),
	list(rule_1 = NULL)
)
