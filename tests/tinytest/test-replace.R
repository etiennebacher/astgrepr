source("helpers.R")
using("astgrepr")

src <- "x <- c(1, 2, 3)
any(duplicated(x), na.rm = TRUE)

any(duplicated(x))
if (any(is.na(x))) {
  TRUE
}
any(is.na(y))
"

root <- tree_new(src) |>
	tree_root()

# one replacement ------------------------------------------

nodes_to_replace <- root |>
	node_find(
		ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
		ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
	)

fixes <- nodes_to_replace |>
	node_replace(
		any_na = "anyNA(~~VAR~~)",
		any_dup = "anyDuplicated(~~VAR~~) > 0"
	)

expect_snapshot(
	"rewrite_several_rules_one_node",
	tree_rewrite(root, fixes)
)

# several replacements ------------------------------------------

nodes_to_replace <- root |>
	node_find_all(
		ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
		ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
	)

fixes <- nodes_to_replace |>
	node_replace_all(
		any_na = "anyNA(~~VAR~~)",
		any_dup = "anyDuplicated(~~VAR~~) > 0"
	)

expect_snapshot(
	"rewrite_several_rules_several_nodes",
	tree_rewrite(root, fixes)
)

# with expando ------------------------------------------

src <- "
iris$Species
mtcars$drat
"

root <- src |>
	tree_new() |>
	tree_root()

fixes <- root |>
	node_find(
		ast_rule(id = "foo", pattern = "$DATA$µVAR")
	) |>
	node_replace(
		foo = "~~DATA~~[[\"~~VAR~~\"]]"
	)

expect_snapshot(
	"rewrite_with_expando",
	tree_rewrite(root, fixes)
)

fixes <- root |>
	node_find_all(
		ast_rule(id = "foo", pattern = "$DATA$µVAR")
	) |>
	node_replace_all(
		foo = "~~DATA~~[[\"~~VAR~~\"]]"
	)

expect_snapshot(
	"rewrite_with_expando_several_replacements",
	tree_rewrite(root, fixes)
)

# with replace multi meta-var ------------------------------------------

src <- "paste0('a', 'b', sep = '')"

root <- src |>
	tree_new() |>
	tree_root()

nodes_to_replace <- root |>
	node_find(ast_rule(id = "foo", pattern = "paste0($$$A sep = '')"))

fixes <- nodes_to_replace |>
	node_replace(
		foo = "paste0(~~A~~)"
	)

expect_snapshot(
	"rewrite_multi_meta_var",
	tree_rewrite(root, fixes)
)

# ensure digits in metavar work ------------------------------------------

src <- "length(x == 0)"

root <- src |>
	tree_new() |>
	tree_root()

nodes_to_replace <- root |>
	node_find(
		ast_rule(id = "foo", pattern = "length($VAR == $VAR2)")
	)

fixes <- nodes_to_replace |>
	node_replace(
		foo = "length(~~VAR~~) == ~~VAR2~~"
	)

expect_snapshot(
	"rewrite_digit_in_metavar",
	tree_rewrite(root, fixes)
)

# check that providing an input with escaped characters (e.g regex) doesn't
# remove those characters ------------------------------------------

src <- r'(
lab = gsub("\\s+", "", lab)
drat = any(is.na(gsub("\\d{1,}\\w\\s+(.*)", "\\1", mpg)))
)'

root <- src |>
	tree_new() |>
	tree_root()

nodes_to_replace <- root |>
	node_find(
		ast_rule(id = "foo", pattern = "$A = $B")
	)

fixes <- nodes_to_replace |>
	node_replace(
		foo = "~~A~~ <- ~~B~~"
	)

expect_snapshot(
	"rewrite_escaped_chars",
	tree_rewrite(root, fixes)
)

# several replacements on the same line ---------------------------------------

src <- r'(
lab <- T; foo <- any(is.na(x))
any(duplicated(gsub("\\d{1,}\\w\\s+(.*)", "\\1", mpg)));a <- T
)'

root <- src |>
	tree_new() |>
	tree_root()

nodes_to_replace <- root |>
	node_find_all(
		ast_rule(id = "foo", pattern = "$A <- T"),
		ast_rule(id = "foo2", pattern = "any(duplicated($VAR))"),
		ast_rule(id = "foo3", pattern = "any(is.na($VAR))")
	)

fixes <- nodes_to_replace |>
	node_replace_all(
		foo = "~~A~~ <- TRUE",
		foo2 = "anyDuplicated(~~VAR~~) > 0",
		foo3 = "anyNA(~~VAR~~)"
	)

expect_snapshot(
	"rewrite_one_line_several_replacements",
	tree_rewrite(root, fixes)
)

# error when replace uses meta var that don't exist in rule
# # ---------------------------------------

src <- "a = 1"

root <- src |>
	tree_new() |>
	tree_root()

expect_error(
	root |>
		node_find(
			ast_rule(id = "foo", pattern = "$A = $B")
		) |>
		node_replace(
			foo = "~~NOTEXIST~~ <- ~~B~~"
		),
	"Couldn't get value for meta-variable `NOTEXIST`"
)

expect_error(
	root |>
		node_find_all(
			ast_rule(id = "foo", pattern = "$A = $B")
		) |>
		node_replace_all(
			foo = "~~NOTEXIST~~ <- ~~B~~"
		),
	"Couldn't get value for meta-variable `NOTEXIST`"
)

# replace multi-metavar

src <- "warning(paste('hi', 'there'))"

root <- src |>
	tree_new() |>
	tree_root()

nodes_to_replace <- root |>
	node_find_all(
		ast_rule(id = "foo", pattern = "warning($$$ paste($$$MSG) $$$)")
	)

fixes <- nodes_to_replace |>
	node_replace_all(
		foo = "warning(~~MSG~~)"
	)

expect_snapshot(
	"rewrite_multi_metavar",
	tree_rewrite(root, fixes)
)
