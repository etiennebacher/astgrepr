source("helpers.R")

# one metavar ---------------------------------------------

temp_rule <- tempfile(fileext = ".yml")
cat(
	"rule:
  any:
    - pattern: $FUN(x, 1, 0)
constraints:
  FUN:
    regex: ^(ifelse|fifelse|if_else)$
message: Prefer as.integer(x) to ~~FUN~~(x, 1L, 0L) if really needed.
",
	file = temp_rule
)

src <- "ifelse(x, 1, 0)"

root <- src |>
	tree_new() |>
	tree_root()

res <- attributes(node_find_all(root, files = temp_rule)[[1]])

expect_equal(
	res$other_info$message,
	"Prefer as.integer(x) to ifelse(x, 1L, 0L) if really needed.",
	fixed = TRUE
)

src <- "fifelse(x, 1, 0)"

root <- src |>
	tree_new() |>
	tree_root()

res <- attributes(node_find_all(root, files = temp_rule)[[1]])

expect_equal(
	res$other_info$message,
	"Prefer as.integer(x) to fifelse(x, 1L, 0L) if really needed.",
	fixed = TRUE
)

# several metavars ---------------------------------------------

temp_rule <- tempfile(fileext = ".yml")
cat(
	"rule:
  any:
    - pattern: $FUN($COND, 1, 0)
constraints:
  FUN:
    regex: ^(ifelse|fifelse|if_else)$
message: Prefer as.integer(~~COND~~) to ~~FUN~~(~~COND~~, 1L, 0L) if really needed.
",
	file = temp_rule
)

src <- "ifelse(x > 2, 1, 0)"

root <- src |>
	tree_new() |>
	tree_root()

res <- attributes(node_find_all(root, files = temp_rule)[[1]])

expect_equal(
	res$other_info$message,
	"Prefer as.integer(x > 2) to ifelse(x > 2, 1L, 0L) if really needed.",
	fixed = TRUE
)

src <- "fifelse(x > 2, 1, 0)"

root <- src |>
	tree_new() |>
	tree_root()

res <- attributes(node_find_all(root, files = temp_rule)[[1]])

expect_equal(
	res$other_info$message,
	"Prefer as.integer(x > 2) to fifelse(x > 2, 1L, 0L) if really needed.",
	fixed = TRUE
)
