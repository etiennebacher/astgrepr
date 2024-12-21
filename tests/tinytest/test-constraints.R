source("helpers.R")

# one constraint ----------------------------------------------------

temp_rule <- tempfile(fileext = ".yml")
cat(
	"rule:
  pattern: print($VAR)
constraints:
  VAR:
    has:
      kind: identifier
",
	file = temp_rule
)

src <- "
print(1)
print(a)
print(x)
"

root <- src |>
	tree_new() |>
	tree_root()

expect_equal(
	root |>
		node_find(files = temp_rule) |>
		node_text(),
	list(rule_1 = "print(a)")
)

expect_equal(
	root |>
		node_find_all(files = temp_rule) |>
		node_text_all(),
	list(rule_1 = list(node_1 = "print(a)", node_2 = "print(x)"))
)

# several constraints ----------------------------------------------------

temp_rule <- tempfile(fileext = ".yml")
cat(
	"rule:
  pattern: $VAR + $FLOAT
constraints:
  VAR:
    kind: identifier
  FLOAT:
    kind: float
",
	file = temp_rule
)

src <- "
1 + 1
x+2
y+a
y + 1
"

root <- src |>
	tree_new() |>
	tree_root()

expect_equal(
	root |>
		node_find(files = temp_rule) |>
		node_text(),
	list(rule_1 = "x+2")
)

expect_equal(
	root |>
		node_find_all(files = temp_rule) |>
		node_text_all(),
	list(rule_1 = list(node_1 = "x+2", node_2 = "y + 1"))
)

# several rules and several constraints ------------------------------------------

temp_rule <- tempfile(fileext = ".yml")
cat(
	"rule:
  pattern: $VAR + $FLOAT
constraints:
  VAR:
    kind: identifier
  FLOAT:
    kind: float
",
	file = temp_rule
)

cat(
	"
---

rule:
  pattern: any(duplicated($VAR))
message: foo
",
	file = temp_rule,
	append = TRUE
)

src <- "
1 + 1
x+2
y+a
y + 1
any(duplicated(x))
"

root <- src |>
	tree_new() |>
	tree_root()

expect_equal(
	root |>
		node_find(files = temp_rule) |>
		node_text(),
	list(rule_1 = "x+2", rule_2 = "any(duplicated(x))")
)

expect_equal(
	root |>
		node_find_all(files = temp_rule) |>
		node_text_all(),
	list(
		rule_1 = list(node_1 = "x+2", node_2 = "y + 1"),
		rule_2 = list(node_1 = "any(duplicated(x))")
	)
)
