source("helpers.R")

src <- "x <- rnorm(100, mean = 2)
any(duplicated(y))
plot(mtcars)
any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

# one replacement ------------------------------------------

node_to_fix <- root |>
  node_find(pattern = "any(duplicated($A))")

fix <- node_to_fix |>
  node_replace(
    paste0("anyDuplicated(", node_text(node_get_match(node_to_fix, "A")), ") > 0")
  )

expect_snapshot(
  "one_fix",
  node_commit_edits(root, fix)
)


# several replacements ------------------------------------------

nodes_to_fix <- root |>
  node_find_all(pattern = "any(duplicated($A))")

fixes <- nodes_to_fix |>
  node_replace_all(
    paste0("anyDuplicated(", node_text_all(node_get_multiple_matches(nodes_to_fix, "A")), ") > 0")
  )

### TODO : wrong, should be "x" in the second replacement

expect_snapshot(
  "several_fixes",
  node_commit_edits(root, fixes)
)
