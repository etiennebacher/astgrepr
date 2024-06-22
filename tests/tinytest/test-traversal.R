source("helpers.R")

### get the previous/next node ---------------------------

src <- "
print('hi there')
a <- 1
fn <- function(x) {
  x + 1
}
"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(pattern = "a <- $A")

root |>
  node_find(pattern = "a <- $A") |>
  node_prev()

root |>
  node_find(pattern = "a <- $A") |>
  node_prev() |>
  node_text()

diffobj::diffPrint(
  root |>
    node_find(pattern = "a <- $A") |>
    node_prev() |>
    node_text(),
  cat(readLines("/home/etienne/Desktop/Git/packages/astgrepr/tests/tinytest/_tinysnapshot/previous_node.txt")),
  guides = FALSE
)



# expect_snapshot_print(
#   label = "previous_node",
#   root |>
#     node_find(pattern = "a <- $A") |>
#     node_prev() |>
#     node_text()
# )
#
# expect_snapshot_print(
#   label = "next_node",
#   root |>
#     node_find(pattern = "a <- $A") |>
#     node_next() |>
#     node_text()
# )
#
# expect_snapshot_print(
#   label = "next_all_nodes",
#   root |>
#     node_find(pattern = "a <- $A") |>
#     node_next_all() |>
#     node_text_all()
# )
#
#
#
# ### get the parent/child node ---------------------------
#
# src <- "
# print('hi there')
# a <- 1
# fn <- function(x) {
#   x + 1
# }
# "
# root <- src |>
#   tree_new() |>
#   tree_root()
#
# expect_snapshot_print(
#   label = "parent_node",
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_parent() |>
#     node_text()
# )
#
# expect_snapshot_print(
#   label = "ancestors_node",
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_ancestors() |>
#     node_text_all()
# )
#
# expect_snapshot_print(
#   label = "child_node",
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_child(0) |>
#     node_text()
# )
#
# expect_equal(
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_child(10) |>
#     node_text(),
#   list()
# )
#
# expect_error(
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_child(1.5),
#   "`nth` must be an integer of length 1."
# )
#
# expect_error(
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_child("a"),
#   "`nth` must be an integer of length 1."
# )
#
# expect_error(
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_child(1:2),
#   "`nth` must be an integer of length 1."
# )
#
# expect_snapshot_print(
#   label = "children_node",
#   root |>
#     node_find(pattern = "$VAR + 1") |>
#     node_children() |>
#     node_text_all()
# )
