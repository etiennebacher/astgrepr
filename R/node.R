node_range <- function(x) {
  x$range()
}

node_is_leaf <- function(x) {
  x$is_leaf()
}

node_is_named <- function(x) {
  x$is_named()
}

node_is_named_leaf <- function(x) {
  x$is_named_leaf()
}

node_kind <- function(x) {
  x$kind()
}

node_text <- function(x) {
  x$text()
}

node_matches <- function(x, rule) {
  x$matches(rule)
}

node_inside <- function(x, rule) {
  x$inside(rule)
}

node_has <- function(x, rule) {
  x$has(rule)
}

node_precedes <- function(x, rule) {
  x$precedes(rule)
}

node_follows <- function(x, rule) {
  x$follows(rule)
}

node_get_match <- function(x, meta_var) {
  x$get_match(meta_var)
}

node_get_multiple_matches <- function(x, meta_var) {
  x$get_multiple_matches(meta_var)
}

node_get_transformed <- function(x, meta_var) {
  x$get_transformed(meta_var)
}

node_get_root <- function(x) {
  x$get_root()
}

node_find <- function(x, rule) {
  x$find(rule)
}

node_find_all <- function(x, rule) {
  x$find_all(rule)
}

node_field <- function(name) {
  x$field(name)
}

node_parent <- function() {
  x$parent()
}

node_child <- function(nth) {
  x$child(nth)
}

node_ancestors <- function() {
  x$ancestors()
}

node_children <- function() {
  x$children()
}

node_next_ <- function() {
  x$next_()
}

node_next_all <- function() {
  x$next_all
}

node_prev <- function() {
  x$prev()
}

node_prev_all <- function() {
  x$prev_all()
}
