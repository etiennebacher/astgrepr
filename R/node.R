#' @export
node_range <- function(x) {
  x$range()
}

#' @export
node_is_leaf <- function(x) {
  x$is_leaf()
}

#' @export
node_is_named <- function(x) {
  x$is_named()
}

#' @export
node_is_named_leaf <- function(x) {
  x$is_named_leaf()
}

#' @export
node_kind <- function(x) {
  x$kind()
}

#' @export
node_text <- function(x) {
  x$text()
}

#' @export
node_matches <- function(x, rule) {
  x$matches(rule)
}

#' @export
node_inside <- function(x, rule) {
  x$inside(rule)
}

#' @export
node_has <- function(x, rule) {
  x$has(rule)
}

#' @export
node_precedes <- function(x, rule) {
  x$precedes(rule)
}

#' @export
node_follows <- function(x, rule) {
  x$follows(rule)
}

#' @export
node_get_match <- function(x, meta_var) {
  x$get_match(meta_var)
}

#' @export
node_get_multiple_matches <- function(x, meta_var) {
  x$get_multiple_matches(meta_var)
}

#' @export
node_get_transformed <- function(x, meta_var) {
  x$get_transformed(meta_var)
}

#' @export
node_get_root <- function(x) {
  x$get_root()
}

#' @export
node_find <- function(x, rule) {
  x$find(rule)
}

#' @export
node_find_all <- function(x, rule) {
  x$find_all(rule)
}

#' @export
node_field <- function(name) {
  x$field(name)
}

#' @export
node_parent <- function() {
  x$parent()
}

#' @export
node_child <- function(nth) {
  x$child(nth)
}

#' @export
node_ancestors <- function() {
  x$ancestors()
}

#' @export
node_children <- function() {
  x$children()
}

#' @export
node_next_ <- function() {
  x$next_()
}

#' @export
node_next_all <- function() {
  x$next_all
}

#' @export
node_prev <- function() {
  x$prev()
}

#' @export
node_prev_all <- function() {
  x$prev_all()
}
