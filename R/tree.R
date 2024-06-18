#' @export
tree_new <- function(src) {
  SgRoot$new(src)
}

#' @export
tree_root <- function(x) {
  x$root()
}
