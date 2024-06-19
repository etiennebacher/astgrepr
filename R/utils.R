check_is_node <- function(x) {
  if (!inherits(x, "SgNode")) {
    stop("`x` must be an object of class 'SgNode'.")
  }
}

check_all_nodes <- function(x) {
  if (!all(vapply(x, \(y) inherits(y, "SgNode"), FUN.VALUE = logical(1)))) {
    stop("All elements of `x` must be objects of class 'SgNode'.")
  }
}

add_sgnodelist_class <- function(x) {
  class(x) <- c("SgNodeList", class(x))
  x
}
