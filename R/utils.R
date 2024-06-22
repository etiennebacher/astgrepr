check_is_tree <- function(x) {
  if (!inherits(x, "SgRoot")) {
    stop("`x` must be an object of class 'SgRoot'.")
  }
}

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

build_matcher_from_dots <- function(...) {
  dots <- eval(substitute(alist(...)))
  dots
}

# Only to be called in functions that return a single node
unwrap_list_output <- function(x) {
  if (length(x) > 0) {
    x[[1]]
  } else {
    x
  }
}
