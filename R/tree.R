#' Create a syntax tree
#'
#' This function takes R code as string and creates the corresponding abstract
#' syntax tree (AST) from which we can query nodes.
#'
#' @param src String. R code to parse.
#'
#' @export
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(x)
#'     any(duplicated(x))"
#'
#' tree_new(src)
tree_new <- function(src) {
  SgRoot$new(src)
}

#' Get the root of the syntax tree
#'
#' This function takes a tree created by [tree_new()] and returns the root node
#' containing all subsequent nodes.
#'
#' @param x A tree created by [tree_new()].
#'
#' @export
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(x)
#'     any(duplicated(x))"
#'
#' tree <- tree_new(src)
#' tree_root(tree)
tree_root <- function(x) {
  x$root()
}
