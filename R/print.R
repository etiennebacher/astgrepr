#' @export
print.SgNode <- function(x, ...) {
  cat("<AST node>")
}

#' @export
print.SgNodeList <- function(x, ...) {
  l <- length(x)
  cat(paste0("<List of ", l, " AST nodes>"))
}
