#' @export
print.SgNode <- function(x, ...) {
  cat("<AST node>")
}

#' @export
print.SgNodeList <- function(x, ...) {
  l <- length(x)
  cat(paste0("<List of ", l, " AST nodes>"))
}

#' @export
print.astgrep_rule <- function(x, ...) {
  cat("<ast-grep rule>\n")
  # browser()
  for (i in seq_along(x)) {
    cat(paste0("  ", names(x)[i], ": ", x[i], "\n"))
  }
}
