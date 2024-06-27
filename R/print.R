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
  non_null <- rrapply::rrapply(x, condition = Negate(is.null), how = "prune")
  cat("<ast-grep rule>\n")
  cat(yaml::as.yaml(non_null))
}

#' @export
print.astgrep_relational_rule <- function(x, ...) {
  non_null <- rrapply::rrapply(x, condition = Negate(is.null), how = "prune")
  cat("<ast-grep relational rule>\n")
  cat(yaml::as.yaml(non_null))
}

#' @export
print.astgrep_pattern_rule <- function(x, ...) {
  non_null <- rrapply::rrapply(x, condition = Negate(is.null), how = "prune")
  cat("<ast-grep pattern rule>\n")
  cat(yaml::as.yaml(non_null))
}
