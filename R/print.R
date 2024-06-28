#' @export
print.SgNode <- function(x, ...) {
  cat("<AST node>")
}

#' @export
print.RuleList <- function(x, ...) {
  l <- length(x)
  cat(paste0("<List of ", l, ifelse(l > 1, " rules>", " rule>")))
  for (i in 1:l) {
    cat(paste0("\n|--", names(x)[i], ": ", length(x[[i]]), " node"))
  }
}

#' @export
print.SgNodeList <- function(x, ...) {
  l <- length(x)
  cat(paste0("<List of ", l, ifelse(l > 1, " rules>", " rule>")))
  for (i in 1:l) {
    cat(paste0("\n|--", names(x)[i], ": ", length(x[[i]]), " nodes"))
  }
}

#' @export
print.astgrep_rule <- function(x, ...) {
  nm <- if (is.null(x[["id"]])) {
    "<unnamed>"
  } else {
    paste0("'", x[["id"]], "'")
  }
  cat(paste0("<ast-grep rule: ", nm, ">\n"))
  cat(to_yaml(x))
}

#' @export
print.astgrep_relational_rule <- function(x, ...) {
  cat("<ast-grep relational rule>\n")
  cat(to_yaml(x))
}

#' @export
print.astgrep_pattern_rule <- function(x, ...) {
  cat("<ast-grep pattern rule>\n")
  cat(to_yaml(x))
}
