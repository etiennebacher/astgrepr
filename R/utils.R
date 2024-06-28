check_is_tree <- function(x) {
  if (!inherits(x, "SgRoot")) {
    stop("`x` must be an object of class 'SgRoot'.")
  }
}

check_is_rulelist_or_node <- function(x) {
  if (!inherits(x, c("RuleList", "SgNode"))) {
    stop("`x` must be an object of class 'RuleList' or 'SgNode'.")
  }
}

check_all_nodes <- function(x) {
  if (!all(vapply(unlist(x, recursive = TRUE), \(y) inherits(y, "SgNode"), FUN.VALUE = logical(1)))) {
    stop("All elements of `x` must be objects of class 'SgNode'.")
  }
}

add_sgnodelist_class <- function(x) {
  class(x) <- c("SgNodeList", class(x))
  x
}

add_rulelist_class <- function(x) {
  class(x) <- c("RuleList", class(x))
  x
}

# Only to be called in functions that return a single node
unwrap_list_output <- function(x) {
  if (length(x) > 0) {
    unlist(x, recursive = FALSE)
  } else {
    x
  }
}

to_yaml <- function(x) {
  non_null <- rrapply::rrapply(x, condition = Negate(is.null), how = "prune")
  if ("id" %in% names(non_null)) {
    non_null[["id"]] <- NULL
  }
  yaml::as.yaml(non_null, indent.mapping.sequence = TRUE)
}