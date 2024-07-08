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
  elems <- unlist(x, recursive = TRUE)
  # faster than going through all elems via vapply() since here we can stop on
  # first non-node instead of using inherits() on all of them
  for (i in elems) {
    if (!inherits(i, "SgNode")) {
      stop("All elements of `x` must be objects of class 'SgNode'.")
    }
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

seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to"))
