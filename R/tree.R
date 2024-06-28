#' Create a syntax tree
#'
#' This function takes R code as string and creates the corresponding abstract
#' syntax tree (AST) from which we can query nodes.
#'
#' @param txt A character string of length 1 containing the code to parse.
#' If provided, `file` must not be provided.
#' @param file Path to file containing the code to parse. If provided, `txt`
#' must not be provided.
#'
#' @export
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(x)
#'     any(duplicated(x))"
#'
#' tree_new(src)
tree_new <- function(txt, file) {
  if ((missing(txt) && missing(file)) || (!missing(txt) && !missing(file))) {
    stop("Must pass either `txt` or `file`.")
  }
  if (!missing(txt) && (!all(is.character(txt)) || length(txt) != 1 )) {
    stop("`txt` must a be a string of length 1.")
  }
  if (!missing(file)) {
    txt <- paste(readLines(file), collapse = "\n")
  }
  SgRoot$new(txt)
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
  check_is_tree(x)
  x$root()
}

#' Rewrite the tree with a list of replacements
#'
#' @param root The root tree, obtained via `tree_root()`
#' @param replacements A list of replacements, obtained via `node_replace()` or
#' `node_replace_all()`.
#'
#' @return A string character corresponding to the code used to build the tree
#' root but with replacements applied.
#' @export
#'
#' @examples
#'
#' src <- "x <- c(1, 2, 3)
#' any(duplicated(x), na.rm = TRUE)
#' any(duplicated(x))
#' if (any(is.na(x))) {
#'   TRUE
#' }
#' any(is.na(y))"
#'
#' root <- tree_new(src) |>
#'   tree_root()
#'
#'
#' ### Only replace the first nodes found by each rule
#'
#' nodes_to_replace <- root |>
#'   node_find(
#'     ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
#'     ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
#'   )
#'
#' fixes <- nodes_to_replace |>
#'   node_replace(
#'     any_na = "anyNA(~~VAR~~)",
#'     any_dup = "anyDuplicated(~~VAR~~) > 0"
#'   )
#'
#' # original code
#' cat(src)
#'
#' # new code
#' tree_rewrite(root, fixes)
#'
#'
#' ### Replace all nodes found by each rule
#'
#' nodes_to_replace <- root |>
#'   node_find(
#'     ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
#'     ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
#'   )
#'
#' fixes <- nodes_to_replace |>
#'   node_replace(
#'     any_na = "anyNA(~~VAR~~)",
#'     any_dup = "anyDuplicated(~~VAR~~) > 0"
#'   )
#'
#' # original code
#' cat(src)
#'
#' # new code
#' tree_rewrite(root, fixes)

tree_rewrite <- function(root, replacements) {
  original_txt <- strsplit(root$text(), "\n")[[1]]
  new_txt <- original_txt

  if (inherits(replacements, "astgrep_replacements")) {
    replacements <- unlist(replacements, recursive = FALSE)
  }

  for (i in seq_along(replacements)) {
    elem_row <- attr(replacements[[i]], "coords_start")[1] + 1
    start <- attr(replacements[[i]], "coords_start")[2] + 1
    end <- attr(replacements[[i]], "coords_end")[2]
    # TODO: find a base R alternative
    stringi::stri_sub(new_txt[elem_row], start, end) <- replacements[[i]]
  }
  class(new_txt) <- c("astgrep_rewritten_tree", class(new_txt))
  new_txt
}
