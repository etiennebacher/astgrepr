#' Create a syntax tree
#'
#' This function takes R code as string and creates the corresponding abstract
#' syntax tree (AST) from which we can query nodes.
#'
#' @param txt A character string of length 1 containing the code to parse.
#' If provided, `file` must not be provided.
#' @param file Path to file containing the code to parse. If provided, `txt`
#' must not be provided.
#' @param ignore_tags Character vector indicating the tags to ignore. Default is
#' `"ast-grep-ignore"`, meaning that any line that follows `# ast-grep-ignore`
#' will be ignored in the output of `node_*()` functions.
#'
#' @export
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(x)
#'     any(duplicated(x))"
#'
#' tree_new(src)
tree_new <- function(txt, file, ignore_tags = "ast-grep-ignore") {
  if ((missing(txt) && missing(file)) || (!missing(txt) && !missing(file))) {
    stop("Must pass either `txt` or `file`.")
  }
  if (!missing(txt) && (!all(is.character(txt)) || length(txt) != 1 )) {
    stop("`txt` must a be a string of length 1.")
  }
  if (length(ignore_tags) > 1) {
    ignore_pattern <- paste0("# (", paste(ignore_tags, collapse = "|"), ")")
  } else {
    ignore_pattern <- paste0("# ", ignore_tags)
  }
  if (!missing(file)) {
    raw_txt <- readLines(file, warn = FALSE)
    txt <- paste(raw_txt, collapse = "\n")
  } else {
    raw_txt <- strsplit(txt, "\\n")[[1]]
  }
  out <- SgRoot$new(txt)
  # Make this 0-indexed for easier comparison with ast-grep output
  attr(out, "lines_to_ignore") <- grep(ignore_pattern, raw_txt) - 1
  out
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
  out <- x$root()
  attr(out, "lines_to_ignore") <- attr(x, "lines_to_ignore")
  out
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
#'   node_find_all(
#'     ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
#'     ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
#'   )
#'
#' fixes <- nodes_to_replace |>
#'   node_replace_all(
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
  new_txt <- root$commit_edits(replacements)
  class(new_txt) <- c("astgrep_rewritten_tree", class(new_txt))
  new_txt
}


# Base R alternative to stringi::stri_sub()
"my_stri_sub<-" <- function(x, start, end, value) {
  # browser()
  if (start == 1) {
    part1 <- character()
  } else {
    part1 <- substr(x, 1, start - 1)
  }

  part2 <- value

  if (end == nchar(x)) {
    part3 <- character()
  } else {
    part3 <- substr(x, end + 1, stop = nchar(x))
  }

  paste0(part1, part2, part3)
}
