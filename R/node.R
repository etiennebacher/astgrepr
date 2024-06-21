#' Get the start and end positions of a node
#'
#' @param x A node, either from [tree_root()] or from another `node_*()`
#' function.
#'
#' @export
#' @name node-range
#'
#' @return A list of two elements: `start` and `end`. Each of those is a vector
#' with two values indicating the row and column. Those are 0-indexed.
#'
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(x)
#'     any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' node_range(root)
#'
#' root |>
#'   node_find(pattern = "rnorm($$$A)") |>
#'   node_range()
node_range <- function(x) {
  check_is_node(x)
  out <- x$range()
  names(out) <- c("start", "end")
  out
}

#' @name node-range
#' @export
node_range_all <- function(x) {
  check_all_nodes(x)
  lapply(x, function(nodes) {
    out <- nodes$range()
    names(out) <- c("start", "end")
    out
  })
}

#' Get information on nodes
#'
#' @inheritParams node-range
#'
#' @export
#' @return A logical value.
#'
#' @name node-is
#'
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(x)
#'     any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' node_is_leaf(root)
#'
#' root |>
#'   node_find(pattern = "rnorm($$$A)") |>
#'   node_is_leaf()
node_is_leaf <- function(x) {
  check_is_node(x)
  x$is_leaf()
}

#' @name node-is
#' @export
node_is_named <- function(x) {
  check_is_node(x)
  x$is_named()
}

#' @name node-is
#' @export
node_is_named_leaf <- function(x) {
  check_is_node(x)
  x$is_named_leaf()
}

#' Find the kind of a node
#'
#' @inheritParams node-range
#'
#' @export
node_kind <- function(x) {
  check_is_node(x)
  x$kind()
}

#' Extract the code corresponding to one or several nodes
#'
#' @description
#' Those functions extract the code corresponding to the node(s):
#' * `node_text()` applies on a single node, for example the output of
#'   [node_get_match()]
#' * `node_text_all()` applies on a list of nodes, for example the output of
#'   [node_get_multiple_matches()]
#'
#' @inheritParams node-range
#'
#' @export
#' @name node-text
#'
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(mtcars)
#'     any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' # node_text() must be applied on single nodes
#' root |>
#'   node_find(pattern = "plot($A)") |>
#'   node_text()
#'
#' # node_find_all() returns a list on nodes on which
#' # we can use node_text_all()
#' root |>
#'   node_find_all(pattern = "any(duplicated($A))") |>
#'   node_text_all()
node_text <- function(x) {
  if (length(x) == 0) return(list())
  check_is_node(x)
  x$text()
}

#' @name node-text
#' @export
node_text_all <- function(x) {
  check_all_nodes(x)
  lapply(x, function(nodes) nodes$text())
}

#' Get more precise information on a node
#'
#' @inheritParams node-range
#' @param m The rule to apply.
#'
#' @export
#' @name node-info
#'
#' @examples
#' src <- "
#' print('hi')
#' fn <- function() {
#'   print('hello')
#' }
#' "
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' some_node <- root |>
#'   node_find(pattern = "print($A)")
#'
#' node_text(some_node)
#'
#' node_matches(some_node, kind = "function_declaration")
node_matches <- function(
    x,
    pattern = NULL,
    kind = NULL,
    regex = NULL,
    inside = NULL,
    has = NULL,
    precedes = NULL,
    follows = NULL,
    all = NULL,
    any = NULL,
    not = NULL,
    matches = NULL
) {
  # if (!is.null(config)) {
  #   if (!missing(pattern)) {
  #     stop("Either provide `pattern` or `config`, not both.")
  #   }
  #   config <- yaml::read_yaml("any_duplicated.yml")$rule
  #   pattern <- NULL
  # } else {
  #   pattern <- as.list(pattern)
  #   names(pattern) <- "pattern"
  #   config <- NULL
  # }
  rule_params <- list(
    pattern = pattern,
    kind = kind,
    regex = regex,
    inside = inside,
    has = has,
    precedes = precedes,
    follows = follows,
    all = all,
    any = any,
    not = not,
    matches = matches
  )
  x$matches(rule_params)
}

#' @name node-info
#' @export
node_inside <- function(x, m) {
  x$inside(m)
}

#' @name node-info
#' @export
node_has <- function(x, m) {
  x$has(m)
}

#' @name node-info
#' @export
node_precedes <- function(x, m) {
  x$precedes(m)
}

#' @name node-info
#' @export
node_follows <- function(x, m) {
  x$follows(m)
}

#' Get the match(es) from a meta-variable
#'
#' Those functions extract the content of the meta-variable specified in
#' [node_find()]:
#' * `node_get_match()` is used when the meta-variable refers to a single
#'   pattern, e.g. `"plot($A)`;
#' * `node_get_multiple_matches()` is used when the meta-variable captures all
#'   elements in a pattern, e.g. `"plot($$$A)"`.
#'
#' @inheritParams node-range
#' @param meta_var The name given to one of the meta-variable(s) in
#' `node_find()`.
#'
#' @export
#' @name node-get-match
#'
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     plot(mtcars)"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' # we capture a single element with "$A" so node_get_match() can be used
#' root |>
#'   node_find(pattern = "plot($A)")) |>
#'   node_get_match("A")
#'
#' # we can specify the variable to extract
#' root |>
#'   node_find(pattern = "rnorm($A, $B)") |>
#'   node_get_match("B")
#'
#' # we capture many elements with "$$$A" so node_get_multiple_matches() can
#' # be used here
#' root |>
#'   node_find(pattern = "rnorm($$$A)") |>
#'   node_get_multiple_matches("A")
node_get_match <- function(x, meta_var) {
  x$get_match(meta_var)
}

#' @name node-get-match
#' @export
node_get_multiple_matches <- function(x, meta_var) {
  out <- x$get_multiple_matches(meta_var)
  if (length(out) == 0) {
    return(list())
  }
  add_sgnodelist_class(out)
}

#' @export
node_get_transformed <- function(x, meta_var) {
  x$get_transformed(meta_var)
}

#' @export
node_get_root <- function(x) {
  check_is_node(x)
  x$get_root()
}

#' Find node(s) matching a pattern
#'
#' @description
#' Those functions find one or several nodes based on some rule:
#' * `node_find()` returns the first node that is found;
#' * `node_find_all()` returns a list of all nodes found.
#'
#' @inheritParams node-range
#' @param pattern The pattern to search. This must be a list of named elements.
#' Elements can be:
#' * `pattern`:
#' * `kind`:
#'
#' @name node-find
#' @export
#' @return
#' `node_find()` returns a single `SgNode`.
#'
#' `node_find_all()` returns a list of `SgNode`s.
#'
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(mtcars)
#'     any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' root |>
#'   node_find(pattern = "any(duplicated($A))")
#'
#' root |>
#'   node_find_all(pattern = "any(duplicated($A))")
node_find <- function(
    x,
    pattern = NULL,
    kind = NULL,
    regex = NULL,
    inside = NULL,
    has = NULL,
    precedes = NULL,
    follows = NULL,
    all = NULL,
    any = NULL,
    not = NULL,
    matches = NULL
) {
  # if (!is.null(config)) {
  #   if (!missing(pattern)) {
  #     stop("Either provide `pattern` or `config`, not both.")
  #   }
  #   config <- yaml::read_yaml("any_duplicated.yml")$rule
  #   pattern <- NULL
  # } else {
  #   pattern <- as.list(pattern)
  #   names(pattern) <- "pattern"
  #   config <- NULL
  # }
  rule_params <- list(
    pattern = pattern,
    kind = kind,
    regex = regex,
    inside = inside,
    has = has,
    precedes = precedes,
    follows = follows,
    all = all,
    any = any,
    not = not,
    matches = matches
  )
  x$find(rule_params) |>
    unwrap_list_output()
}

#' @name node-find
#' @export
node_find_all <- function(
    x,
    pattern = NULL,
    kind = NULL,
    regex = NULL,
    inside = NULL,
    has = NULL,
    precedes = NULL,
    follows = NULL,
    all = NULL,
    any = NULL,
    not = NULL,
    matches = NULL
) {
  # if (!is.null(config)) {
  #   if (!missing(pattern)) {
  #     stop("Either provide `pattern` or `config`, not both.")
  #   }
  #   pattern <- yaml::read_yaml("any_duplicated.yml")$rule
  # } else {
  #   pattern <- as.list(pattern)
  #   names(pattern) <- "pattern"
  # }
  rule_params <- list(
    pattern = pattern,
    kind = kind,
    regex = regex,
    inside = inside,
    has = has,
    precedes = precedes,
    follows = follows,
    all = all,
    any = any,
    not = not,
    matches = matches
  )
  x$find_all(rule_params)
}

#' Navigate the tree
#'
#' This is a collection of functions used to navigate the tree. Some of
#' them have a variant that applies on a single node (e.g. `node_next()`) and
#' one that applies on a list of nodes (e.g. `node_next_all()`).
#'
#' @inheritParams node-range
#' @param nth Integer. The child node to find. This is 0-indexed, so setting
#' `nth = 0` gets the first child.
#'
#' @name node-traversal
#' @export
node_parent <- function(x) {
  check_is_node(x)
  x$parent()
}

#' @name node-traversal
#' @export
node_child <- function(x, nth) {
  x$child(nth)
}

#' @name node-traversal
#' @export
node_field <- function(name) {
  x$field(name)
}

#' @name node-traversal
#' @export
node_ancestors <- function(x) {
  check_is_node(x)
  out <- x$ancestors()
  add_sgnodelist_class(out)
}

#' @name node-traversal
#' @export
node_children <- function(x) {
  check_is_node(x)
  out <- x$children()
  add_sgnodelist_class(out)
}

#' @name node-traversal
#' @export
node_next <- function(x) {
  check_is_node(x)
  x$next_()
}

#' @name node-traversal
#' @export
node_next_all <- function(x) {
  check_is_node(x)
  x$next_all
}

#' @name node-traversal
#' @export
node_prev <- function(x) {
  check_is_node(x)
  x$prev()
}

#' @name node-traversal
#' @export
node_prev_all <- function(x) {
  check_is_node(x)
  x$prev_all()
}
