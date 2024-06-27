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
#' Get information on whether a node is a leaf (meaning that it doesn't have
#' any children) and whether it is named.
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
#'     x <- z + 1
#'     any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' node_is_leaf(root)
#'
#' root |>
#'   node_find(pattern = "z") |>
#'   node_is_leaf()
#'
#' root |>
#'   node_find(pattern = "z") |>
#'   node_is_named()
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
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     x <- z + 1
#'     any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' root |>
#'   node_find(pattern = "any(duplicated($VAR))") |>
#'   node_kind()
#'
#' root |>
#'   node_find(pattern = "$X + $VALUE") |>
#'   node_kind()
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
#' @inheritParams node-find
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
#' some_node |>
#'   node_get_match("A") |>
#'   node_matches(kind = "argument")
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
#'   node_find(pattern = "plot($A)") |>
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
  unwrap_list_output(x$get_match(meta_var))
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

#' Recover the tree root from a node
#'
#' @inheritParams node-range
#'
#' @export
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
#' root |>
#'   node_find("print($A)") |>
#'   node_get_root() |>
#'   tree_root() |>
#'   node_text()
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
#' Some arguments (such as `kind`) require some knowledge of the tree-sitter
#' grammar of R. This grammar can be found here: <https://github.com/r-lib/tree-sitter-r/blob/main/src/grammar.json>.
#'
#' @inheritParams node-range
#'
#' @param pattern The pattern to search. This can contain meta-variables to
#' capture certain elements. Those meta-variables can then be recovered with
#' [node_get_match()] and [node_get_multiple_matches()]. The meta-variables must
#' start with `$` and have only uppercase letters, e.g. `$VAR`.
#'
#' @param kind The kind of element to search, e.g `"while_statement"`.
#' @param regex A regular expression to match the node's text. The regex must
#' match the whole text of the node.
#' @param inside TODO.
#' @param has TODO.
#' @param precedes TODO.
#' @param follows TODO.
#' @param all TODO.
#' @param any TODO.
#' @param not TODO.
#' @param matches TODO.
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
#'
#' # using the 'kind' of the nodes to find elements
#' src <- "
#'   a <- 1
#'   while (TRUE) { print('a') }
#' "
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' root |>
#'   node_find(kind = "while_statement") |>
#'   node_text()
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
  unwrap_list_output(x$find(rule_params))
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
#' @description
#' This is a collection of functions used to navigate the tree. Some of
#' them have a variant that applies on a single node (e.g. `node_next()`) and
#' one that applies on a list of nodes (e.g. `node_next_all()`):
#'
#' * `node_prev()`, `node_prev_all()`, `node_next()`, and `node_next_all()`
#'   get the previous and next node(s) that are at the same depth as the current
#'   node;
#' * `node_parent()`, `node_ancestors()`, `node_child()` and `node_children()`
#'   get the node(s) that are above or below the current node in terms of depth.
#'   All nodes except the root node have at least one node (the root).
#'
#' @inheritParams node-range
#' @param nth Integer. The child node to find. This is 0-indexed, so setting
#' `nth = 0` gets the first child.
#'
#' @name node-traversal
#' @export
#' @examples
#'
#' ### get the previous/next node ---------------------------
#'
#' src <- "
#' print('hi there')
#' a <- 1
#' fn <- function(x) {
#'   x + 1
#' }
#' "
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' root |>
#'   node_find(pattern = "a <- $A") |>
#'   node_prev() |>
#'   node_text()
#'
#' root |>
#'   node_find(pattern = "a <- $A") |>
#'   node_next() |>
#'   node_text()
#'
#' # there are nodes inside the function, but there are no more nodes on the
#' # same level as "fn"
#' root |>
#'   node_find(pattern = "a <- $A") |>
#'   node_next_all() |>
#'   node_text_all()
#'
#'
#' ### get the parent/child node ---------------------------
#'
#' src <- "
#' print('hi there')
#' a <- 1
#' fn <- function(x) {
#'   x + 1
#' }
#' "
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' root |>
#'   node_find(pattern = "$VAR + 1") |>
#'   node_parent() |>
#'   node_text()
#'
#' root |>
#'   node_find(pattern = "$VAR + 1") |>
#'   node_ancestors() |>
#'   node_text_all()
#'
#' root |>
#'   node_find(pattern = "$VAR + 1") |>
#'   node_child(0) |>
#'   node_text()
#'
#' root |>
#'   node_find(pattern = "$VAR + 1") |>
#'   node_children() |>
#'   node_text_all()
node_parent <- function(x) {
  check_is_node(x)
  unwrap_list_output(x$parent())
}

#' @name node-traversal
#' @export
node_child <- function(x, nth) {
  if (length(nth) != 1 || !is.numeric(nth) || (nth != 0 && nth %% 1 != 0)) {
    stop("`nth` must be an integer of length 1.")
  }
  unwrap_list_output(x$child(nth))
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
  unwrap_list_output(x$next_())
}

#' @name node-traversal
#' @export
node_next_all <- function(x) {
  check_is_node(x)
  add_sgnodelist_class(x$next_all())
}

#' @name node-traversal
#' @export
node_prev <- function(x) {
  check_is_node(x)
  unwrap_list_output(x$prev())
}

#' @name node-traversal
#' @export
node_prev_all <- function(x) {
  check_is_node(x)
  x$prev_all()
}

#' Change the code in the tree
#'
#' @description
#' `node_replace()` stores the replacement for a particular node, it doesn't
#' change the actual code. `node_replace_all()` does the same but for several
#' nodes (e.g. the output of `node_find_all()`).
#'
#' `node_commit_edits()` takes a list of replacements (the output of
#' `node_replace()` or `node_replace_all()`) and applies them one by one to the
#' input node, returning the modified code.
#'
#' @inheritParams node-range
#' @param new_text The replacement for a node.
#' @param edits A list of replacements (the output of `node_replace()` or
#' `node_replace_all()`).
#'
#' @name node-fix
#' @export
#'
#' @examples
#' src <- "x <- rnorm(100, mean = 2)
#' any(duplicated(y))
#' plot(mtcars)
#' any(duplicated(x))"
#'
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' # one replacement ------------------------------------------
#'
#' node_to_fix <- root |>
#'   node_find(pattern = "any(duplicated($A))")
#'
#' fix <- node_to_fix |>
#'   node_replace(
#'     paste0(
#'       "anyDuplicated(",
#'       node_text(node_get_match(node_to_fix, "A")),
#'       ") > 0"
#'     )
#'   )
#'
#' node_commit_edits(root, fix) |>
#'   cat()
#'
#' # several replacements ------------------------------------------
#'
#' nodes_to_fix <- root |>
#'   node_find_all(pattern = "any(duplicated($A))")
#'
#' fixes <- nodes_to_fix |>
#'   node_replace_all(
#'     paste0(
#'       "anyDuplicated(",
#'       node_text_all(lapply(nodes_to_fix, function(x) node_get_match(x, "A"))),
#'       ") > 0"
#'     )
#'   )
#'
#' node_commit_edits(root, fixes) |>
#'   cat()
node_replace <- function(x, new_text) {
  check_is_node(x)
  x$replace(new_text)
}

#' @name node-fix
#' @export
node_replace_all <- function(x, new_text) {
  check_all_nodes(x)
  if (length(x) > 1 && length(new_text) == 1) {
    new_text <- rep(new_text, length(x))
  }
  lapply(seq_along(x), function(y) {
    x[[y]]$replace(new_text[y])
  })
}

#' @name node-fix
#' @export
node_commit_edits <- function(x, edits) {
  check_is_node(x)
  if (!is.list(edits)) {
    stop("`edits` must be a list.")
  }
  if (length(edits[[1]]) == 1) {
    edits <- list(edits)
  }
  x$commit_edits(edits)
}
