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
#'   node_find(ast_rule(pattern = "rnorm($$$A)")) |>
#'   node_range()
#'
#' # There is also an "_all" variant when there are several nodes per rule
#' root |>
#'   node_find_all(
#'     ast_rule(pattern = "any(duplicated($A))"),
#'     ast_rule(pattern = "plot($A)")
#'   ) |>
#'   node_range_all()
node_range <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	out <- lapply(x, function(y) {
		res <- y$range()
		names(res) <- c("start", "end")
		res
	})
	out
}

#' @name node-range
#' @export
node_range_all <- function(x) {
	check_all_nodes(x)
	lapply(x, function(rule) {
		lapply(rule, function(node) {
			out <- node$range()
			names(out) <- c("start", "end")
			out
		})
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
#'   node_find(ast_rule(pattern = "z")) |>
#'   node_is_leaf()
#'
#' root |>
#'   node_find(ast_rule(pattern = "z")) |>
#'   node_is_named()
node_is_leaf <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$is_leaf())
}

#' @name node-is
#' @export
node_is_named <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$is_named())
}

#' @name node-is
#' @export
node_is_named_leaf <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$is_named_leaf())
}

node_to_list <- function(x) {
	if (length(x) == 1 && inherits(x, "SgNode")) {
		x <- list(x)
	}
	x
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
#'   node_find(ast_rule(pattern = "any(duplicated($VAR))")) |>
#'   node_kind()
#'
#' root |>
#'   node_find(ast_rule(pattern = "$X + $VALUE")) |>
#'   node_kind()
node_kind <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) {
		if (length(y) > 0) {
			y$kind()
		} else {
			NULL
		}
	})
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
#'   node_find(ast_rule(pattern = "plot($A)")) |>
#'   node_text()
#'
#' # node_find_all() returns a list on nodes on which
#' # we can use node_text_all()
#' root |>
#'   node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
#'   node_text_all()
node_text <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) {
		if (length(y) > 0) {
			if (is.list(y)) {
				y[[1]]$text()
			} else {
				y$text()
			}
		} else {
			NULL
		}
	})
}

#' @name node-text
#' @export
node_text_all <- function(x) {
	check_all_nodes(x)
	lapply(x, function(rule) {
		lapply(rule, function(node) {
			node$text()
		})
	})
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
#'   node_find(ast_rule(pattern = "print($A)"))
#'
#' node_text(some_node)
#'
#' some_node |>
#'   node_get_match("A") |>
#'   node_matches(ast_rule(kind = "argument"))
node_matches <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)

	out <- lapply(seq_along(rules), function(rule_idx) {
		rule <- rules[[rule_idx]]
		constraints <- attr(rule, "other_info")$constraints
		rule <- list(rule = rule, constraints = constraints) |>
			to_yaml()
		res <- x[[rule_idx]][[1]]$matches(rule)
		res
	}) |>
		list()

	names(out) <- names(x)
	out
}

#' @name node-info
#' @export
node_inside <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)

	out <- lapply(seq_along(rules), function(rule_idx) {
		rule <- rules[[rule_idx]]
		res <- x[[rule_idx]][[1]]$inside(to_yaml(rule))
		res
	}) |>
		list()
	names(out) <- names(rules)
	out
}

#' @name node-info
#' @export
node_has <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)

	out <- lapply(seq_along(rules), function(rule_idx) {
		rule <- rules[[rule_idx]]
		res <- x[[rule_idx]][[1]]$has(to_yaml(rule))
		res
	}) |>
		list()
	names(out) <- names(rules)
	out
}

#' @name node-info
#' @export
node_precedes <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)

	out <- lapply(seq_along(rules), function(rule_idx) {
		rule <- rules[[rule_idx]]
		res <- x[[rule_idx]][[1]]$precedes(to_yaml(rule))
		res
	}) |>
		list()
	names(out) <- names(rules)
	out
}

#' @name node-info
#' @export
node_follows <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)

	out <- lapply(seq_along(rules), function(rule_idx) {
		rule <- rules[[rule_idx]]
		res <- x[[rule_idx]][[1]]$follows(to_yaml(rule))
		res
	}) |>
		list()
	names(out) <- names(rules)
	out
}

#' Get the match(es) from a meta-variable
#'
#' Those functions extract the content of the meta-variable specified in
#' [node_find()]:
#' * `node_get_match()` is used when the meta-variable refers to a single
#' pattern, e.g. `"plot($A)`;
#' * `node_get_multiple_matches()` is used when the meta-variable captures all
#' elements in a pattern, e.g. `"plot($$$A)"`.
#'
#' @inheritParams node-range
#' @param meta_var The name given to one of the meta-variable(s) in
#'   `node_find()`.
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
#'   node_find(ast_rule(pattern = "plot($A)")) |>
#'   node_get_match("A")
#'
#' # we can specify the variable to extract
#' root |>
#'   node_find(ast_rule(pattern = "rnorm($A, $B)")) |>
#'   node_get_match("B")
#'
#' # we capture many elements with "$$$A" so node_get_multiple_matches() can
#' # be used here
#' root |>
#'   node_find(ast_rule(pattern = "rnorm($$$A)")) |>
#'   node_get_multiple_matches("A")
node_get_match <- function(x, meta_var) {
	check_is_rulelist_or_node(x)
	if (length(x) == 1 && inherits(x, "SgNode")) {
		x <- list(x)
	}
	lapply(x, function(y) y$get_match(meta_var)) |>
		add_rulelist_class()
}

#' @name node-get-match
#' @export
node_get_multiple_matches <- function(x, meta_var) {
	check_is_rulelist_or_node(x)
	lapply(x, function(y) y$get_multiple_matches(meta_var)) |>
		add_sgnodelist_class()
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
#'   node_find(ast_rule(pattern = "print($A)")) |>
#'   node_get_root() |>
#'   tree_root() |>
#'   node_text()
node_get_root <- function(x) {
	check_is_rulelist_or_node(x)
	x[[1]]$get_root()
}

#' Find node(s) matching a pattern
#'
#' @description Those functions find one or several nodes based on some rule:
#' * `node_find()` returns the first node that is found;
#' * `node_find_all()` returns a list of all nodes found.
#'
#' Some arguments (such as `kind`) require some knowledge of the tree-sitter
#' grammar of R. This grammar can be found here:
#' <https://github.com/r-lib/tree-sitter-r/blob/main/src/grammar.json>.
#'
#' @inheritParams node-range
#' @param ... Any number of rules created with `ast_rule()`.
#' @param files A vector of filenames containing rules. Those must be `.yaml`
#' files.
#' @name node-find
#' @export
#' @return `node_find()` returns a single `SgNode`.
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
#'   node_find(ast_rule(pattern = "any(duplicated($A))"))
#'
#' root |>
#'   node_find_all(ast_rule(pattern = "any(duplicated($A))"))
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
#'   node_find(ast_rule(kind = "while_statement"))
#'
#' # one can pass several rules at once
#' src <- "x <- rnorm(100, mean = 2)
#'     any(duplicated(y))
#'     plot(mtcars)
#'     any(duplicated(x))
#'     while (TRUE) { print('a') }"
#' root <- src |>
#'   tree_new() |>
#'   tree_root()
#'
#' root |>
#'   node_find(
#'     ast_rule(pattern = "any(duplicated($A))"),
#'     ast_rule(kind = "while_statement")
#'   )
#'
#' root |>
#'   node_find_all(
#'     ast_rule(pattern = "any(duplicated($A))"),
#'     ast_rule(kind = "while_statement")
#'   )
node_find <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)
	rules_ids <- get_rules_ids(rules)
	names(rules) <- rules_ids

	lapply(seq_along(rules), function(rule_idx) {
		rule <- rules[[rule_idx]]
		name_rule <- rules_ids[[rule_idx]]
		constraints <- attr(rule, "other_info")$constraints
		rule <- list(rule = rule, constraints = constraints) |>
			to_yaml()
		res <- x$find_all(rule)
		res <- unlist(res)
		res <- remove_ignored_nodes(
			res,
			rule_id = name_rule,
			ignored_lines = attributes(x)$lines_to_ignore
		)
		if (length(res) > 0) {
			res <- res[[1]]
			attr(res, "other_info") <- attr(rule, "other_info")
		}
		res
	}) |>
		add_rulelist_class() |>
		stats::setNames(rules_ids)
}

#' @name node-find
#' @export
node_find_all <- function(x, ..., files = NULL) {
	rules <- list(...)
	rules <- combine_rules_and_files(rules, files)
	rules_ids <- get_rules_ids(rules)

	rules2 <- vapply(
		seq_along(rules),
		function(x) {
			rul <- rules[[x]]
			cons <- attr(rul, "other_info")$constraints
			list(rule = rul, constraints = cons) |>
				to_yaml()
		},
		character(1)
	)

	out <- x$find_all(rules2)
	out <- lapply(seq_along(out), function(node_idx) {
		res <- out[[node_idx]]
		name_rule <- rules_ids[[node_idx]]
		if (length(res) == 0) {
			return(NULL)
		}
		res <- unlist(res, recursive = FALSE)
		res <- remove_ignored_nodes(
			res,
			rule_id = name_rule,
			ignored_lines = attributes(x)$lines_to_ignore
		)
		if (is.null(res)) {
			return(list())
		} else if (!is.list(res)) {
			res <- list(res)
		}
		names(res) <- paste0("node_", seq_along(res))
		attr(res, "other_info") <- attr(rules[[node_idx]], "other_info")

		msg <- attr(res, "other_info")$message
		meta_var <- regmatches(msg, gregexpr("~~([A-Z0-9]+)~~", msg))
		if (length(meta_var) > 0) {
			meta_var <- meta_var[[1]]
			meta_var <- gsub("~~", "", meta_var)

			repl <- vapply(
				meta_var,
				function(mv) {
					matches <- node_get_match(res[[1]], mv)
					if (length(matches[[1]]) == 0) {
						matches <- res[[1]]$get_multiple_matches(mv)
						txts <- unlist(node_text_all(list(matches)))
						if (txts[length(txts)] == ",") {
							txts <- txts[-length(txts)]
						}
						txts <- gsub("^,$", ", ", txts)
						paste(txts, collapse = "")
					} else {
						node_text(matches)[[1]]
					}
				},
				character(1)
			)

			if (length(repl) > 0) {
				for (i in names(repl)) {
					msg <- gsub(paste0("~~", i, "~~"), repl[i], msg, fixed = TRUE)
				}
			}
			attr(res, "other_info")$message <- msg
		}

		res
	}) |>
		add_sgnodelist_class()

	names(out) <- rules_ids
	out
}

combine_rules_and_files <- function(rules, files) {
	if (is.null(files) && length(rules) == 0) {
		stop("Must be specify either argument `files` or some rules in `...`.")
	}
	if (!is.null(files)) {
		files_char <- lapply(files, function(x) {
			rul <- readLines(x, warn = FALSE)
			rul <- rul[grep("^#", rul, invert = TRUE)]
			rul <- paste(rul, collapse = "\n")
			rul <- strsplit(rul, "---")[[1]]
			lapply(rul, function(y) {
				out <- yaml::yaml.load(y)
				res <- out$rule
				attr(res, "id") <- out$id
				class(res) <- c("astgrep_rule", class(res))
				attr(res, "other_info") <- out[-which(names(out) %in% c("rule", "id"))]
				res
			})
		})
		files_char <- unlist(files_char, recursive = FALSE)
		rules <- append(rules, files_char)
	}
	rules
}

get_rules_ids <- function(rules) {
	rules_ids <- lapply(seq_along(rules), function(rule_idx) {
		id <- attr(rules[[rule_idx]], "id")
		if (is.null(id)) {
			id <- paste0("rule_", rule_idx)
		}
		id
	})
	if (anyDuplicated(rules_ids) > 0) {
		name_count <- table(factor(unlist(rules_ids)))
		name_count <- name_count[name_count > 1]
		stop(
			"Rule IDs must be unique. The following are duplicated: ",
			paste0(names(name_count), " (", name_count, ")", collapse = ", "),
			"."
		)
	}
	rules_ids
}

remove_ignored_nodes <- function(nodes, rule_id, ignored_lines) {
	if (length(ignored_lines) == 0) {
		return(nodes)
	}
	ignored_lines <- unique(
		c(
			unlist(ignored_lines[[rule_id]]),
			unlist(ignored_lines[["all_rules"]])
		)
	)
	nodes_suppressed <- lapply(nodes, function(found) {
		line_start <- found$range()[[1]][[1]]
		if (any(ignored_lines + 1 == line_start)) {
			return(NULL)
		} else {
			found
		}
	})
	nodes_suppressed <- drop_null_elements(nodes_suppressed)
	if (length(nodes_suppressed) == 0) {
		NULL
	} else {
		nodes_suppressed
	}
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
#'   node_find(ast_rule(pattern = "a <- $A")) |>
#'   node_prev() |>
#'   node_text()
#'
#' root |>
#'   node_find(ast_rule(pattern = "a <- $A")) |>
#'   node_next() |>
#'   node_text()
#'
#' # there are nodes inside the function, but there are no more nodes on the
#' # same level as "fn"
#' root |>
#'   node_find(ast_rule(pattern = "a <- $A")) |>
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
#'   node_find(ast_rule(pattern = "$VAR + 1")) |>
#'   node_parent() |>
#'   node_text()
#'
#' root |>
#'   node_find(ast_rule(pattern = "$VAR + 1")) |>
#'   node_ancestors() |>
#'   node_text_all()
#'
#' root |>
#'   node_find(ast_rule(pattern = "$VAR + 1")) |>
#'   node_child(0) |>
#'   node_text()
#'
#' root |>
#'   node_find(ast_rule(pattern = "$VAR + 1")) |>
#'   node_children() |>
#'   node_text_all()
node_parent <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$parent()[[1]]) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_child <- function(x, nth) {
	if (length(nth) != 1 || !is.numeric(nth) || (nth != 0 && nth %% 1 != 0)) {
		stop("`nth` must be an integer of length 1.")
	}
	x <- node_to_list(x)
	lapply(x, function(y) {
		res <- y$child(nth)
		if (length(res) > 0) {
			res[[1]]
		} else {
			NULL
		}
	}) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_ancestors <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$ancestors()) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_children <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$children()) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_next <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$next_()[[1]]) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_next_all <- function(x) {
	check_is_rulelist_or_node(x)
	lapply(x, function(y) y$next_all()) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_prev <- function(x) {
	check_is_rulelist_or_node(x)
	x <- node_to_list(x)
	lapply(x, function(y) y$prev()[[1]]) |>
		add_rulelist_class()
}

#' @name node-traversal
#' @export
node_prev_all <- function(x) {
	check_is_rulelist_or_node(x)
	lapply(x, function(y) y$prev_all()) |>
		add_rulelist_class()
}

#' Change the code in the tree
#'
#' @description
#' `node_replace()` gives the replacement for a particular node.
#' `node_replace_all()` does the same but for several nodes (e.g. the output of
#' `node_find_all()`). The output of those functions can be passed to
#' `tree_rewrite()` to rewrite the entire input code with those replacements.
#'
#' @inheritParams node-range
#' @param ... Named elements where the name is a rule ID and the value is a
#' character string indicating the replacement to apply to nodes that match this
#' rule. Meta-variables are accepted but the syntax is different: they must be
#' wrapped in `~~`, e.g `"anyNA(~~VAR~~)"`.
#'
#' @name node-fix
#' @export
#'
#' @examples
#' src <- "
#' x <- c(1, 2, 3)
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
#' nodes_to_replace |>
#'   node_replace(
#'     any_na = "anyNA(~~VAR~~)",
#'     any_dup = "anyDuplicated(~~VAR~~) > 0"
#'   )
#'
#' ### Replace all nodes found by each rule
#'
#' nodes_to_replace <- root |>
#'   node_find(
#'     ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
#'     ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
#'   )
#'
#' nodes_to_replace |>
#'   node_replace(
#'     any_na = "anyNA(~~VAR~~)",
#'     any_dup = "anyDuplicated(~~VAR~~) > 0"
#'   )
node_replace <- function(x, ...) {
	check_is_rulelist_or_node(x)
	replacements <- list(...)

	out <- lapply(seq_along(x), function(y) {
		if (is.null(x[[y]])) {
			return(invisible())
		}
		id <- names(x)[y]
		repl <- replacements[[id]]
		meta_var <- get_meta_var(repl)

		res <- vapply(
			meta_var,
			function(mv) {
				matches <- node_get_match(x[[y]], mv)
				if (length(matches[[1]]) == 0) {
					matches <- x[[y]]$get_multiple_matches(mv)
					if (length(matches) == 0) {
						stop(
							"Couldn't get value for meta-variable `",
							mv,
							"`.\nAre you sure it exists in the rule `",
							id,
							"`?",
							call. = FALSE
						)
					}
					txts <- unlist(node_text_all(list(matches)))
					if (txts[length(txts)] == ",") {
						txts <- txts[-length(txts)]
					}
					txts <- gsub("^,$", ", ", txts)
					paste(txts, collapse = "")
				} else {
					node_text(matches)[[1]]
				}
			},
			character(1)
		)
		new_text <- repl
		if (length(res) > 0) {
			for (i in names(res)) {
				new_text <- gsub(paste0("~~", i, "~~"), res[i], new_text, fixed = TRUE)
			}
		}
		list(x[[y]]$replace(new_text))
	})
	out <- drop_null_elements(out)
	out <- unlist(out, recursive = FALSE)

	class(out) <- c("astgrep_replacement", class(out))
	out
}

#' @name node-fix
#' @export
node_replace_all <- function(x, ...) {
	check_all_nodes(x)
	replacements <- list(...)

	out <- lapply(seq_along(x), function(y) {
		if (is.null(x[[y]])) {
			return(invisible())
		}
		id <- names(x)[y]
		repl <- replacements[[id]]
		meta_var <- get_meta_var(repl)

		lapply(x[[y]], function(z) {
			res <- vapply(
				meta_var,
				function(mv) {
					mv_text <- node_text(node_get_match(z, mv))[[1]]

					if (is.null(mv_text)) {
						temp <- z$get_multiple_matches(mv)
						mv_text <- lapply(temp, function(nd) {
							out <- node_text(nd)
							if (out == ",") {
								out <- ", "
							}
							out
						}) |>
							unlist() |>
							paste(collapse = "")
						if (is.null(mv_text) || mv_text == "") {
							stop(
								"Couldn't get value for meta-variable `",
								mv,
								"`.\nAre you sure it exists in the rule `",
								id,
								"`?",
								call. = FALSE
							)
						}
					}
					mv_text
				},
				character(1)
			)
			new_text <- repl
			if (length(res) > 0) {
				for (i in names(res)) {
					new_text <- gsub(
						paste0("~~", i, "~~"),
						res[i],
						new_text,
						fixed = TRUE
					)
				}
			}
			z$replace(new_text)
		})
	})
	out <- drop_null_elements(out)
	out <- unlist(out, recursive = FALSE)

	class(out) <- c("astgrep_replacements", class(out))
	out
}

get_meta_var <- function(x) {
	meta_var <- regmatches(x, gregexpr("~~([A-Z0-9]+)~~", x))[[1]]
	gsub("~~", "", meta_var)
}
