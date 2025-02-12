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
  if (!missing(txt) && (!all(is.character(txt)) || length(txt) != 1)) {
    stop("`txt` must a be a string of length 1.")
  }
  if (!missing(file)) {
    raw_txt <- readLines(file, warn = FALSE)
    txt <- paste(raw_txt, collapse = "\n")
  } else {
    raw_txt <- strsplit(txt, "\\n")[[1]]
  }
  HAS_TRAILING_NEW_LINE <- TRUE
  if (!grepl("\\\n$", txt)) {
    txt <- paste0(txt, "\n")
    HAS_TRAILING_NEW_LINE <- FALSE
  }
  out <- SgRoot$new(txt)
  attr(out, "lines_to_ignore") <- find_lines_to_ignore(raw_txt, ignore_tags)
  attr(out, "has_trailing_new_line") <- HAS_TRAILING_NEW_LINE
  out
}

find_lines_to_ignore <- function(raw_txt, ignore_tags) {
  if (length(ignore_tags) == 0) {
    return(NULL)
  } else if (length(ignore_tags) > 1) {
    ignore_pattern <- paste0("# (", paste(ignore_tags, collapse = "|"), ")")
  } else {
    ignore_pattern <- paste0("# ", ignore_tags)
  }

  single_lines_to_ignore <- grep(ignore_pattern, raw_txt, perl = TRUE)
  env_output <- new.env()

  for_all_rules <- list()
  for (i in single_lines_to_ignore) {
    txt <- raw_txt[i]
    if (!grepl(":", txt)) {
      for_all_rules[[length(for_all_rules) + 1]] <- i
    } else {
      rule_ids <- gsub(
        paste0("# (", paste(ignore_tags, collapse = "|"), "):"),
        "",
        txt
      )
      rule_ids <- strsplit(rule_ids, ",")[[1]]
      rule_ids <- trimws(rule_ids)
      for (rul in rule_ids) {
        env_output[[rul]] <- i
      }
    }
  }

  for (i in ignore_tags) {
    ignore_tags_start <- paste0("# ", i, "-start")
    ignore_tags_end <- paste0("# ", i, "-end")

    ignore_chunk_start <- grep(ignore_tags_start, raw_txt, perl = TRUE)
    ignore_chunk_end <- grep(ignore_tags_end, raw_txt, perl = TRUE)

    l_ignore_chunk_start <- length(ignore_chunk_start)
    l_ignore_chunk_end <- length(ignore_chunk_end)

    if (l_ignore_chunk_start != l_ignore_chunk_end) {
      stop(
        "Mismatch: the number of `start` patterns (",
        l_ignore_chunk_start,
        ") and of `end` patterns (",
        l_ignore_chunk_end,
        ") must be equal."
      )
    }

    if (l_ignore_chunk_start == 0 && l_ignore_chunk_end == 0) {
      next
    }

    for (i in seq_along(ignore_chunk_start)) {
      txt <- raw_txt[ignore_chunk_start[i]]
      if (!grepl(":", txt)) {
        for_all_rules <- append(
          for_all_rules,
          ignore_chunk_start[i]:ignore_chunk_end[i]
        )
      } else {
        rule_ids <- gsub(
          paste0("# (", paste(ignore_tags, collapse = "|"), ")-start:"),
          "",
          txt
        )
        rule_ids <- strsplit(rule_ids, ",")[[1]]
        rule_ids <- trimws(rule_ids)
        for (rul in rule_ids) {
          env_output[[rul]] <- ignore_chunk_start[i]:ignore_chunk_end[i]
        }
      }
    }
  }

  env_output[["all_rules"]] <- unlist(for_all_rules)

  # Make this 0-indexed for easier comparison with ast-grep output
  env_output <- eapply(env_output, function(x) {
    x - 1
  })
  env_output
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
  attr(out, "has_trailing_new_line") <- attr(x, "has_trailing_new_line")
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
  output <- root$commit_edits(replacements)
  new_txt <- output$new_content
  # https://github.com/ast-grep/ast-grep/issues/1345
  leading_newlines <- strrep("\n", root$range()[[1]][1])
  new_txt <- paste0(leading_newlines, new_txt)
  if (isFALSE(attributes(root)[["has_trailing_new_line"]])) {
    new_txt <- gsub("\\\n$", "", new_txt)
  }
  class(new_txt) <- c("astgrep_rewritten_tree", class(new_txt))
  attr(new_txt, "has_skipped_fixes") <- output$has_skipped_fixes
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
