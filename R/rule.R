#' Build a rule
#'
#' Rules are the core of `astgrepr`. Those are used to search for nodes and
#' are used in `node_match*()` and `node_find*()` functions. `ast_rule()` is
#' a very flexible function that allows one to build simple rules but also much
#' more complex and specific ones.
#'
#' @param pattern The pattern to look for. This can be a string or an object of
#' class `"astgrep_pattern_rule"` created by `pattern_rule()`.
#' @param kind The kind of nodes to look for.
#' @param regex A regex used to look for nodes. This must follow the syntax of
#' the Rust [`regex` crate](https://docs.rs/regex/latest/regex/).
#' @param inside In which node should the node we look for be positioned? This
#' can be another rule made with `ast_rule()` or an object of class
#' `"astgrep_relational_rule"` created with `relational_rule()`.
#' @param has Same input type as `inside`, but this looks for nodes that contain
#' another type of node.
#' @param precedes Same input type as `inside`, but this looks for nodes that
#' precede another type of node.
#' @param follows Same input type as `inside`, but this looks for node that
#' follow another type of node.
#' @param all This takes one or a list of rules made with `ast_rule()`.
#' It only matches nodes that respect all of the rules.
#' @param any This takes one or a list of rules made with `ast_rule()`.
#' It matches nodes that respect any of the rules.
#' @param not This takes one or a list of rules made with `ast_rule()`.
#' It excludes those nodes from the selection.
#' @param matches This takes the `id` of another rule. It is useful to
#' reuse rules.
#' @param id The name of this rule. This can be reused in another rule with
#' `matches`.
#'
#' @return A list (possibly nested) with the class `"astgrep_rule"`.
#' @export
ast_rule <- function(
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
    matches = NULL,
    id = NULL
  ) {
  check_all_null(pattern, kind, regex, inside, has, precedes, follows, all, any, not, matches)
  assert_string_or_pattern_rule(pattern)
  checkmate::assert_string(kind, null.ok = TRUE)
  checkmate::assert_string(regex, null.ok = TRUE)
  assert_relational_rule(inside)
  assert_relational_rule(has)
  assert_relational_rule(precedes)
  assert_relational_rule(follows)
  assert_ast_rule(all)
  assert_ast_rule(any)
  assert_ast_rule(not)
  checkmate::assert_string(matches, null.ok = TRUE)
  checkmate::assert_string(id, null.ok = TRUE)

  out <- list(
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

  class(out) <- c("astgrep_rule", class(out))
  out
}

#' Build a pattern rule
#'
#' This is a specific type of rule. It can be used in the more general ruleset
#' built with `ast_rule()`.
#'
#' @param selector Defines the surrounding code that helps to resolve any
#' ambiguity in the syntax.
#' @param context Defines the sub-syntax node kind that is the actual matcher
#' of the pattern.
#' @param strictness Optional, defines how strictly pattern will match against
#' nodes. See 'Details'.
#'
#' @details
#' The `strictness` parameter defines the type of nodes the `ast-grep` matcher
#' should consider. It has the following values:
#'
#' * `cst`: All nodes in the pattern and target code must be matched. No node
#'   is skipped.
#' * `smart`: All nodes in the pattern must be matched, but it will skip unnamed
#'   nodes in target code. This is the default behavior.
#' * `ast`: Only named AST nodes in both pattern and target code are matched.
#'   All unnamed nodes are skipped.
#' * `relaxed`: Named AST nodes in both pattern and target code are matched.
#'   Comments and unnamed nodes are ignored.
#' * `signature`: Only named AST nodes' kinds are matched. Comments, unnamed
#'   nodes and text are ignored.
#'
#' More information: <https://ast-grep.github.io/guide/rule-config/atomic-rule.html#pattern-object>
#'
#' @export
pattern_rule <- function(selector = NULL, context = NULL, strictness = "smart") {
  checkmate::assert_choice(
    strictness,
    choices = c("cst", "smart", "ast", "relaxed", "signature"),
    null.ok = TRUE
  )
  out <- list(selector = selector, context = context, strictness = strictness)
  class(out) <- c("astgrep_pattern_rule", class(out))
  out
}

#' Build a relational rule
#'
#' @param stopBy todo
#' @param field todo
#' @param regex todo
#'
#' @export
relational_rule <- function(stopBy = "neighbor", field = NULL, regex = NULL) {
  checkmate::assert_choice(
    stopBy,
    choices = c("end", "neighbor"),
    null.ok = TRUE
  )
  out <- list(stopBy = stopBy, field = field, regex = regex)
  class(out) <- c("astgrep_relational_rule", class(out))
  out
}


check_all_null <- function(pattern, kind, regex, inside, has, precedes, follows, all, any, not, matches) {
  my_args <- list(pattern, kind, regex, inside, has, precedes, follows, all, any, not, matches)
  n_non_nulls <- sum(vapply(my_args, function(x) !is.null(x), FUN.VALUE = logical(1L)))
  if (n_non_nulls == 0) {
    stop("`ast_rule()` requires at least one non-NULL parameter.")
  }
}

check_string_or_pattern_rule <- function(x) {
  checkmate::check_string(x, null.ok = TRUE) || inherits(x, "astgrep_pattern_rule")
}
assert_string_or_pattern_rule <- checkmate::makeAssertionFunction(check_string_or_pattern_rule)

check_relational_rule <- function(x) {
  is.null(x) || inherits(x, "astgrep_relational_rule")
}
assert_relational_rule <- checkmate::makeAssertionFunction(check_relational_rule)

check_ast_rule <- function(x) {
  is.null(x) || inherits(x, "astgrep_rule")
}
assert_ast_rule <- checkmate::makeAssertionFunction(check_ast_rule)
