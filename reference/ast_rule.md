# Build a rule

Rules are the core of `astgrepr`. Those are used to search for nodes and
are used in `node_match*()` and `node_find*()` functions. `ast_rule()`
is a very flexible function that allows one to build simple rules but
also much more complex and specific ones.

## Usage

``` r
ast_rule(
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
)
```

## Arguments

- pattern:

  The pattern to look for. This can be a string or an object of class
  `"astgrep_pattern_rule"` created by
  [`pattern_rule()`](https://astgrepr.etiennebacher.com/reference/pattern_rule.md).
  This can contain meta-variables to capture certain elements. Those
  meta-variables can then be recovered with
  [`node_get_match()`](https://astgrepr.etiennebacher.com/reference/node-get-match.md)
  and
  [`node_get_multiple_matches()`](https://astgrepr.etiennebacher.com/reference/node-get-match.md).
  The meta-variables must start with `$` and have only uppercase
  letters, e.g. `$VAR`.

- kind:

  The kind of nodes to look for.

- regex:

  A regex used to look for nodes. This must follow the syntax of the
  Rust [`regex` crate](https://docs.rs/regex/latest/regex/).

- inside:

  In which node should the node we look for be positioned? This can be
  another rule made with `ast_rule()` or an object of class
  `"astgrep_relational_rule"` created with
  [`relational_rule()`](https://astgrepr.etiennebacher.com/reference/relational_rule.md).

- has:

  Same input type as `inside`, but this looks for nodes that contain
  another type of node.

- precedes:

  Same input type as `inside`, but this looks for nodes that precede
  another type of node.

- follows:

  Same input type as `inside`, but this looks for node that follow
  another type of node.

- all:

  This takes one or a list of rules made with `ast_rule()`. It only
  matches nodes that respect all of the rules.

- any:

  This takes one or a list of rules made with `ast_rule()`. It matches
  nodes that respect any of the rules.

- not:

  This takes one or a list of rules made with `ast_rule()`. It excludes
  those nodes from the selection.

- matches:

  This takes the `id` of another rule. It is useful to reuse rules.

- id:

  The name of this rule. This can be reused in another rule with
  `matches`.

## Value

A list (possibly nested) with the class `"astgrep_rule"`.

## About meta-variables

Meta-variables allow us to capture some of the content in a pattern.
Usually, using `$` followed by an id in uppercase letters is enough:

    src <- "any(duplicated(x))"

    root <- src |>
      tree_new() |>
      tree_root()

    root |>
      node_find(ast_rule(pattern = "any(duplicated($A))"))
    #> <List of 1 rule>
    #> |--rule_1: 1 node

However, in some cases using `$` is a problem. For instance, if we want
to capture a column name coming after `$`, then we can't use `$` both as
code and as identifier.

    src <- "df$a"

    root <- src |>
      tree_new() |>
      tree_root()

    root |>
      node_find(ast_rule(pattern = "df$$A"))
    #> <List of 1 rule>
    #> |--rule_1: 0 node

In this situation, we can use `µ` instead:

    root |>
      node_find(ast_rule(pattern = "df$µA"))
    #> <List of 1 rule>
    #> |--rule_1: 1 node

## Examples

``` r
ast_rule(pattern = "print($A)")
#> <ast-grep rule: <unnamed>>
#> pattern: print($A)

ast_rule(
  pattern = "print($A)",
  inside = ast_rule(
    any = ast_rule(
      kind = c("for_statement", "while_statement")
    )
  )
)
#> <ast-grep rule: <unnamed>>
#> pattern: print($A)
#> inside:
#>   any:
#>     kind:
#>       - for_statement
#>       - while_statement
```
