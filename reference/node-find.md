# Find node(s) matching a pattern

Those functions find one or several nodes based on some rule:

- `node_find()` returns the first node that is found;

- `node_find_all()` returns a list of all nodes found.

Some arguments (such as `kind`) require some knowledge of the
tree-sitter grammar of R. This grammar can be found here:
<https://github.com/r-lib/tree-sitter-r/blob/main/src/grammar.json>.

## Usage

``` r
node_find(x, ..., files = NULL)

node_find_all(x, ..., files = NULL)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

- ...:

  Any number of rules created with
  [`ast_rule()`](https://astgrepr.etiennebacher.com/reference/ast_rule.md).

- files:

  A vector of filenames containing rules. Those must be `.yaml` files.

## Value

`node_find()` returns a single `SgNode`.

`node_find_all()` returns a list of `SgNode`s.

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "any(duplicated($A))"))
#> <List of 1 rule>
#> |--rule_1: 1 node

root |>
  node_find_all(ast_rule(pattern = "any(duplicated($A))"))
#> <List of 1 rule>
#> |--rule_1: 2 nodes

# using the 'kind' of the nodes to find elements
src <- "
  a <- 1
  while (TRUE) { print('a') }
"

root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(kind = "while_statement"))
#> <List of 1 rule>
#> |--rule_1: 1 node

# one can pass several rules at once
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))
    while (TRUE) { print('a') }"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(
    ast_rule(pattern = "any(duplicated($A))"),
    ast_rule(kind = "while_statement")
  )
#> <List of 2 rules>
#> |--rule_1: 1 node
#> |--rule_2: 1 node

root |>
  node_find_all(
    ast_rule(pattern = "any(duplicated($A))"),
    ast_rule(kind = "while_statement")
  )
#> <List of 2 rules>
#> |--rule_1: 2 nodes
#> |--rule_2: 1 nodes
```
