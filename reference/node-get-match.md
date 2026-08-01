# Get the match(es) from a meta-variable

Those functions extract the content of the meta-variable specified in
[`node_find()`](https://astgrepr.etiennebacher.com/reference/node-find.md):

- `node_get_match()` is used when the meta-variable refers to a single
  pattern, e.g. `"plot($A)`;

- `node_get_multiple_matches()` is used when the meta-variable captures
  all elements in a pattern, e.g. `"plot($$$A)"`.

## Usage

``` r
node_get_match(x, meta_var)

node_get_multiple_matches(x, meta_var)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

- meta_var:

  The name given to one of the meta-variable(s) in
  [`node_find()`](https://astgrepr.etiennebacher.com/reference/node-find.md).

## Value

`node_get_match()` returns a list of depth 1, where each element is the
node corresponding to the `rule` passed (this can be of length 0 if no
node is matched). `node_get_multiple_matches()` also returns a list of
depth 1, but each element can contain multiple nodes when the
meta-variable captures all elements in a pattern.

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    plot(mtcars)"

root <- src |>
  tree_new() |>
  tree_root()

# we capture a single element with "$A" so node_get_match() can be used
root |>
  node_find(ast_rule(pattern = "plot($A)")) |>
  node_get_match("A")
#> <List of 1 rule>
#> |--rule_1: 1 node

# we can specify the variable to extract
root |>
  node_find(ast_rule(pattern = "rnorm($A, $B)")) |>
  node_get_match("B")
#> <List of 1 rule>
#> |--rule_1: 1 node

# we capture many elements with "$$$A" so node_get_multiple_matches() can
# be used here
root |>
  node_find(ast_rule(pattern = "rnorm($$$A)")) |>
  node_get_multiple_matches("A")
#> <List of 1 rule>
#> |--rule_1: 3 nodes
```
