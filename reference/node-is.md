# Get information on nodes

Get information on whether a node is a leaf (meaning that it doesn't
have any children) and whether it is named.

## Usage

``` r
node_is_leaf(x)

node_is_named(x)

node_is_named_leaf(x)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

## Value

A logical value.

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    x <- z + 1
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

node_is_leaf(root)
#> [[1]]
#> [1] FALSE
#> 

root |>
  node_find(ast_rule(pattern = "z")) |>
  node_is_leaf()
#> $rule_1
#> [1] TRUE
#> 

root |>
  node_find(ast_rule(pattern = "z")) |>
  node_is_named()
#> $rule_1
#> [1] TRUE
#> 
```
