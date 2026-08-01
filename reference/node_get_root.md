# Recover the tree root from a node

Recover the tree root from a node

## Usage

``` r
node_get_root(x)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

## Value

A list of two elements: `start` and `end`. Each of those is a vector
with two values indicating the row and column. Those are 0-indexed.

## Examples

``` r
src <- "
print('hi')
fn <- function() {
  print('hello')
}
"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "print($A)")) |>
  node_get_root() |>
  tree_root() |>
  node_text()
#> [[1]]
#> [1] "print('hi')\nfn <- function() {\n  print('hello')\n}\n"
#> 
```
