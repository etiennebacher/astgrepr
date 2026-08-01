# Find the kind of a node

Find the kind of a node

## Usage

``` r
node_kind(x)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

## Value

A list with as many elements as in the input. Each element is a
character value.

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    x <- z + 1
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "any(duplicated($VAR))")) |>
  node_kind()
#> $rule_1
#> [1] "call"
#> 

root |>
  node_find(ast_rule(pattern = "$X + $VALUE")) |>
  node_kind()
#> $rule_1
#> [1] "binary_operator"
#> 
```
