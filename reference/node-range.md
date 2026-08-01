# Get the start and end positions of a node

Get the start and end positions of a node

## Usage

``` r
node_range(x)

node_range_all(x)
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
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(x)
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

node_range(root)
#> [[1]]
#> [[1]]$start
#> [1] 0 0
#> 
#> [[1]]$end
#> [1] 4 0
#> 
#> 

root |>
  node_find(ast_rule(pattern = "rnorm($$$A)")) |>
  node_range()
#> $rule_1
#> $rule_1$start
#> [1] 0 5
#> 
#> $rule_1$end
#> [1]  0 25
#> 
#> 

# There is also an "_all" variant when there are several nodes per rule
root |>
  node_find_all(
    ast_rule(pattern = "any(duplicated($A))"),
    ast_rule(pattern = "plot($A)")
  ) |>
  node_range_all()
#> $rule_1
#> $rule_1$node_1
#> $rule_1$node_1$start
#> [1] 1 4
#> 
#> $rule_1$node_1$end
#> [1]  1 22
#> 
#> 
#> $rule_1$node_2
#> $rule_1$node_2$start
#> [1] 3 4
#> 
#> $rule_1$node_2$end
#> [1]  3 22
#> 
#> 
#> 
#> $rule_2
#> $rule_2$node_1
#> $rule_2$node_1$start
#> [1] 2 4
#> 
#> $rule_2$node_1$end
#> [1]  2 11
#> 
#> 
#> 
```
