# Get the root of the syntax tree

This function takes a tree created by
[`tree_new()`](https://astgrepr.etiennebacher.com/reference/tree_new.md)
and returns the root node containing all subsequent nodes.

## Usage

``` r
tree_root(x)
```

## Arguments

- x:

  A tree created by
  [`tree_new()`](https://astgrepr.etiennebacher.com/reference/tree_new.md).

## Value

A node corresponding to the root of the abstract syntax tree

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(x)
    any(duplicated(x))"

tree <- tree_new(src)
tree_root(tree)
#> <AST node>
```
