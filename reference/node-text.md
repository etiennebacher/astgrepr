# Extract the code corresponding to one or several nodes

Those functions extract the code corresponding to the node(s):

- `node_text()` applies on a single node, for example the output of
  [`node_get_match()`](https://astgrepr.etiennebacher.com/reference/node-get-match.md)

- `node_text_all()` applies on a list of nodes, for example the output
  of
  [`node_get_multiple_matches()`](https://astgrepr.etiennebacher.com/reference/node-get-match.md)

## Usage

``` r
node_text(x)

node_text_all(x)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

## Value

A list with as many elements as there are in the input. Each element is
a list itself with the text corresponding to the input.

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(mtcars)
    any(duplicated(x))"

root <- src |>
  tree_new() |>
  tree_root()

# node_text() must be applied on single nodes
root |>
  node_find(ast_rule(pattern = "plot($A)")) |>
  node_text()
#> $rule_1
#> [1] "plot(mtcars)"
#> 

# node_find_all() returns a list on nodes on which
# we can use node_text_all()
root |>
  node_find_all(ast_rule(pattern = "any(duplicated($A))")) |>
  node_text_all()
#> $rule_1
#> $rule_1$node_1
#> [1] "any(duplicated(y))"
#> 
#> $rule_1$node_2
#> [1] "any(duplicated(x))"
#> 
#> 
```
