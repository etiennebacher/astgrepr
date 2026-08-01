# Get more precise information on a node

Get more precise information on a node

## Usage

``` r
node_matches(x, ..., files = NULL)

node_inside(x, ..., files = NULL)

node_has(x, ..., files = NULL)

node_precedes(x, ..., files = NULL)

node_follows(x, ..., files = NULL)
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

A list containing as many elements as there are nodes as input.

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

some_node <- root |>
  node_find(ast_rule(pattern = "print($A)"))

node_text(some_node)
#> $rule_1
#> [1] "print('hi')"
#> 

# Check if a node matches a specific rule
some_node |>
  node_get_match("A") |>
  node_matches(ast_rule(kind = "argument"))
#> $rule_1
#> $rule_1[[1]]
#> [1] TRUE
#> 
#> 

# Check if a node is inside another one
some_node |>
  node_get_match("A") |>
  node_inside(ast_rule(kind = "call"))
#> [[1]]
#> [[1]][[1]]
#> [1] TRUE
#> 
#> 
```
