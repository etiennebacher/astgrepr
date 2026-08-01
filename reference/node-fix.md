# Change the code in the tree

`node_replace()` gives the replacement for a particular node.
`node_replace_all()` does the same but for several nodes (e.g. the
output of
[`node_find_all()`](https://astgrepr.etiennebacher.com/reference/node-find.md)).
The output of those functions can be passed to
[`tree_rewrite()`](https://astgrepr.etiennebacher.com/reference/tree_rewrite.md)
to rewrite the entire input code with those replacements.

## Usage

``` r
node_replace(x, ...)

node_replace_all(x, ...)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

- ...:

  Named elements where the name is a rule ID and the value is a
  character string indicating the replacement to apply to nodes that
  match this rule. Meta-variables are accepted but the syntax is
  different: they must be wrapped in `~~`, e.g `"anyNA(~~VAR~~)"`.

## Value

A list where each element is the replacement for a piece of the code.
Each element is a list containing 3 sub-elements:

- the start position for the replacement

- the end position for the replacement

- the text used as replacement

## Examples

``` r
src <- "
x <- c(1, 2, 3)
any(duplicated(x), na.rm = TRUE)
any(duplicated(x))
if (any(is.na(x))) {
  TRUE
}
any(is.na(y))"

root <- tree_new(src) |>
  tree_root()


### Only replace the first nodes found by each rule

nodes_to_replace <- root |>
  node_find(
    ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
    ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
  )

nodes_to_replace |>
  node_replace(
    any_na = "anyNA(~~VAR~~)",
    any_dup = "anyDuplicated(~~VAR~~) > 0"
  )
#> [[1]]
#> [[1]][[1]]
#> [1] 73
#> 
#> [[1]][[2]]
#> [1] 86
#> 
#> [[1]][[3]]
#> [1] "anyNA(x)"
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] 50
#> 
#> [[2]][[2]]
#> [1] 68
#> 
#> [[2]][[3]]
#> [1] "anyDuplicated(x) > 0"
#> 
#> 
#> attr(,"class")
#> [1] "astgrep_replacement" "list"               

### Replace all nodes found by each rule

nodes_to_replace <- root |>
  node_find(
    ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
    ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
  )

nodes_to_replace |>
  node_replace(
    any_na = "anyNA(~~VAR~~)",
    any_dup = "anyDuplicated(~~VAR~~) > 0"
  )
#> [[1]]
#> [[1]][[1]]
#> [1] 73
#> 
#> [[1]][[2]]
#> [1] 86
#> 
#> [[1]][[3]]
#> [1] "anyNA(x)"
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] 50
#> 
#> [[2]][[2]]
#> [1] 68
#> 
#> [[2]][[3]]
#> [1] "anyDuplicated(x) > 0"
#> 
#> 
#> attr(,"class")
#> [1] "astgrep_replacement" "list"               
```
