# Rewrite the tree with a list of replacements

Rewrite the tree with a list of replacements

## Usage

``` r
tree_rewrite(root, replacements)
```

## Arguments

- root:

  The root tree, obtained via
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)

- replacements:

  A list of replacements, obtained via
  [`node_replace()`](https://astgrepr.etiennebacher.com/reference/node-fix.md)
  or
  [`node_replace_all()`](https://astgrepr.etiennebacher.com/reference/node-fix.md).

## Value

A string character corresponding to the code used to build the tree root
but with replacements applied.

## Examples

``` r

src <- "x <- c(1, 2, 3)
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

fixes <- nodes_to_replace |>
  node_replace(
    any_na = "anyNA(~~VAR~~)",
    any_dup = "anyDuplicated(~~VAR~~) > 0"
  )

# original code
cat(src)
#> x <- c(1, 2, 3)
#> any(duplicated(x), na.rm = TRUE)
#> any(duplicated(x))
#> if (any(is.na(x))) {
#>   TRUE
#> }
#> any(is.na(y))

# new code
tree_rewrite(root, fixes)
#> x <- c(1, 2, 3)
#> any(duplicated(x), na.rm = TRUE)
#> anyDuplicated(x) > 0
#> if (anyNA(x)) {
#>   TRUE
#> }
#> any(is.na(y))


### Replace all nodes found by each rule

nodes_to_replace <- root |>
  node_find_all(
    ast_rule(id = "any_na", pattern = "any(is.na($VAR))"),
    ast_rule(id = "any_dup", pattern = "any(duplicated($VAR))")
  )

fixes <- nodes_to_replace |>
  node_replace_all(
    any_na = "anyNA(~~VAR~~)",
    any_dup = "anyDuplicated(~~VAR~~) > 0"
  )

# original code
cat(src)
#> x <- c(1, 2, 3)
#> any(duplicated(x), na.rm = TRUE)
#> any(duplicated(x))
#> if (any(is.na(x))) {
#>   TRUE
#> }
#> any(is.na(y))

# new code
tree_rewrite(root, fixes)
#> x <- c(1, 2, 3)
#> any(duplicated(x), na.rm = TRUE)
#> anyDuplicated(x) > 0
#> if (anyNA(x)) {
#>   TRUE
#> }
#> anyNA(y)
```
