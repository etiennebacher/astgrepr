# Navigate the tree

This is a collection of functions used to navigate the tree. Some of
them have a variant that applies on a single node (e.g. `node_next()`)
and one that applies on a list of nodes (e.g. `node_next_all()`):

- `node_prev()`, `node_prev_all()`, `node_next()`, and `node_next_all()`
  get the previous and next node(s) that are at the same depth as the
  current node;

- `node_parent()`, `node_ancestors()`, `node_child()` and
  `node_children()` get the node(s) that are above or below the current
  node in terms of depth. All nodes except the root node have at least
  one node (the root).

## Usage

``` r
node_parent(x)

node_child(x, nth)

node_ancestors(x)

node_children(x)

node_next(x)

node_next_all(x)

node_prev(x)

node_prev_all(x)
```

## Arguments

- x:

  A node, either from
  [`tree_root()`](https://astgrepr.etiennebacher.com/reference/tree_root.md)
  or from another `node_*()` function.

- nth:

  Integer. The child node to find. This is 0-indexed, so setting
  `nth = 0` gets the first child.

## Value

A node

## Examples

``` r

### get the previous/next node ---------------------------

src <- "
print('hi there')
a <- 1
fn <- function(x) {
  x + 1
}
"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "a <- $A")) |>
  node_prev() |>
  node_text()
#> $rule_1
#> [1] "print('hi there')"
#> 

root |>
  node_find(ast_rule(pattern = "a <- $A")) |>
  node_next() |>
  node_text()
#> $rule_1
#> [1] "fn <- function(x) {\n  x + 1\n}"
#> 

# there are nodes inside the function, but there are no more nodes on the
# same level as "fn"
root |>
  node_find(ast_rule(pattern = "a <- $A")) |>
  node_next_all() |>
  node_text_all()
#> $rule_1
#> $rule_1[[1]]
#> [1] "fn <- function(x) {\n  x + 1\n}"
#> 
#> 


### get the parent/child node ---------------------------

src <- "
print('hi there')
a <- 1
fn <- function(x) {
  x + 1
}
"
root <- src |>
  tree_new() |>
  tree_root()

root |>
  node_find(ast_rule(pattern = "$VAR + 1")) |>
  node_parent() |>
  node_text()
#> $rule_1
#> [1] "{\n  x + 1\n}"
#> 

root |>
  node_find(ast_rule(pattern = "$VAR + 1")) |>
  node_ancestors() |>
  node_text_all()
#> $rule_1
#> $rule_1[[1]]
#> [1] "{\n  x + 1\n}"
#> 
#> $rule_1[[2]]
#> [1] "function(x) {\n  x + 1\n}"
#> 
#> $rule_1[[3]]
#> [1] "fn <- function(x) {\n  x + 1\n}"
#> 
#> $rule_1[[4]]
#> [1] "print('hi there')\na <- 1\nfn <- function(x) {\n  x + 1\n}\n"
#> 
#> 

root |>
  node_find(ast_rule(pattern = "$VAR + 1")) |>
  node_child(0) |>
  node_text()
#> $rule_1
#> [1] "x"
#> 

root |>
  node_find(ast_rule(pattern = "$VAR + 1")) |>
  node_children() |>
  node_text_all()
#> $rule_1
#> $rule_1[[1]]
#> [1] "x"
#> 
#> $rule_1[[2]]
#> [1] "+"
#> 
#> $rule_1[[3]]
#> [1] "1"
#> 
#> 
```
