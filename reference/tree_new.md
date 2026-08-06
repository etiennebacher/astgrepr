# Create a syntax tree

This function takes R code as string and creates the corresponding
abstract syntax tree (AST) from which we can query nodes.

## Usage

``` r
tree_new(txt, file, ignore_tags = "ast-grep-ignore")
```

## Arguments

- txt:

  A character string of length 1 containing the code to parse. If
  provided, `file` must not be provided.

- file:

  Path to file containing the code to parse. If provided, `txt` must not
  be provided.

- ignore_tags:

  Character vector indicating the tags to ignore. Default is
  `"ast-grep-ignore"`, meaning that any line that follows
  `# ast-grep-ignore` will be ignored in the output of `node_*()`
  functions.

## Value

An abstract syntax tree containing nodes

## Examples

``` r
src <- "x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(x)
    any(duplicated(x))"

tree_new(src)
#> <pointer: 0x558f5588e9e0>
#> attr(,"class")
#> [1] "SgRoot"
#> attr(,"lines_to_ignore")
#> attr(,"lines_to_ignore")$all_rules
#> numeric(0)
#> 
#> attr(,"has_trailing_new_line")
#> [1] FALSE
```
