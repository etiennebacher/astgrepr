
<!-- README.md is generated from README.Rmd. Please edit that file -->

# astgrepr

<!-- badges: start -->

[![R-CMD-check](https://github.com/etiennebacher/astgrepr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/etiennebacher/astgrepr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`astgrepr` provides R bindings to the
[ast-grep](https://ast-grep.github.io/) Rust crate. `ast-grep` is a tool
to parse the abstract syntax tree (AST) of some code and to perform
search and rewrite of code. This is extremely useful to build linters,
stylers, and perform a lot of code analysis.

Since `astgrepr` can be used as a low-level foundation for other tools
(such as linters), it doesn’t rely on any R dependencies (but has Rust
dependencies brought in by `ast-grep`).

## Demo

``` r

library(astgrepr)

src <- "library(tidyverse)
    x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(x)
    any(duplicated(x))"

node <- src |> 
  tree_new() |> 
  tree_root()

# get everything inside rnorm()
node |> 
  node_find(pattern = "rnorm($$$A)") |> 
  node_get_multiple_matches("A") |> 
  node_text_all()
#> [[1]]
#> [1] "100"
#> 
#> [[2]]
#> [1] ","
#> 
#> [[3]]
#> [1] "mean = 2"
```

``` r

# find occurrences of any(duplicated())
node |> 
  node_find_all(pattern = "any(duplicated($A))") |> 
  node_text_all()
#> [[1]]
#> [1] "any(duplicated(y))"
#> 
#> [[2]]
#> [1] "any(duplicated(x))"
```

## Related tools

There is some recent work linking `tree-sitter` and R. Those are not
competing with `astgrepr` but are rather a complement to it:

- [`r-lib/tree-sitter-r`](https://github.com/r-lib/tree-sitter-r):
  provide the R grammer to be used with tools built on `tree-sitter`.
  `astgrepr` relies on this grammar under the hood.
- [`DavisVaughan/r-tree-sitter`](https://github.com/DavisVaughan/r-tree-sitter):
  a companion of `r-lib/tree-sitter-r`. This gives a way to get the
  tree-sitter representation of some code directly in R. This is useful
  to learn how tree-sitter represents the R grammar, which is required
  if you want advanced use of `astgrepr`. However, it doesn’t provide a
  way to easily select specific nodes (e.g. based on patterns).
