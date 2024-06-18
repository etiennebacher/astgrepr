
<!-- README.md is generated from README.Rmd. Please edit that file -->

# astgrepr

<!-- badges: start -->
<!-- badges: end -->

`astgrepr` provides R bindings to the
[ast-grep](https://ast-grep.github.io/) Rust crate.

``` r
library(astgrepr)

src <- "library(tidyverse)
    x <- rnorm(100, mean = 2)
    any(duplicated(y))
    plot(x)
    any(duplicated(x))"

node <- astgrepr:::SgRoot$new(src)$root()

# get everything inside rnorm()
lapply(node$find(list(
  pattern = "rnorm($$$A)"
))$get_multiple_matches("A"), \(x) x$text())
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
lapply(
  node$find_all(list(pattern = "any(duplicated($A))")),
  \(x) x$text()
)
#> [[1]]
#> [1] "any(duplicated(y))"
#> 
#> [[2]]
#> [1] "any(duplicated(x))"
```
