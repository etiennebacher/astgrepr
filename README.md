
<!-- README.md is generated from README.Rmd. Please edit that file -->

# astgrepr

<!-- badges: start -->
<!-- badges: end -->

`astgrepr` provides R bindings to the
[ast-grep](https://ast-grep.github.io/) Rust crate.

``` r
library(astgrepr)

x = astgrepr:::SgRoot$new("print('hello')\nlogger('hello', 'world', '!')", "python")
node = x$root()

node$matches(list(kind = "call"))
#> [1] FALSE
  
node$find(list(pattern = "print($A)"))$get_match("A")$text()
#> [1] "'hello'"
```
