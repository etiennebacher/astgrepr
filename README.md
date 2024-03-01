
<!-- README.md is generated from README.Rmd. Please edit that file -->

# astgrepr

<!-- badges: start -->
<!-- badges: end -->

`astgrepr` provides R bindings to the
[ast-grep](https://ast-grep.github.io/) Rust crate.

``` r
library(astgrepr)

x = astgrepr:::SgRoot$new("print('hello')\nlogger('hello', 'world', '!')", "python")
root = x$root()

root$range()
#> <pointer: 0x0000021761606370>
#> attr(,"class")
#> [1] "Range"

root$text()
#> [1] "print('hello')\nlogger('hello', 'world', '!')"
root$is_leaf()
#> [1] FALSE
root$kind()
#> [1] "module"
```
