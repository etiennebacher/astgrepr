This is the first CRAN release of this package.

This is the third try. Thank you for the review, one of the comments received after the second try is adressed below.

> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace [...]
> -> tools/config.R; R/tinytest.R

The code in `tools/config.R` is internal only, it is used for the development of the package and is not exposed to the users.

Functions in `R/tinytest.R` that write to the user's working directory are not exported anymore as their usage is also internal only.
