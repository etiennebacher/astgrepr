
if (requireNamespace("tinytest", quietly=TRUE)){
  tinytest::test_package("astgrepr", testdir = "tinytest")
}

if(requireNamespace('spelling', quietly = TRUE)) {
  spelling::spell_check_test(vignettes = TRUE, error = FALSE, skip_on_cran = TRUE)
}
