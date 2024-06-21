test_all_astgrepr <- function() {
  tinytest::test_all(testdir = "tests/tinytest")
}

test_this_file <- function() {
  file <- rstudioapi::getSourceEditorContext()$path
  if (!grepl("tinytest/", file)) {
    message("Must run this when the active window is a test file.")
    return(invisible())
  }
  tinytest::run_test_file(file)
}
