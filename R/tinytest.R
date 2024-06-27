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

#' @export
expect_snapshot <- function(label, current) {
  snapshot_fn <- file.path("_snapshots", paste0(label, ".txt"))
  cal <- sys.call(sys.parent(5))
  if (!dir.exists(dirname(snapshot_fn))) {
    dir.create(dirname(snapshot_fn), showWarnings = FALSE, recursive = TRUE)
    info <- paste("Creating snapshot:", snapshot_fn)
    return(tinytest::tinytest(FALSE, call = cal, info = info))
  }
  if (!file.exists(snapshot_fn)) {
    cat(current, file = snapshot_fn)
    return(tinytest::tinytest(FALSE, call = cal, info = if (exists(info)) info))
  }
  target <- readLines(snapshot_fn, warn = FALSE)
  current <- strsplit(current, "\\n")[[1]]
  tinytest::tinytest(
    result = identical(current, target),
    call = sys.call(sys.parent(1)),
    diff = paste0("Check content of ", snapshot_fn)
  )
}
