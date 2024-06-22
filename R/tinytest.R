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

expect_snapshot <- function(label, current) {
  snapshot_fn <- file.path("_snapshots", paste0(label, ".txt"))
  cal <- sys.call(sys.parent(1))
  diff <- info <- NA_character_
  success <- TRUE
  if (!dir.exists(dirname(snapshot_fn))) {
    dir.create(dirname(snapshot_fn), showWarnings = FALSE, recursive = TRUE)
    info <- paste("Creating snapshot:", snapshot_fn)
    return(tinytest::tinytest(FALSE, call = cal, info = info))
  }
  if (!file.exists(snapshot_fn)) {
    cat(current, file = snapshot_fn)
    return(tinytest::tinytest(FALSE, call = cal, info = info))
  }
  target <- readLines(snapshot_fn, warn = FALSE)
  target <- paste(target, collapse = "\n")
  do <- suppressWarnings(diffobj::diffPrint(current, target))
  if (suppressWarnings(any(do))) {
    success <- FALSE
    diff <- paste(as.character(do), collapse = "\n")
    info <- paste("Snapshot:", snapshot_fn)
  }
  tinytest::tinytest(result = success, call = cal, info = info, diff = diff)
}
