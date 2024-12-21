# nocov start
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
# nocov end

#' `tinytest` helper
#'
#' @param label Snapshot name
#' @param current Code that outputs some text
#'
#' @export
#' @keywords internal
expect_snapshot <- function(label, current) {
	snapshot_file <- file.path("_snapshots", paste0(label, ".txt"))
	if (is.list(current)) {
		current2 <- paste(utils::capture.output(print(current)), collapse = "\n")
	} else {
		current2 <- paste(current, collapse = "\n")
	}
	if (!dir.exists(dirname(snapshot_file))) {
		dir.create(dirname(snapshot_file), showWarnings = FALSE, recursive = TRUE)
	}
	if (!file.exists(snapshot_file)) {
		cat(current2, file = snapshot_file, sep = "\n")
		message("Creating file ", snapshot_file)
		return(invisible())
	}
	target <- paste(readLines(snapshot_file, warn = FALSE), collapse = "\n")
	tinytest::tinytest(
		result = identical(current2, target),
		call = sys.call(sys.parent(1)),
		diff = paste0("Check content of ", snapshot_file)
	)
}
