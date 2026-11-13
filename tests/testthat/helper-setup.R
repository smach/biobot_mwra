# Helper file for testthat - sources all R files

# Find project root (two levels up from tests/testthat)
find_project_root <- function() {
  # Try multiple approaches to find project root
  if (file.exists("R/utils.R")) {
    return(getwd())
  }
  if (file.exists("../../R/utils.R")) {
    return(normalizePath("../.."))
  }
  if (file.exists("../R/utils.R")) {
    return(normalizePath(".."))
  }
  # Fallback: look for .git directory
  path <- getwd()
  while (path != dirname(path)) {
    if (file.exists(file.path(path, "R", "utils.R"))) {
      return(path)
    }
    path <- dirname(path)
  }
  stop("Could not find project root")
}

PROJECT_ROOT <- find_project_root()

# Source all R files in correct order
source(file.path(PROJECT_ROOT, "R", "utils.R"))
source(file.path(PROJECT_ROOT, "R", "01_check_updates.R"))
source(file.path(PROJECT_ROOT, "R", "02_download_pdf.R"))
source(file.path(PROJECT_ROOT, "R", "03_extract_data.R"))
source(file.path(PROJECT_ROOT, "R", "04_visualize.R"))
source(file.path(PROJECT_ROOT, "R", "05_wwscan.R"))
source(file.path(PROJECT_ROOT, "R", "06_indicators.R"))
source(file.path(PROJECT_ROOT, "R", "07_dph.R"))
source(file.path(PROJECT_ROOT, "R", "08_cdc.R"))

# Helper function to get test fixtures path
test_fixture_path <- function(...) {
  file.path(PROJECT_ROOT, "tests", "testthat", "fixtures", ...)
}

# Stub impersonate_fetch so it "downloads" a local file, and run with_retry's
# closure exactly once. Restores both when the calling frame exits. Shared by
# the DPH and CDC tests; test-wwscan.R keeps its own identical copy.
local_fetch_stubs <- function(source_file, frame = parent.frame()) {
  original_fetch <- impersonate_fetch
  original_retry <- with_retry

  assign("impersonate_fetch", function(url, output_path = NULL, ...) {
    if (is.null(output_path)) {
      return(paste(readLines(source_file, warn = FALSE), collapse = "\n"))
    }
    file.copy(source_file, output_path, overwrite = TRUE)
    invisible(output_path)
  }, envir = .GlobalEnv)

  assign("with_retry", function(fn, ...) fn(), envir = .GlobalEnv)

  withr::defer({
    assign("impersonate_fetch", original_fetch, envir = .GlobalEnv)
    assign("with_retry", original_retry, envir = .GlobalEnv)
  }, envir = frame)
}
