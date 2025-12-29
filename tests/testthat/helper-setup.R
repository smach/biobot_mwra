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

# Helper function to get test fixtures path
test_fixture_path <- function(...) {
  file.path(PROJECT_ROOT, "tests", "testthat", "fixtures", ...)
}
