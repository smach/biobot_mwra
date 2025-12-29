#!/usr/bin/env Rscript
# Run all testthat tests for biobot_mwra project

# Required packages
required_packages <- c("testthat", "withr", "jsonlite", "rvest", "httr2",
                       "pdftools", "readr", "ggplot2", "scales", "xml2")

# Check and load packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}

library(testthat)

# Source all R files
message("Sourcing R files...")
source("R/utils.R")
source("R/01_check_updates.R")
source("R/02_download_pdf.R")
source("R/03_extract_data.R")
source("R/04_visualize.R")

# Run tests
message("\nRunning tests...\n")
test_dir(
  "tests/testthat",
  reporter = "progress",
  stop_on_failure = FALSE
)
