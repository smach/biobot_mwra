#!/usr/bin/env Rscript
# MWRA Biobot Data Monitor
# Checks for new COVID wastewater data and updates visualizations

# Set to TRUE to force update even when no new data
force <- FALSE

# Load packages
library(rvest)
library(httr2)
library(pdftools)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(jsonlite)

# Source helper functions
source("R/utils.R")
source("R/01_check_updates.R")
source("R/02_download_pdf.R")
source("R/03_extract_data.R")
source("R/04_visualize.R")

# --- Start monitoring ---

message("========================================")
message("MWRA Biobot Data Monitor")
message("========================================")
message("Started at: ", Sys.time())
if (force) message("FORCE MODE enabled")
message("")

# Step 1: Check for updates
message("Step 1: Checking for updates...")
update_info <- check_for_updates()

if (!is.null(update_info$error)) {
  message("ERROR: Check failed - ", update_info$error)
  set_gha_output("data_updated", "false")
  stop("Check failed")
}

message("  Current sample date: ", update_info$sample_date)
message("  Previous sample date: ",
        ifelse(is.null(update_info$previous_date), "None (first run)",
               update_info$previous_date))

if (!update_info$is_new && !force) {
  message("")
  message("No new data available. Exiting.")
  log_check()
  set_gha_output("data_updated", "false")
} else {
  # Continue with update
  message("")
  if (force && !update_info$is_new) {
    message("FORCING UPDATE")
  } else {
    message("NEW DATA AVAILABLE!")
  }
  message("")

  # Step 2: Download PDF
  message("Step 2: Downloading PDF...")
  message("  URL: ", update_info$full_pdf_url)

  pdf_path <- download_pdf(update_info$full_pdf_url)

  if (is.null(pdf_path)) {
    set_gha_output("data_updated", "false")
    stop("Download failed")
  }

  message("  Saved to: ", pdf_path)
  message("")

  # Step 3: Extract data
  message("Step 3: Extracting data from PDF...")
  data <- extract_and_save(pdf_path)
  message("  North system: ", nrow(data$north), " records")
  message("  South system: ", nrow(data$south), " records")
  message("")

  # Step 4: Generate visualizations
  message("Step 4: Generating visualizations...")
  generate_all_plots()
  message("")

  # Step 5: Copy data to docs folder for web dashboard
  message("Step 5: Updating web dashboard data...")
  if (!dir.exists("docs/data")) {
    dir.create("docs/data", recursive = TRUE)
  }
  file.copy("data/processed/combined_data.csv", "docs/data/combined_data.csv",
            overwrite = TRUE)
  message("  Copied data to docs/data/")
  message("")

  # Step 6: Update state
  message("Step 6: Updating state...")
  update_state(update_info$sample_date, update_info$pdf_url)
  message("  State file updated")

  set_gha_output("data_updated", "true")
  set_gha_output("sample_date", update_info$sample_date)

  message("")
  message("========================================")
  message("Pipeline completed successfully!")
}

message("Finished at: ", Sys.time())
message("========================================")

# Clean up force variable so next source() starts fresh
rm(force)
