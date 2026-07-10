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

# A transient Imperva bot challenge is tolerated as a clean no-op -- the next
# run almost always recovers -- but if the newest processed data is already
# this old, the pipeline hasn't truly succeeded in a long time (broken bypass
# or MWRA stopped publishing) and the run should fail loudly instead.
MAX_STALE_DAYS <- 14

# Handle an all-retries-challenged fetch: no false alarm while the data is
# reasonably fresh, hard failure once it goes stale. Called for both the
# update check (Step 1) and the PDF download (Step 2). Note: no quit() here;
# run_monitor.R is also source()d interactively.
handle_challenge <- function(where) {
  message("")
  message("MWRA served a bot-challenge page during the ", where, ".")
  set_gha_output("data_updated", "false")

  stale_days <- data_staleness_days()
  if (!is.na(stale_days) && stale_days > MAX_STALE_DAYS) {
    stop(sprintf(paste0(
      "Bot challenge encountered and the newest data is %.0f days old ",
      "(limit %d). Either the curl-impersonate bypass no longer works or ",
      "MWRA has stopped publishing -- needs a human look."),
      stale_days, MAX_STALE_DAYS))
  }

  message("Newest data is ",
          if (is.na(stale_days)) "of unknown age"
          else sprintf("%.0f day(s) old", stale_days),
          "; treating this as transient. The next run should recover.")
}

# Step 1: Check for updates
message("Step 1: Checking for updates...")
update_info <- check_for_updates()

if (identical(update_info$status, "challenge")) {
  handle_challenge("update check")
} else if (!is.null(update_info$error)) {
  message("ERROR: Check failed - ", update_info$error)
  set_gha_output("data_updated", "false")
  stop("Check failed")
} else {

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

    download <- download_pdf(update_info$full_pdf_url)

    if (identical(download$status, "challenge")) {
      # State was not advanced, so the next run re-detects this data and
      # retries the download.
      handle_challenge("PDF download")
    } else if (is.null(download$path)) {
      set_gha_output("data_updated", "false")
      stop("Download failed")
    } else {
      pdf_path <- download$path

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
  }
}

message("Finished at: ", Sys.time())
message("========================================")

# Clean up force variable so next source() starts fresh
rm(force)
