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
# run almost always recovers -- but if the bypass hasn't gotten a clean page
# load through the bot wall in this long, it is genuinely broken (or the wall
# changed) and the run should fail loudly instead. Note this is deliberately
# NOT keyed off how old the *data* is: MWRA can pause publishing for weeks with
# a perfectly healthy bypass, and that pause is reported separately via the
# one-time stale-data issue below -- it must not turn every challenged run red.
MAX_STALE_DAYS <- 14

# Handle an all-retries-challenged fetch: no false alarm while the bypass is
# still getting through on other runs, hard failure once it has been shut out
# for MAX_STALE_DAYS. Called for both the update check (Step 1) and the PDF
# download (Step 2). Note: no quit() here; run_monitor.R is also source()d
# interactively.
handle_challenge <- function(where) {
  message("")
  message("MWRA served a bot-challenge page during the ", where, ".")
  set_gha_output("data_updated", "false")

  stale_days <- fetch_staleness_days()
  if (!is.na(stale_days) && stale_days > MAX_STALE_DAYS) {
    stop(sprintf(paste0(
      "Bot challenge encountered and MWRA's page has not loaded cleanly in ",
      "%.0f days (limit %d). The curl-impersonate bypass appears to be shut ",
      "out -- needs a human look."),
      stale_days, MAX_STALE_DAYS))
  }

  message("Bypass last got through ",
          if (is.na(stale_days)) "at an unknown time"
          else sprintf("%.0f day(s) ago", stale_days),
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

  # The page loaded cleanly (real content, not a challenge), so the bypass is
  # working right now. Record it -- committed on clean runs by the workflow --
  # so a later challenged run keeps quiet instead of tripping the hard-fail.
  record_successful_fetch()
  set_gha_output("fetch_ok", "true")

  message("  Current sample date: ", update_info$sample_date)
  message("  Previous sample date: ",
          ifelse(is.null(update_info$previous_date), "None (first run)",
                 update_info$previous_date))

  if (!update_info$is_new && !force) {
    message("")
    message("No new data available. Exiting.")
    log_check()
    set_gha_output("data_updated", "false")

    # A long publishing pause used to be invisible: the page loads, nothing is
    # new, the run exits cleanly, and nobody hears about it. That is exactly
    # what happened in July 2026, when MWRA stopped posting for weeks without
    # any signal from this pipeline. Flag it so the workflow can open a single
    # issue -- not a hard failure, because the pipeline itself is healthy.
    stale_days <- data_staleness_days()
    if (!is.na(stale_days) && stale_days > MAX_STALE_DAYS) {
      message(sprintf(
        "WARNING: MWRA's newest published data is %.0f days old (limit %d). They appear to have paused publishing.",
        stale_days, MAX_STALE_DAYS))
      set_gha_output("data_stale", "true")
      set_gha_output("stale_days", sprintf("%.0f", stale_days))
    }
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
