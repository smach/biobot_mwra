#!/usr/bin/env Rscript
# WastewaterSCAN COVID Monitor (Boston / Deer Island)
#
# A second source for the same sewershed the Biobot pipeline covers, kept
# deliberately separate from it: different lab method, different units, its
# own state file, its own workflow. See R/05_wwscan.R for the data contract.

# Set to TRUE to rewrite outputs even when there is no new sample
force <- FALSE

library(jsonlite)
library(readr)

source("R/utils.R")
source("R/05_wwscan.R")

message("========================================")
message("WastewaterSCAN COVID Monitor (Boston)")
message("========================================")
message("Started at: ", Sys.time())
if (force) message("FORCE MODE enabled")
message("")

# A failed fetch is tolerated as a quiet no-op while our stored data is still
# reasonably fresh, because a single network blip should not page anyone. Once
# the newest sample we hold is this old, though, something is genuinely wrong
# -- the bucket moved, or WastewaterSCAN stopped publishing -- and the run
# should fail loudly. Mirrors the MWRA pipeline's MAX_STALE_DAYS handling.
MAX_STALE_DAYS <- 14

wwscan_staleness <- function() {
  data_staleness_days(load_state(WWSCAN_STATE_FILE))
}

#' Report a fetch failure as transient or fatal depending on data age
handle_fetch_failure <- function(err) {
  message("")
  message("Could not read the WastewaterSCAN feed: ", conditionMessage(err))
  set_gha_output("data_updated", "false")

  stale_days <- wwscan_staleness()
  if (is.na(stale_days) || stale_days > MAX_STALE_DAYS) {
    stop(sprintf(paste0(
      "WastewaterSCAN fetch failed and the newest stored sample is %s ",
      "(limit %d days). The JSON feed may have moved -- see the README for ",
      "how to re-derive its URL from the dashboard."),
      if (is.na(stale_days)) "missing entirely" else
        sprintf("%.0f days old", stale_days),
      MAX_STALE_DAYS))
  }

  message(sprintf(
    "Newest stored sample is %.0f day(s) old; treating this as transient.",
    stale_days))
}

# Step 1: Fetch the feed
message("Step 1: Fetching WastewaterSCAN data...")
message("  URL: ", wwscan_plant_url())

payload <- tryCatch(fetch_wwscan_plant(), error = function(e) e)

if (inherits(payload, "error")) {
  handle_fetch_failure(payload)
} else {

  message("  Feed last updated: ", if (is.null(payload$updated)) "unknown"
          else payload$updated)
  message("  Site: ", payload$plant$name, " (", payload$plant$site_name, ")")
  message("")

  # Step 2: Extract the COVID series
  message("Step 2: Extracting SARS-CoV-2 (", WWSCAN_COVID_TARGET, ") series...")
  covid <- extract_wwscan_target(payload)
  message("  ", nrow(covid), " samples from ", min(covid$date), " to ",
          max(covid$date))

  latest_date <- max(covid$date)
  previous <- load_state(WWSCAN_STATE_FILE)$last_sample_date

  message("  Latest sample date: ", latest_date)
  message("  Previous sample date: ",
          if (is.null(previous)) "None (first run)" else previous)
  message("")

  # A live feed whose newest sample has gone stale is the failure the whole
  # exercise exists to catch, so say so loudly even though the fetch worked.
  feed_age <- as.numeric(Sys.Date() - latest_date)
  if (feed_age > MAX_STALE_DAYS) {
    set_gha_output("data_updated", "false")
    stop(sprintf(paste0(
      "WastewaterSCAN's newest Boston sample is %.0f days old (limit %d). ",
      "They appear to have stopped publishing for this site."),
      feed_age, MAX_STALE_DAYS))
  }

  is_new <- is.null(previous) || latest_date > as.Date(previous)

  if (!is_new && !force) {
    message("No new samples since the last run. Exiting.")
    set_gha_output("data_updated", "false")
  } else {
    if (force && !is_new) {
      message("FORCING UPDATE")
    } else {
      message("NEW DATA AVAILABLE!")
    }
    message("")

    # Step 3: Write the CSV
    message("Step 3: Writing data...")
    save_wwscan_data(covid)
    message("  Wrote ", WWSCAN_CSV_PATH)

    if (!dir.exists("docs/data")) {
      dir.create("docs/data", recursive = TRUE)
    }
    file.copy(WWSCAN_CSV_PATH, "docs/data/wwscan_covid.csv", overwrite = TRUE)
    message("  Copied data to docs/data/")
    message("")

    # Step 4: Read WastewaterSCAN's own level and trend, then summarize
    message("Step 4: Reading WastewaterSCAN's published level and trend...")

    # A failure here is not fatal: the measurements are already written and the
    # chart is still worth publishing. But it must never fall back to a trend
    # of our own invention, so the headline says the trend is unavailable and
    # the log says why.
    categories <- tryCatch(fetch_wwscan_categories(), error = function(e) e)

    if (inherits(categories, "error")) {
      message("  WARNING: could not read the categories file: ",
              conditionMessage(categories))
      message("  The headline will report the trend as unavailable.")
      categories <- list()
    }

    status <- wwscan_published_status(categories)

    if (status$available) {
      message("  WastewaterSCAN says: ", status$direction,
              " / ", wwscan_trend_detail(status))
      # Their verdict is published per sample date. If it lags the feed we'd be
      # pairing a fresh number with a stale judgement, so say so out loud.
      if (!is.na(status$as_of) && status$as_of != latest_date) {
        message("  NOTE: their trend covers ", status$as_of,
                " but our newest sample is ", latest_date)
      }
    } else {
      message("  WastewaterSCAN has no usable trend for ", WWSCAN_COVID_TARGET)
    }

    info <- wwscan_summary(covid, status)
    change_text <- wwscan_change_sentence(info)

    message("  ", info$headline)
    message("  ", change_text)

    save_wwscan_summary(info)
    message("  Wrote docs/data/wwscan_summary.json")
    message("")

    # Step 5: Update state
    message("Step 5: Updating state...")
    update_wwscan_state(latest_date, payload$updated)
    message("  State file updated")

    set_gha_output("data_updated", "true")
    set_gha_output("sample_date", format(latest_date, "%Y-%m-%d"))
    set_gha_output("headline", info$headline)
    set_gha_output("level", info$level)
    set_gha_output("trend_direction", info$trend$direction)
    set_gha_output("trend_detail", wwscan_trend_detail(info$trend))
    set_gha_output("latest_value", sprintf("%.1f", info$latest_value))
    set_gha_output("change_text", change_text)

    message("")
    message("========================================")
    message("Pipeline completed successfully!")
  }
}

message("Finished at: ", Sys.time())
message("========================================")

rm(force)
