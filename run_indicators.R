#!/usr/bin/env Rscript
# Other Covid indicators: Mass. DPH cases and testing, CDC hospital data
#
# Produces data/processed/indicators_summary.json, the block of ready-made
# sentences the email prints under the wastewater chart. Kept entirely separate
# from the wastewater pipelines: its own modules, CSVs, and workflow.
#
# How a run works:
#   1. Try to fetch each of the three sources. Each stands alone -- one failing
#      doesn't stop the others, and a successful fetch rewrites that source's
#      CSV.
#   2. Build the summary from the CSVs on disk, whatever just happened. So a
#      source that failed today still appears, from its last good fetch, until
#      its newest week is too old to call current.
#
# To test the DPH parser without going through mass.gov's bot wall, point
# DPH_WORKBOOK_PATH at a workbook you downloaded by hand:
#   DPH_WORKBOOK_PATH=temp/covid-19-dashboard.xlsx CURL_IMPERSONATE_BIN=curl \
#     Rscript run_indicators.R

library(jsonlite)
library(readr)
library(readxl)

source("R/utils.R")
source("R/06_indicators.R")
source("R/07_dph.R")
source("R/08_cdc.R")

message("========================================")
message("Other Covid indicators (Mass. DPH, CDC)")
message("========================================")
message("Started at: ", Sys.time())
message("")

#' Fetch one source and store it, reporting rather than stopping on failure
#'
#' @param label Name for the log
#' @param fetch Function returning the source's data frame
#' @param save Function writing that data frame to its CSV
#' @return TRUE if the source was fetched and stored, FALSE otherwise
refresh_source <- function(label, fetch, save) {
  message("Fetching ", label, "...")
  tryCatch({
    data <- fetch()
    save(data)
    message(sprintf("  %d rows, newest week ending %s", nrow(data),
                    max(data$week_end)))
    TRUE
  }, error = function(e) {
    message("  Could not refresh ", label, ": ", conditionMessage(e))
    message("  Keeping the last stored copy, if there is one.")
    FALSE
  })
}

# A workbook saved by hand stands in for the download when this is set.
dph_override <- Sys.getenv("DPH_WORKBOOK_PATH")

fetched <- c(
  dph = refresh_source(
    "Mass. DPH workbook",
    function() {
      if (nzchar(dph_override)) {
        message("  Using local workbook: ", dph_override)
        extract_dph_series(validate_dph_workbook(dph_override))
      } else {
        extract_dph_series(fetch_dph_workbook())
      }
    },
    save_dph_data
  ),
  cdc_ed = refresh_source(
    "CDC emergency department visits", fetch_cdc_ed,
    function(data) save_cdc_data(data, CDC_ED_CSV_PATH)
  ),
  cdc_hosp = refresh_source(
    "CDC hospital admissions", fetch_cdc_hosp,
    function(data) save_cdc_data(data, CDC_HOSP_CSV_PATH)
  )
)
message("")

# Build the summary from what is stored, not from what was just fetched.
message("Building summary...")
result <- indicators_summary(
  dph = read_dph_data(),
  cdc_ed = read_cdc_ed_data(),
  cdc_hosp = read_cdc_hosp_data()
)

for (section in result$payload$sections) {
  message("  ", section$heading)
  for (line in section$lines) message("    - ", line)
}
if (length(result$payload$sections) == 0) {
  message("  No current figures from any source; the email will omit the block.")
}

written <- save_indicators_summary(result$payload)
message(if (written) "  Wrote " else "  No change to ", INDICATORS_SUMMARY_PATH)
message("")

# Sources with nothing current to show. The workflow turns a non-empty value
# into a single issue instead of failing the run: DPH stops updating every
# summer between seasons, and a daily red run for weeks would be noise.
if (length(result$unavailable) > 0) {
  message("No current data from: ", paste(result$unavailable, collapse = "; "))
}
set_gha_output("unavailable", paste(result$unavailable, collapse = "; "))
set_gha_output("newest_week_end",
               if (is.null(result$payload$newest_week_end)) ""
               else result$payload$newest_week_end)

message("Finished at: ", Sys.time())
message("========================================")

# Every source failing at once is not a quiet week -- it means the runner has
# no network, or something shared is broken -- so that one does fail the run.
if (!any(fetched)) {
  stop("None of the three sources could be fetched.")
}

rm(dph_override, fetched, result, written)
