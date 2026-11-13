# CDC emergency department and hospital admission data for Massachusetts
#
# The DPH workbook has no hospital figures, so these two come from CDC's open
# data portal (data.cdc.gov, a Socrata site). Unlike every other feed in this
# repo, these are documented public APIs: no bot wall, no token needed for the
# handful of requests we make. CDC refreshes both on Fridays with the week that
# ended the previous Saturday.
#
#   rdmq-nq56  NSSP: percent of emergency department visits with a Covid
#              diagnosis, plus CDC's own trend label for that series
#   ua7e-t2fy  NHSN: weekly new hospital admissions with confirmed Covid
#
# The trend label is CDC's verdict and is repeated as-is. We do not compute a
# trend of our own for either series -- see "Do not reintroduce a local trend
# test" in CLAUDE.md for why this repo reads verdicts instead of making them.
#
# Both fetches go through impersonate_fetch(), the single network entry point,
# even though nothing here needs a browser fingerprint.

CDC_API_BASE <- "https://data.cdc.gov/resource/"

CDC_ED_DATASET <- "rdmq-nq56"
CDC_HOSP_DATASET <- "ua7e-t2fy"

# Dataset landing pages, used as the source credit in the summary.
CDC_ED_PAGE_URL <- paste0(
  "https://data.cdc.gov/Public-Health-Surveillance/",
  "NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56"
)
CDC_HOSP_PAGE_URL <- paste0(
  "https://data.cdc.gov/Public-Health-Surveillance/",
  "Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/ua7e-t2fy"
)

CDC_ED_CSV_PATH <- "data/processed/cdc_ed_covid.csv"
CDC_HOSP_CSV_PATH <- "data/processed/cdc_hosp_covid.csv"

# The ED dataset labels state names in full; the hospital one uses postal codes.
CDC_ED_GEOGRAPHY <- "Massachusetts"
CDC_HOSP_JURISDICTION <- "MA"

# Enough rows for several years of weekly data.
CDC_ROW_LIMIT <- 500

#' Build a Socrata query URL
#'
#' Names and values are percent-encoded, so the `$` in `$select` and the space
#' in `week_end DESC` survive both the shell and the server.
#'
#' @param dataset Socrata dataset id, e.g. "rdmq-nq56"
#' @param params Named list or character vector of query parameters
#' @return Full URL
cdc_query_url <- function(dataset, params) {
  encode <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
  query <- paste(vapply(names(params), encode, character(1)),
                 vapply(params, encode, character(1)),
                 sep = "=", collapse = "&")
  paste0(CDC_API_BASE, dataset, ".json?", query)
}

#' Fetch rows from a CDC dataset
#'
#' @param url URL from cdc_query_url()
#' @param required Fields that must appear in the response
#' @return Data frame of character columns
#' @details Stops when the response isn't a non-empty JSON array carrying the
#'   required fields, which is the signal the dataset was renamed or retired.
fetch_cdc_rows <- function(url, required) {
  json_path <- tempfile(fileext = ".json")
  on.exit(unlink(json_path), add = TRUE)

  with_retry(function() {
    impersonate_fetch(url, output_path = json_path)

    rows <- tryCatch(
      jsonlite::fromJSON(json_path),
      error = function(e) {
        stop(sprintf("CDC response was not valid JSON: %s", e$message))
      }
    )

    if (!is.data.frame(rows) || nrow(rows) == 0) {
      stop("CDC returned no rows -- the dataset may have moved or been retired.")
    }

    missing <- setdiff(required, names(rows))
    if (length(missing) > 0) {
      stop("CDC response is missing field(s): ", paste(missing, collapse = ", "))
    }

    rows
  }, max_attempts = 3)
}

#' Pull a column that Socrata may have left out
#'
#' Socrata drops a field from a row when its value is null, so a column that is
#' empty for every row returned doesn't appear at all.
cdc_column <- function(rows, name) {
  if (name %in% names(rows)) rows[[name]] else rep(NA_character_, nrow(rows))
}

#' Convert a Socrata timestamp ("2026-11-07T00:00:00.000") to a Date
cdc_date <- function(x) {
  as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d")
}

#' Sort a series by week and drop rows without a date
cdc_tidy <- function(data) {
  data <- data[!is.na(data$week_end), , drop = FALSE]
  data <- data[order(data$week_end), , drop = FALSE]
  rownames(data) <- NULL
  data
}

#' Fetch the Massachusetts emergency department series
#'
#' @return Data frame: week_end, ed_pct_covid, ed_trend
fetch_cdc_ed <- function() {
  url <- cdc_query_url(CDC_ED_DATASET, list(
    geography = CDC_ED_GEOGRAPHY,
    # The dataset also carries sub-state rows under the same state name.
    trend_source = "State",
    `$select` = "week_end,percent_visits_covid,ed_trends_covid",
    `$order` = "week_end DESC",
    `$limit` = CDC_ROW_LIMIT
  ))

  rows <- fetch_cdc_rows(url, required = "week_end")

  cdc_tidy(data.frame(
    week_end = cdc_date(rows$week_end),
    ed_pct_covid = suppressWarnings(
      as.numeric(cdc_column(rows, "percent_visits_covid"))),
    ed_trend = as.character(cdc_column(rows, "ed_trends_covid")),
    stringsAsFactors = FALSE
  ))
}

#' Fetch the Massachusetts hospital admission series
#'
#' @return Data frame: week_end, hosp_admissions, hosp_pct_reporting
fetch_cdc_hosp <- function() {
  url <- cdc_query_url(CDC_HOSP_DATASET, list(
    jurisdiction = CDC_HOSP_JURISDICTION,
    `$select` = paste("weekendingdate", "totalconfc19newadm",
                      "totalconfc19newadmperchosprep", sep = ","),
    `$order` = "weekendingdate DESC",
    `$limit` = CDC_ROW_LIMIT
  ))

  rows <- fetch_cdc_rows(url, required = "weekendingdate")

  cdc_tidy(data.frame(
    week_end = cdc_date(rows$weekendingdate),
    hosp_admissions = suppressWarnings(
      as.numeric(cdc_column(rows, "totalconfc19newadm"))),
    # Share of expected hospitals that reported. Stored for anyone checking a
    # surprising week; the email doesn't print it.
    hosp_pct_reporting = suppressWarnings(
      as.numeric(cdc_column(rows, "totalconfc19newadmperchosprep"))),
    stringsAsFactors = FALSE
  ))
}

#' Write a CDC series to CSV
#'
#' @param data Data frame from fetch_cdc_ed() or fetch_cdc_hosp()
#' @param path Output path
#' @return The path, invisibly
save_cdc_data <- function(data, path) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  readr::write_csv(data, path, na = "")
  invisible(path)
}

#' Read the stored emergency department series back
#'
#' @return Data frame, or NULL when the file doesn't exist yet
read_cdc_ed_data <- function(path = CDC_ED_CSV_PATH) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.data.frame(readr::read_csv(path, col_types = readr::cols(
    week_end = readr::col_date(),
    ed_pct_covid = readr::col_double(),
    ed_trend = readr::col_character()
  )))
}

#' Read the stored hospital admission series back
#'
#' @return Data frame, or NULL when the file doesn't exist yet
read_cdc_hosp_data <- function(path = CDC_HOSP_CSV_PATH) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.data.frame(readr::read_csv(path, col_types = readr::cols(
    week_end = readr::col_date(),
    hosp_admissions = readr::col_double(),
    hosp_pct_reporting = readr::col_double()
  )))
}

#' CDC's trend label, in the words the email uses
#'
#' Only the three labels that actually describe a direction are repeated.
#' "Sparse", "Limited Data", and "Data Unavailable" mean CDC didn't call one,
#' and the line simply goes without.
#'
#' @param label Value of `ed_trends_covid`
#' @return Lower-case label, or NA
cdc_trend_word <- function(label) {
  if (length(label) != 1 || is.na(label)) {
    return(NA_character_)
  }
  switch(label,
    "Increasing" = "increasing",
    "Decreasing" = "decreasing",
    "No Change" = "no change",
    NA_character_
  )
}

#' Build the emergency department line of the summary
#'
#' @param data Data frame from read_cdc_ed_data(), or NULL
#' @return An indicator_line(), or NULL when there is nothing to report
cdc_ed_line <- function(data) {
  pair <- indicator_latest_pair(data, "ed_pct_covid")
  if (is.null(pair)) {
    return(NULL)
  }

  text <- paste("ER visits with Covid:",
                indicator_with_change(pair$current, pair$previous,
                                      indicator_pct2))

  # The label is CDC's reading of its own smoothed series, so it can differ
  # from the week-over-week comparison beside it. It is attributed for that
  # reason, and never replaced with a judgement of ours.
  trend <- cdc_trend_word(pair$row$ed_trend)
  if (!is.na(trend)) {
    text <- sprintf("%s (CDC trend: %s)", text, trend)
  }

  indicator_line(text, pair$week_end, "cdc_ed",
                 list(measure = "ed_pct_covid", current = pair$current,
                      previous = pair$previous, cdc_trend = pair$row$ed_trend),
                 what = "ER visits")
}

#' Build the hospital admission line of the summary
#'
#' @param data Data frame from read_cdc_hosp_data(), or NULL
#' @return An indicator_line(), or NULL when there is nothing to report
cdc_hosp_line <- function(data) {
  pair <- indicator_latest_pair(data, "hosp_admissions")
  if (is.null(pair)) {
    return(NULL)
  }

  indicator_line(
    paste("New hospital admissions:",
          indicator_with_change(pair$current, pair$previous, indicator_count)),
    pair$week_end, "cdc_hosp",
    list(measure = "hosp_admissions", current = pair$current,
         previous = pair$previous,
         pct_hospitals_reporting = pair$row$hosp_pct_reporting),
    what = "hospital admissions"
  )
}
