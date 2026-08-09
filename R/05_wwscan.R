# WastewaterSCAN COVID data for the Boston (Deer Island) sewershed
#
# MWRA sends Deer Island samples to WastewaterSCAN as well as to Biobot, and
# WastewaterSCAN publishes far more often (every 2-3 days vs. Biobot's weekly,
# which stalled entirely in July 2026). This module is a second, independent
# source for the same sewershed -- it does NOT feed the Biobot pipeline.
#
# The two are not comparable and must never be merged or plotted on one axis:
#   Biobot        copies/mL of wastewater, flow-adjusted
#   WastewaterSCAN gene copies per gram dry solids, normalized to PMMoV
#
# Data source: the dashboard at data.wastewaterscan.org is a static site that
# reads per-plant JSON straight from a public Google Cloud Storage bucket. We
# read the same file. See README for how to re-derive the URL if it moves.

# Public JSON the WastewaterSCAN dashboard itself fetches.
WWSCAN_JSON_BASE <- "https://storage.googleapis.com/wastewater-dev-data/json/"

# Deer Island Treatment Plant, listed as "Boston, MA" (pop. served 2.4M).
WWSCAN_BOSTON_PLANT_ID <- "b50c6424-02d1-482f-b928-dbed1d7eab25"

# SARS-CoV-2 is assay "N Gene" in the targets table (public name "SC2_N").
WWSCAN_COVID_TARGET <- "N Gene"

WWSCAN_STATE_FILE <- "state/wwscan_state.json"
WWSCAN_CSV_PATH <- "data/processed/wwscan_covid.csv"

# The dashboard multiplies every PMMoV-normalized value by 1e6 before plotting,
# which is what its "(x1 million)" axis label refers to. We store the same
# scaled numbers so our charts and its charts show identical values.
WWSCAN_PMMOV_SCALE <- 1e6

# National 33rd/66th percentile cutoffs for the N Gene target, in the scaled
# units above. WastewaterSCAN uses these to label a site's current level as
# low / medium / high -- the word shown next to "SARS-CoV-2" on its overview
# page. The values are baked into the dashboard's JavaScript bundle rather than
# served as data, so they are copied here; see README for how to re-check them.
WWSCAN_TERTILE_33 <- 20.331799376770654
WWSCAN_TERTILE_66 <- 105.40155394749259

# Trailing window WastewaterSCAN uses for its trend statement, and the
# significance level for the slope test.
WWSCAN_TREND_DAYS <- 21
WWSCAN_TREND_ALPHA <- 0.05

#' URL of the per-plant JSON file
#'
#' @param plant_id WastewaterSCAN plant UUID
#' @return Full URL to the plant's JSON
wwscan_plant_url <- function(plant_id = WWSCAN_BOSTON_PLANT_ID) {
  paste0(WWSCAN_JSON_BASE, plant_id, ".json")
}

#' Fetch and parse a plant's WastewaterSCAN JSON
#'
#' Routes through impersonate_fetch() so this stays the only network entry
#' point in the codebase (see CLAUDE.md) and so tests have a single stub seam.
#' The response is written to a temp file rather than read back as a string:
#' the Boston file is ~5 MB and jsonlite parses the file directly.
#'
#' @param plant_id WastewaterSCAN plant UUID
#' @return Parsed list with `samples`, `plant`, and `updated` elements
#' @details Stops with an informative error if the response is not JSON with a
#'   samples array -- that is the signal the bucket has moved or changed shape.
fetch_wwscan_plant <- function(plant_id = WWSCAN_BOSTON_PLANT_ID) {
  url <- wwscan_plant_url(plant_id)

  json_path <- tempfile(fileext = ".json")
  on.exit(unlink(json_path), add = TRUE)

  with_retry(function() {
    impersonate_fetch(url, output_path = json_path)

    payload <- tryCatch(
      jsonlite::read_json(json_path, simplifyVector = FALSE),
      error = function(e) {
        stop(sprintf("WastewaterSCAN response was not valid JSON: %s", e$message))
      }
    )

    if (is.null(payload$samples) || length(payload$samples) == 0) {
      stop("WastewaterSCAN JSON has no samples -- the feed may have moved.")
    }

    payload
  }, max_attempts = 3)
}

#' Pull one numeric field for one target out of every sample
#'
#' @param entries List of per-sample target entries (may contain NULLs)
#' @param field Name of the numeric field to extract
#' @return Numeric vector, NA where the sample or field is missing
wwscan_numeric_field <- function(entries, field) {
  vapply(entries, function(entry) {
    if (is.null(entry) || is.null(entry[[field]])) {
      return(NA_real_)
    }
    as.numeric(entry[[field]])
  }, numeric(1))
}

#' Extract one target's time series from a parsed plant payload
#'
#' Mirrors the dashboard's own field selection: the plotted line is the
#' 5-sample trimmed mean, the confidence interval belongs to the unsmoothed
#' value, and all PMMoV-normalized numbers are scaled by 1e6.
#'
#' @param payload Parsed JSON from fetch_wwscan_plant()
#' @param target Assay name, e.g. "N Gene"
#' @return Data frame sorted by date with one row per sample
extract_wwscan_target <- function(payload, target = WWSCAN_COVID_TARGET) {
  samples <- payload$samples

  dates <- vapply(samples, function(s) {
    if (is.null(s$collection_date)) NA_character_ else as.character(s$collection_date)
  }, character(1))

  entries <- lapply(samples, function(s) s$targets[[target]])

  if (all(vapply(entries, is.null, logical(1)))) {
    stop(sprintf("No '%s' target found in the WastewaterSCAN data.", target))
  }

  categories <- vapply(entries, function(entry) {
    if (is.null(entry) || is.null(entry$activity_category)) {
      return(NA_character_)
    }
    as.character(entry$activity_category)
  }, character(1))

  result <- data.frame(
    date = as.Date(dates),
    # Smoothed, PMMoV-normalized -- the line the dashboard draws.
    covid = wwscan_numeric_field(entries, "gc_g_dry_weight_trimmed5_pmmov") *
      WWSCAN_PMMOV_SCALE,
    # Unsmoothed per-sample value, and its CI. The trend test uses this one:
    # the smoothed series is autocorrelated by construction, so regressing on
    # it would report a significant trend almost every week.
    covid_unsmoothed = wwscan_numeric_field(entries, "gc_g_dry_weight_pmmov") *
      WWSCAN_PMMOV_SCALE,
    lower_ci = wwscan_numeric_field(entries, "gc_g_dry_weight_pmmov_lci") *
      WWSCAN_PMMOV_SCALE,
    upper_ci = wwscan_numeric_field(entries, "gc_g_dry_weight_pmmov_uci") *
      WWSCAN_PMMOV_SCALE,
    # WastewaterSCAN's own per-sample label, which is plant-relative and so
    # does NOT match the national low/medium/high level below.
    activity_category = categories,
    stringsAsFactors = FALSE
  )

  result <- result[!is.na(result$date), ]
  result <- result[order(result$date), ]
  rownames(result) <- NULL
  result
}

#' Classify a concentration against the national tertile cutoffs
#'
#' @param value Smoothed, scaled concentration
#' @return "low", "medium", "high", or "unknown"
wwscan_level <- function(value) {
  if (length(value) != 1 || is.na(value)) {
    return("unknown")
  }
  if (value < WWSCAN_TERTILE_33) {
    "low"
  } else if (value < WWSCAN_TERTILE_66) {
    "medium"
  } else {
    "high"
  }
}

#' Test for a trend over the trailing window
#'
#' Ordinary least squares of the unsmoothed concentration on date. A slope is
#' called a trend only when its p-value clears `alpha`, which is why a series
#' that drifts around without direction reports "No trend".
#'
#' @param data Data frame from extract_wwscan_target()
#' @param days Length of the trailing window in days
#' @param alpha Significance level for the slope
#' @return List with direction ("up", "down", "none", "unknown"), description,
#'   slope, p_value, and n (points in the window)
wwscan_trend <- function(data, days = WWSCAN_TREND_DAYS,
                         alpha = WWSCAN_TREND_ALPHA) {
  unknown <- list(direction = "unknown", description = "Trend unavailable",
                  slope = NA_real_, p_value = NA_real_, n = 0L)

  if (nrow(data) == 0 || all(is.na(data$date))) {
    return(unknown)
  }

  latest <- max(data$date, na.rm = TRUE)
  # An NA in `date` would index NA rows into the window rather than dropping
  # them, so it has to be excluded explicitly, not just compared against.
  # Half-open window (latest - days, latest], matching wwscan_window_mean(),
  # so the trend and the change sentence describe the same "last 21 days".
  keep <- !is.na(data$date) & data$date > latest - days &
    !is.na(data$covid_unsmoothed)
  window <- data[keep, ]

  # Three points is the minimum that leaves a residual degree of freedom.
  if (nrow(window) < 3) {
    return(unknown)
  }

  fit <- stats::lm(covid_unsmoothed ~ as.numeric(date), data = window)
  coefs <- summary(fit)$coefficients

  # A window where every value is identical yields no slope row at all.
  if (nrow(coefs) < 2) {
    return(unknown)
  }

  slope <- coefs[2, 1]
  p_value <- coefs[2, 4]
  significant <- !is.na(p_value) && p_value < alpha

  direction <- if (!significant) {
    "none"
  } else if (slope > 0) {
    "up"
  } else {
    "down"
  }

  list(
    direction = direction,
    description = switch(direction,
      up = "Upward trend",
      down = "Downward trend",
      "No trend"
    ),
    slope = slope,
    p_value = p_value,
    n = nrow(window)
  )
}

#' Mean of the trailing window, for a plain-language change figure
#'
#' @param data Data frame from extract_wwscan_target()
#' @param end End date of the window (inclusive)
#' @param days Window length in days
#' @return Mean smoothed concentration, or NA if the window is empty
wwscan_window_mean <- function(data, end, days = WWSCAN_TREND_DAYS) {
  keep <- !is.na(data$date) & data$date > end - days & data$date <= end &
    !is.na(data$covid)
  if (!any(keep)) {
    return(NA_real_)
  }
  mean(data$covid[keep])
}

#' Build the summary used by notifications and the dashboard
#'
#' The headline sentence deliberately copies WastewaterSCAN's own wording so
#' an email and its site never appear to disagree.
#'
#' @param data Data frame from extract_wwscan_target()
#' @return List of summary values, including a ready-to-send `headline`
wwscan_summary <- function(data) {
  if (nrow(data) == 0 || all(is.na(data$date))) {
    stop("No WastewaterSCAN data to summarize.")
  }

  # Take the newest row by date rather than the last row, so a caller that
  # hands over unsorted data still gets the right "latest".
  latest_row <- data[which.max(data$date), ]
  latest_date <- latest_row$date
  latest_value <- latest_row$covid
  level <- wwscan_level(latest_value)
  trend <- wwscan_trend(data)

  recent_mean <- wwscan_window_mean(data, latest_date)
  prior_mean <- wwscan_window_mean(data, latest_date - WWSCAN_TREND_DAYS)
  pct_change <- if (is.na(recent_mean) || is.na(prior_mean) || prior_mean == 0) {
    NA_real_
  } else {
    100 * (recent_mean - prior_mean) / prior_mean
  }

  list(
    latest_date = latest_date,
    latest_value = latest_value,
    level = level,
    trend = trend,
    recent_mean = recent_mean,
    prior_mean = prior_mean,
    pct_change = pct_change,
    n_samples = nrow(data),
    headline = sprintf(
      "%s in the last %d days and %s concentration",
      trend$description, WWSCAN_TREND_DAYS, level
    )
  )
}

#' One-line plain-language description of the change between windows
#'
#' @param summary_info List from wwscan_summary()
#' @return Character scalar
wwscan_change_sentence <- function(summary_info) {
  if (is.na(summary_info$pct_change)) {
    return("Not enough recent samples to compare with the previous three weeks.")
  }

  change <- summary_info$pct_change
  direction <- if (change >= 0) "up" else "down"

  sprintf(
    paste0("The average of the last %d days is %.0f (%s %.0f%% from %.0f ",
           "over the previous %d days)."),
    WWSCAN_TREND_DAYS, summary_info$recent_mean, direction, abs(change),
    summary_info$prior_mean, WWSCAN_TREND_DAYS
  )
}

#' Write the COVID series to CSV
#'
#' @param data Data frame from extract_wwscan_target()
#' @param path Output path
#' @return The path, invisibly
save_wwscan_data <- function(data, path = WWSCAN_CSV_PATH) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # The national level is stored per row so the dashboard doesn't have to
  # duplicate the tertile cutoffs in JavaScript.
  out <- data
  out$level <- vapply(out$covid, wwscan_level, character(1))
  out <- out[, c("date", "covid", "covid_unsmoothed", "lower_ci", "upper_ci",
                 "level", "activity_category")]

  readr::write_csv(out, path, na = "")
  invisible(path)
}

#' Write the summary the dashboard displays
#'
#' The dashboard shows the level and trend sentence rather than recomputing
#' them, so the tertile cutoffs and the slope test live in exactly one place.
#'
#' @param summary_info List from wwscan_summary()
#' @param path Output path for the JSON
#' @return The path, invisibly
save_wwscan_summary <- function(summary_info, path = "docs/data/wwscan_summary.json") {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  payload <- list(
    latest_date = format(summary_info$latest_date, "%Y-%m-%d"),
    latest_value = round(summary_info$latest_value, 1),
    level = summary_info$level,
    trend = summary_info$trend$direction,
    headline = summary_info$headline,
    change_text = wwscan_change_sentence(summary_info),
    tertile_33 = WWSCAN_TERTILE_33,
    tertile_66 = WWSCAN_TERTILE_66,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' Update the WastewaterSCAN state file after a successful run
#'
#' @param sample_date Latest sample date (Date or YYYY-MM-DD character)
#' @param updated The feed's own `updated` timestamp, if present
#' @return The state list, invisibly
update_wwscan_state <- function(sample_date, updated = NULL) {
  state <- list(
    last_sample_date = format(as.Date(sample_date), "%Y-%m-%d"),
    feed_updated = if (is.null(updated)) NA_character_ else as.character(updated),
    last_check_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  save_state(state, WWSCAN_STATE_FILE)
  invisible(state)
}
