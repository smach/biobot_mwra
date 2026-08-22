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
#
# It also reads a second, much smaller file of per-plant verdicts -- level and
# trend, already decided. We read that one too rather than deciding either
# ourselves; see fetch_wwscan_categories() and wwscan_published_status().

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
# units above. These are NOT the source of the headline level any more -- that
# comes from WastewaterSCAN's own published tertile band. They remain here for
# the two jobs the published band can't do: drawing the low/medium and
# medium/high lines on our chart, and labelling every historical row in the
# CSV. The values are baked into the dashboard's JavaScript bundle rather than
# served as data, so they are copied here; see README for how to re-check them.
WWSCAN_TERTILE_33 <- 20.331799376770654
WWSCAN_TERTILE_66 <- 105.40155394749259

# Trailing window WastewaterSCAN uses for its trend statement. We no longer run
# a significance test of our own (see wwscan_published_status), but the window
# still defines the two 21-day averages behind the plain-language change line.
WWSCAN_TREND_DAYS <- 21

# WastewaterSCAN's own per-plant verdicts: the tertile band and the trend test
# it ran (slope `m`, p-value `p`, and a `significant` flag). Its dashboard does
# not compute either in the browser -- it reads this file and renders it. So
# reading the same file is what actually guarantees our email agrees with their
# site, which reimplementing the test never did.
WWSCAN_CATEGORIES_URL <-
  "https://data.wastewaterscan.org/data/categories/plants.json"

# The categories file is keyed by the plant's short `uid` -- the first segment
# of the UUID above, not the full id the per-plant JSON is named for.
WWSCAN_BOSTON_PLANT_UID <- "b50c6424"

# How far the 21-day average has to move before the headline names the
# direction on a window WastewaterSCAN did NOT call significant. Roughly the
# 25th percentile of this site's observed 21-day swings, so the flattest
# quarter of windows still read as a plain "No trend" and the rest say which
# way things moved while making clear it isn't a statistical trend.
WWSCAN_NOTABLE_CHANGE_PCT <- 15

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

#' Fetch WastewaterSCAN's own per-plant level and trend verdicts
#'
#' The companion to fetch_wwscan_plant(): where that returns the measurements,
#' this returns what WastewaterSCAN concluded from them. Routes through
#' impersonate_fetch() for the same reasons -- single network entry point, one
#' stub seam for tests.
#'
#' @param url Location of the categories file
#' @return Named list keyed by plant uid, each holding one entry per target
#' @details Stops if the response isn't a non-empty JSON object, which is the
#'   signal the file has moved or changed shape.
fetch_wwscan_categories <- function(url = WWSCAN_CATEGORIES_URL) {
  json_path <- tempfile(fileext = ".json")
  on.exit(unlink(json_path), add = TRUE)

  with_retry(function() {
    impersonate_fetch(url, output_path = json_path)

    payload <- tryCatch(
      jsonlite::read_json(json_path, simplifyVector = FALSE),
      error = function(e) {
        stop(sprintf("WastewaterSCAN categories were not valid JSON: %s",
                     e$message))
      }
    )

    if (!is.list(payload) || length(payload) == 0 || is.null(names(payload))) {
      stop("WastewaterSCAN categories file is empty -- it may have moved.")
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
    # Unsmoothed per-sample value, and its CI. Kept for the chart's scatter
    # points and for anyone reading the CSV; nothing here regresses on it.
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

#' Translate WastewaterSCAN's tertile band into the word its dashboard shows
#'
#' @param tertile 1, 2, or 3 as published in the categories file
#' @return "low", "medium", "high", or "unknown"
wwscan_level_from_tertile <- function(tertile) {
  if (is.null(tertile) || length(tertile) != 1 || is.na(tertile)) {
    return("unknown")
  }
  switch(as.character(tertile),
    "1" = "low",
    "2" = "medium",
    "3" = "high",
    "unknown"
  )
}

#' Read WastewaterSCAN's published level and trend for one plant and target
#'
#' We do not decide either of these. WastewaterSCAN runs the trend test, and
#' its dashboard renders the result straight out of the categories file --
#' `details$trend$significant` picks between "Upward/Downward trend" and "No
#' trend", `details$tertile` picks low/medium/high. This function reads the
#' same two fields so our email repeats their verdict instead of offering a
#' second opinion that can quietly disagree with it.
#'
#' @param categories Parsed list from fetch_wwscan_categories()
#' @param uid Plant uid (see WWSCAN_BOSTON_PLANT_UID)
#' @param target Assay name, e.g. "N Gene"
#' @return List with level, direction ("up"/"down"/"none"/"unknown"),
#'   significant, slope, p_value, as_of (the sample date the verdict covers),
#'   method, and an `available` flag
wwscan_published_status <- function(categories,
                                    uid = WWSCAN_BOSTON_PLANT_UID,
                                    target = WWSCAN_COVID_TARGET) {
  # Missing keys chain to NULL rather than erroring, so an absent plant, an
  # absent target, or an empty list all land on the same unavailable result.
  entry <- categories[[uid]][[target]]
  trend <- entry$details$trend

  slope <- if (is.null(trend$m)) NA_real_ else as.numeric(trend$m)
  p_value <- if (is.null(trend$p)) NA_real_ else as.numeric(trend$p)

  # A target WastewaterSCAN hasn't calculated carries a message where the
  # trend would be, so a missing flag is an expected state, not a parse bug.
  significant <- if (is.null(trend$significant)) NA else isTRUE(trend$significant)

  direction <- if (is.na(significant)) {
    "unknown"
  } else if (!significant) {
    "none"
  } else if (is.na(slope)) {
    # Called significant with no slope to point anywhere: report nothing
    # rather than guess a direction.
    "unknown"
  } else if (slope > 0) {
    "up"
  } else {
    "down"
  }

  list(
    level = wwscan_level_from_tertile(entry$details$tertile),
    direction = direction,
    significant = significant,
    slope = slope,
    p_value = p_value,
    as_of = if (is.null(entry$lastSampleDate)) as.Date(NA)
            else as.Date(entry$lastSampleDate),
    method = if (is.null(entry$method)) NA_character_ else as.character(entry$method),
    available = direction != "unknown"
  )
}

#' How WastewaterSCAN's trend test came out, in one clause
#'
#' Goes in the notification body so the significance claim is auditable
#' without opening their site.
#'
#' @param status List from wwscan_published_status()
#' @return Character scalar
wwscan_trend_detail <- function(status) {
  if (!status$available) {
    return("not available from WastewaterSCAN")
  }

  p_text <- if (is.na(status$p_value)) "" else sprintf(" (p = %.2f)", status$p_value)

  if (isTRUE(status$significant)) {
    sprintf("significant %s slope%s",
            if (status$direction == "up") "upward" else "downward", p_text)
  } else {
    sprintf("not statistically significant%s", p_text)
  }
}

#' Build the headline sentence
#'
#' Keeps the shape of WastewaterSCAN's own line -- "<trend> in the last 21 days
#' and <level> concentration" -- so the two read as the same statement. The one
#' place we say more than they do is a window they did not call significant but
#' where the 21-day average clearly moved: "No trend" on its own reads as
#' "levels are flat", which is not what a non-significant slope means, and it
#' contradicted the change figure printed directly underneath it.
#'
#' @param status List from wwscan_published_status()
#' @param pct_change Percent change between the two 21-day windows
#' @param level Level word to use; defaults to the published one, but callers
#'   may pass a fallback for the case where only the band couldn't be read
#' @return Character scalar, with no trailing period (callers append one)
wwscan_headline <- function(status, pct_change, level = status$level) {
  level_clause <- sprintf("%s concentration", level)

  if (!status$available) {
    return(sprintf("WastewaterSCAN trend not available; %s", level_clause))
  }

  if (status$direction %in% c("up", "down")) {
    return(sprintf(
      "%s in the last %d days and %s",
      if (status$direction == "up") "Upward trend" else "Downward trend",
      WWSCAN_TREND_DAYS, level_clause
    ))
  }

  # Compare the number the reader will actually see, not the unrounded one:
  # a 14.6% change prints as "up 15%" in the change line underneath, and a
  # headline saying "No trend" next to that is the mismatch this whole
  # rewording exists to remove.
  shown_change <- if (is.na(pct_change)) NA_real_ else
    as.numeric(sprintf("%.0f", abs(pct_change)))

  if (!is.na(shown_change) && shown_change >= WWSCAN_NOTABLE_CHANGE_PCT) {
    return(sprintf(
      "%s %.0f%% in the last %d days (not a statistically significant trend) and %s",
      if (pct_change > 0) "Up" else "Down", shown_change,
      WWSCAN_TREND_DAYS, level_clause
    ))
  }

  sprintf("No trend in the last %d days and %s", WWSCAN_TREND_DAYS, level_clause)
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
#' Pairs our own measurements with WastewaterSCAN's verdict on them: the
#' 21-day averages and their percent change are computed here, the level and
#' trend are theirs.
#'
#' @param data Data frame from extract_wwscan_target()
#' @param status List from wwscan_published_status()
#' @return List of summary values, including a ready-to-send `headline`
wwscan_summary <- function(data, status) {
  if (nrow(data) == 0 || all(is.na(data$date))) {
    stop("No WastewaterSCAN data to summarize.")
  }

  # Take the newest row by date rather than the last row, so a caller that
  # hands over unsorted data still gets the right "latest".
  latest_row <- data[which.max(data$date), ]
  latest_date <- latest_row$date
  latest_value <- latest_row$covid

  # Their published band is the headline level. Classifying the latest value
  # against the copied cutoffs is only a fallback for when the categories file
  # can't be read -- it's a lookup against published numbers, not a second
  # opinion on the trend, and `status` is left holding exactly what they said.
  from_them <- status$level != "unknown"
  level <- if (from_them) status$level else wwscan_level(latest_value)
  level_source <- if (from_them) "wastewaterscan" else "local cutoffs"

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
    level_source = level_source,
    trend = status,
    recent_mean = recent_mean,
    prior_mean = prior_mean,
    pct_change = pct_change,
    n_samples = nrow(data),
    headline = wwscan_headline(status, pct_change, level)
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
#' The dashboard renders this rather than recomputing anything, so the
#' headline our page shows and the one the notification email carries are
#' byte-for-byte the same sentence.
#'
#' The trend fields are recorded alongside it so a surprising headline can be
#' checked against the numbers WastewaterSCAN actually published, rather than
#' taken on faith.
#'
#' @param summary_info List from wwscan_summary()
#' @param path Output path for the JSON
#' @return The path, invisibly
save_wwscan_summary <- function(summary_info, path = "docs/data/wwscan_summary.json") {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  trend <- summary_info$trend

  payload <- list(
    latest_date = format(summary_info$latest_date, "%Y-%m-%d"),
    latest_value = round(summary_info$latest_value, 1),
    level = summary_info$level,
    level_source = summary_info$level_source,
    trend = trend$direction,
    trend_significant = trend$significant,
    trend_p = if (is.na(trend$p_value)) NULL else round(trend$p_value, 4),
    trend_slope = if (is.na(trend$slope)) NULL else trend$slope,
    trend_as_of = if (is.na(trend$as_of)) NULL else format(trend$as_of, "%Y-%m-%d"),
    trend_source = if (trend$available) "wastewaterscan" else "unavailable",
    change_pct = if (is.na(summary_info$pct_change)) NULL else
      round(summary_info$pct_change),
    headline = summary_info$headline,
    change_text = wwscan_change_sentence(summary_info),
    tertile_33 = WWSCAN_TERTILE_33,
    tertile_66 = WWSCAN_TERTILE_66,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  # jsonlite writes a NULL element as `{}` and a numeric NA as the string
  # "NA", so drop the fields that have no value instead of emitting either.
  # A logical NA is fine -- that one becomes a real JSON null.
  payload <- Filter(Negate(is.null), payload)

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
