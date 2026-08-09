# Tests for R/05_wwscan.R
#
# The fixture is a trimmed copy of the real Boston/Deer Island feed (last 30
# samples, N Gene and PMMoV targets only). Its newest sample is 2026-08-05,
# the same day the WastewaterSCAN dashboard reported "SARS-CoV-2 Medium / No
# trend in the last 21 days and medium concentration" -- so the summary tests
# below double as a check that we still reproduce the dashboard's own wording.

# Stub impersonate_fetch so it "downloads" a local file, and run with_retry's
# closure exactly once. Restores both when the calling frame exits.
local_wwscan_stubs <- function(source_file, frame = parent.frame()) {
  original_fetch <- impersonate_fetch
  original_retry <- with_retry

  assign("impersonate_fetch", function(url, output_path = NULL, ...) {
    if (is.null(output_path)) {
      return(paste(readLines(source_file, warn = FALSE), collapse = "\n"))
    }
    file.copy(source_file, output_path, overwrite = TRUE)
    invisible(output_path)
  }, envir = .GlobalEnv)

  assign("with_retry", function(fn, ...) fn(), envir = .GlobalEnv)

  withr::defer({
    assign("impersonate_fetch", original_fetch, envir = .GlobalEnv)
    assign("with_retry", original_retry, envir = .GlobalEnv)
  }, envir = frame)
}

fixture_payload <- function() {
  jsonlite::read_json(test_fixture_path("wwscan_boston.json"),
                      simplifyVector = FALSE)
}

describe("wwscan_plant_url()", {
  it("builds the per-plant JSON URL", {
    expect_equal(
      wwscan_plant_url("abc-123"),
      "https://storage.googleapis.com/wastewater-dev-data/json/abc-123.json"
    )
  })

  it("defaults to the Boston / Deer Island plant", {
    expect_match(wwscan_plant_url(), WWSCAN_BOSTON_PLANT_ID, fixed = TRUE)
  })
})

describe("fetch_wwscan_plant()", {
  it("parses the feed into samples, plant, and updated", {
    local_wwscan_stubs(test_fixture_path("wwscan_boston.json"))

    payload <- fetch_wwscan_plant()

    expect_length(payload$samples, 30)
    expect_equal(payload$plant$site_name, "Deer Island Treatment Plant")
    expect_false(is.null(payload$updated))
  })

  it("errors when the response is not JSON", {
    not_json <- withr::local_tempfile(fileext = ".html")
    writeLines("<html><body>Nope</body></html>", not_json)
    local_wwscan_stubs(not_json)

    expect_error(fetch_wwscan_plant(), "not valid JSON")
  })

  it("errors when the feed has no samples", {
    empty <- withr::local_tempfile(fileext = ".json")
    writeLines('{"samples": [], "plant": {}}', empty)
    local_wwscan_stubs(empty)

    expect_error(fetch_wwscan_plant(), "no samples")
  })
})

describe("extract_wwscan_target()", {
  it("returns one row per sample, sorted by date", {
    result <- extract_wwscan_target(fixture_payload())

    expect_equal(nrow(result), 30)
    expect_s3_class(result$date, "Date")
    expect_false(is.unsorted(result$date))
    expect_equal(max(result$date), as.Date("2026-08-05"))
  })

  it("scales PMMoV-normalized values by one million, as the dashboard does", {
    payload <- fixture_payload()
    result <- extract_wwscan_target(payload)

    last_raw <- payload$samples[[30]]$targets[["N Gene"]]
    expect_equal(
      result$covid[nrow(result)],
      last_raw$gc_g_dry_weight_trimmed5_pmmov * 1e6
    )
    expect_equal(
      result$covid_unsmoothed[nrow(result)],
      last_raw$gc_g_dry_weight_pmmov * 1e6
    )
  })

  it("carries WastewaterSCAN's own per-sample activity category", {
    result <- extract_wwscan_target(fixture_payload())
    expect_true(all(result$activity_category %in%
                      c("very low", "low", "medium", "high", "very high",
                        "not calculated")))
  })

  it("returns NA rather than failing when a field is null", {
    payload <- fixture_payload()
    payload$samples[[1]]$targets[["N Gene"]]$gc_g_dry_weight_trimmed5_pmmov <- NULL

    result <- extract_wwscan_target(payload)

    expect_true(is.na(result$covid[1]))
    expect_false(is.na(result$covid_unsmoothed[1]))
  })

  it("returns NA when a sample is missing the target entirely", {
    payload <- fixture_payload()
    payload$samples[[2]]$targets[["N Gene"]] <- NULL

    result <- extract_wwscan_target(payload)

    expect_equal(nrow(result), 30)
    expect_true(is.na(result$covid[2]))
  })

  it("errors when no sample has the requested target", {
    expect_error(
      extract_wwscan_target(fixture_payload(), target = "Nonexistent"),
      "No 'Nonexistent' target"
    )
  })
})

describe("wwscan_level()", {
  it("classifies against the national tertile cutoffs", {
    expect_equal(wwscan_level(5), "low")
    expect_equal(wwscan_level(65.4), "medium")
    expect_equal(wwscan_level(500), "high")
  })

  it("treats each cutoff as the floor of the higher band", {
    expect_equal(wwscan_level(WWSCAN_TERTILE_33), "medium")
    expect_equal(wwscan_level(WWSCAN_TERTILE_33 - 0.001), "low")
    expect_equal(wwscan_level(WWSCAN_TERTILE_66), "high")
    expect_equal(wwscan_level(WWSCAN_TERTILE_66 - 0.001), "medium")
  })

  it("returns unknown for missing values", {
    expect_equal(wwscan_level(NA_real_), "unknown")
    expect_equal(wwscan_level(numeric(0)), "unknown")
  })
})

describe("wwscan_trend()", {
  # Helper: build a data frame ending today-ish with the given values spaced
  # every other day, matching WastewaterSCAN's real sampling cadence.
  trend_data <- function(values) {
    n <- length(values)
    data.frame(
      date = as.Date("2026-08-05") - rev(seq(0, by = 2, length.out = n)),
      covid = values,
      covid_unsmoothed = values
    )
  }

  it("reports an upward trend when values climb", {
    result <- wwscan_trend(trend_data(c(10, 19, 31, 39, 52, 59, 71)))

    expect_equal(result$direction, "up")
    expect_equal(result$description, "Upward trend")
    expect_true(result$slope > 0)
    expect_true(result$p_value < 0.05)
  })

  it("reports a downward trend when values fall", {
    result <- wwscan_trend(trend_data(c(71, 59, 52, 39, 31, 19, 10)))

    expect_equal(result$direction, "down")
    expect_equal(result$description, "Downward trend")
    expect_true(result$slope < 0)
  })

  it("reports no trend when noisy values have no direction", {
    result <- wwscan_trend(trend_data(c(50, 45, 55, 48, 52, 47, 53)))

    expect_equal(result$direction, "none")
    expect_equal(result$description, "No trend")
    expect_true(result$p_value > 0.05)
  })

  it("only considers samples inside the trailing window", {
    old <- data.frame(
      date = as.Date(c("2026-01-01", "2026-01-03", "2026-01-05")),
      covid = c(1000, 1100, 1200),
      covid_unsmoothed = c(1000, 1100, 1200)
    )
    recent <- trend_data(c(50, 45, 55, 48, 52, 47, 53))

    result <- wwscan_trend(rbind(old, recent))

    # The January spike is far outside 21 days, so it must not sway the slope.
    expect_equal(result$n, 7)
    expect_equal(result$direction, "none")
  })

  it("returns unknown when the window has too few points", {
    result <- wwscan_trend(trend_data(c(40, 60)))

    expect_equal(result$direction, "unknown")
    expect_equal(result$description, "Trend unavailable")
  })

  it("returns unknown for an empty data frame", {
    empty <- data.frame(date = as.Date(character(0)), covid = numeric(0),
                        covid_unsmoothed = numeric(0))
    expect_equal(wwscan_trend(empty)$direction, "unknown")
  })

  it("drops rows with a missing date instead of indexing NA rows", {
    data <- trend_data(c(50, 45, 55, 48, 52, 47, 53))
    data$date[3] <- NA

    result <- wwscan_trend(data)

    expect_equal(result$n, 6)
    expect_false(is.na(result$p_value))
  })
})

describe("wwscan_summary()", {
  it("reproduces the wording the dashboard showed for this data", {
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()))

    expect_equal(info$latest_date, as.Date("2026-08-05"))
    expect_equal(info$level, "medium")
    expect_equal(info$trend$direction, "none")
    expect_equal(info$headline,
                 "No trend in the last 21 days and medium concentration")
  })

  it("compares the trailing window with the one before it", {
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()))

    expect_false(is.na(info$recent_mean))
    expect_false(is.na(info$prior_mean))
    expect_equal(info$pct_change,
                 100 * (info$recent_mean - info$prior_mean) / info$prior_mean)
  })

  it("errors on an empty data frame", {
    empty <- data.frame(date = as.Date(character(0)), covid = numeric(0),
                        covid_unsmoothed = numeric(0))
    expect_error(wwscan_summary(empty), "No WastewaterSCAN data")
  })

  it("finds the newest sample even when rows arrive out of order", {
    ordered <- extract_wwscan_target(fixture_payload())
    shuffled <- ordered[c(10, 30, 1, 25, 5, seq_len(nrow(ordered))[-c(1, 5, 10, 25, 30)]), ]

    info <- wwscan_summary(shuffled)

    expect_equal(info$latest_date, as.Date("2026-08-05"))
    expect_equal(info$level, "medium")
  })
})

describe("wwscan_change_sentence()", {
  it("describes a rise in plain language", {
    info <- list(pct_change = 25, recent_mean = 50, prior_mean = 40)
    expect_match(wwscan_change_sentence(info), "up 25%")
  })

  it("describes a fall in plain language", {
    info <- list(pct_change = -20, recent_mean = 40, prior_mean = 50)
    expect_match(wwscan_change_sentence(info), "down 20%")
  })

  it("says so when there is nothing to compare", {
    info <- list(pct_change = NA_real_, recent_mean = NA_real_,
                 prior_mean = NA_real_)
    expect_match(wwscan_change_sentence(info), "Not enough recent samples")
  })
})

describe("save_wwscan_data()", {
  it("writes the expected columns and adds the national level", {
    temp_dir <- withr::local_tempdir()
    path <- file.path(temp_dir, "nested", "wwscan_covid.csv")

    save_wwscan_data(extract_wwscan_target(fixture_payload()), path)

    expect_true(file.exists(path))
    written <- readr::read_csv(path, show_col_types = FALSE)

    expect_equal(names(written),
                 c("date", "covid", "covid_unsmoothed", "lower_ci",
                   "upper_ci", "level", "activity_category"))
    expect_equal(nrow(written), 30)
    expect_true(all(written$level %in% c("low", "medium", "high", "unknown")))
  })

  it("writes missing values as empty fields, not the string NA", {
    temp_dir <- withr::local_tempdir()
    path <- file.path(temp_dir, "wwscan_covid.csv")

    payload <- fixture_payload()
    payload$samples[[1]]$targets[["N Gene"]] <- NULL
    save_wwscan_data(extract_wwscan_target(payload), path)

    lines <- readLines(path)
    expect_false(any(grepl(",NA,", lines, fixed = TRUE)))
  })
})

describe("update_wwscan_state()", {
  it("records the sample date and feed timestamp", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "wwscan_state.json")

    original_save_state <- save_state
    assign("save_state", function(state, sf = NULL) {
      jsonlite::write_json(state, state_file, auto_unbox = TRUE, pretty = TRUE)
    }, envir = .GlobalEnv)
    on.exit(assign("save_state", original_save_state, envir = .GlobalEnv),
            add = TRUE)

    result <- update_wwscan_state(as.Date("2026-08-05"),
                                  "2026-08-08T09:19:12.482638")

    expect_equal(result$last_sample_date, "2026-08-05")
    expect_equal(result$feed_updated, "2026-08-08T09:19:12.482638")
    expect_match(result$last_check_time,
                 "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")

    saved <- jsonlite::read_json(state_file)
    expect_equal(saved$last_sample_date, "2026-08-05")
  })

  it("accepts a character date", {
    original_save_state <- save_state
    assign("save_state", function(state, sf = NULL) invisible(NULL),
           envir = .GlobalEnv)
    on.exit(assign("save_state", original_save_state, envir = .GlobalEnv),
            add = TRUE)

    result <- update_wwscan_state("2026-08-05")

    expect_equal(result$last_sample_date, "2026-08-05")
    expect_true(is.na(result$feed_updated))
  })
})

describe("WastewaterSCAN staleness", {
  it("reuses data_staleness_days() against the WastewaterSCAN state", {
    six_days_ago <- format(Sys.Date() - 6, "%Y-%m-%d")
    expect_equal(
      data_staleness_days(list(last_sample_date = six_days_ago)),
      6
    )
  })
})
