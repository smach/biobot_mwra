# Tests for R/05_wwscan.R
#
# Two fixtures, mirroring the two files the dashboard reads:
#
#   wwscan_boston.json     a trimmed copy of the real Boston/Deer Island feed
#                          (last 30 samples, N Gene and PMMoV targets only),
#                          newest sample 2026-08-05
#   wwscan_categories.json WastewaterSCAN's own verdicts in their real shape,
#                          with the Boston entry dated to match the feed
#                          fixture, plus two invented sites that carry a
#                          significant rise and a significant fall so those
#                          branches get exercised against real field names
#
# Nothing here re-tests a significance calculation, because we no longer make
# one: the point of these tests is that we read WastewaterSCAN's verdict
# faithfully and never substitute one of our own.

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

fixture_categories <- function() {
  jsonlite::read_json(test_fixture_path("wwscan_categories.json"),
                      simplifyVector = FALSE)
}

# The Boston status the two fixtures agree on: medium, not significant.
fixture_status <- function() {
  wwscan_published_status(fixture_categories())
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

describe("fetch_wwscan_categories()", {
  it("parses the categories file keyed by plant uid", {
    local_wwscan_stubs(test_fixture_path("wwscan_categories.json"))

    result <- fetch_wwscan_categories()

    expect_true(WWSCAN_BOSTON_PLANT_UID %in% names(result))
    expect_false(is.null(result[[WWSCAN_BOSTON_PLANT_UID]][["N Gene"]]))
  })

  it("errors when the response is not JSON", {
    not_json <- withr::local_tempfile(fileext = ".html")
    writeLines("<html><body>Nope</body></html>", not_json)
    local_wwscan_stubs(not_json)

    expect_error(fetch_wwscan_categories(), "not valid JSON")
  })

  it("errors when the file is an empty object", {
    empty <- withr::local_tempfile(fileext = ".json")
    writeLines("{}", empty)
    local_wwscan_stubs(empty)

    expect_error(fetch_wwscan_categories(), "empty")
  })
})

describe("wwscan_level_from_tertile()", {
  it("maps the published band to the dashboard's word", {
    expect_equal(wwscan_level_from_tertile(1), "low")
    expect_equal(wwscan_level_from_tertile(2), "medium")
    expect_equal(wwscan_level_from_tertile(3), "high")
  })

  it("returns unknown for a missing or unexpected band", {
    expect_equal(wwscan_level_from_tertile(NULL), "unknown")
    expect_equal(wwscan_level_from_tertile(NA), "unknown")
    expect_equal(wwscan_level_from_tertile(9), "unknown")
  })
})

describe("wwscan_published_status()", {
  it("reports WastewaterSCAN's verdict rather than judging for itself", {
    status <- fixture_status()

    expect_equal(status$level, "medium")
    expect_equal(status$direction, "none")
    expect_false(status$significant)
    # Their numbers, carried through untouched.
    expect_equal(round(status$p_value, 4), 0.5466)
    expect_equal(round(status$slope, 4), 0.114)
    expect_equal(status$as_of, as.Date("2026-08-05"))
    expect_true(status$available)
  })

  it("calls a significant positive slope an upward trend", {
    status <- wwscan_published_status(fixture_categories(), uid = "aa11bb22")

    expect_equal(status$direction, "up")
    expect_true(status$significant)
    expect_equal(status$level, "high")
  })

  it("calls a significant negative slope a downward trend", {
    status <- wwscan_published_status(fixture_categories(), uid = "cc33dd44")

    expect_equal(status$direction, "down")
    expect_equal(status$level, "low")
  })

  it("is unavailable for a target they have not calculated", {
    status <- wwscan_published_status(fixture_categories(), target = "S Gene")

    expect_false(status$available)
    expect_equal(status$direction, "unknown")
    expect_true(is.na(status$significant))
  })

  it("is unavailable for a target scored by the seasonal method", {
    # RSV carries onset fields where the trend would be; nothing to read.
    status <- wwscan_published_status(fixture_categories(), target = "RSV")

    expect_false(status$available)
    expect_equal(status$direction, "unknown")
  })

  it("is unavailable for an unknown plant, without erroring", {
    status <- wwscan_published_status(fixture_categories(), uid = "nosuchid")

    expect_false(status$available)
    expect_equal(status$level, "unknown")
  })

  it("is unavailable when the categories file could not be read at all", {
    status <- wwscan_published_status(list())

    expect_false(status$available)
    expect_equal(status$direction, "unknown")
    expect_true(is.na(status$p_value))
  })
})

describe("wwscan_headline()", {
  significant_up <- function() {
    wwscan_published_status(fixture_categories(), uid = "aa11bb22")
  }

  it("uses WastewaterSCAN's own wording when they call it a trend", {
    expect_equal(
      wwscan_headline(significant_up(), 40),
      "Upward trend in the last 21 days and high concentration"
    )
  })

  it("names the change and says it isn't significant when they don't", {
    expect_equal(
      wwscan_headline(fixture_status(), 31.4),
      paste("Up 31% in the last 21 days (not a statistically significant",
            "trend) and medium concentration")
    )
  })

  it("says which way a non-significant fall went", {
    expect_match(wwscan_headline(fixture_status(), -28), "^Down 28%")
  })

  it("still says plain 'No trend' when the average barely moved", {
    expect_equal(
      wwscan_headline(fixture_status(), 4),
      "No trend in the last 21 days and medium concentration"
    )
  })

  it("never contradicts the rounded change figure shown underneath", {
    # 14.6% prints as "up 15%" in the change sentence, so the headline has to
    # treat it as 15 too -- the mismatch that prompted this rewrite.
    expect_match(wwscan_headline(fixture_status(), 14.6), "^Up 15%")
    expect_match(wwscan_headline(fixture_status(), 14.4), "^No trend")
  })

  it("says so plainly when their trend can't be read", {
    expect_equal(
      wwscan_headline(wwscan_published_status(list()), 31, level = "medium"),
      "WastewaterSCAN trend not available; medium concentration"
    )
  })

  it("tolerates a missing change figure", {
    expect_equal(
      wwscan_headline(fixture_status(), NA_real_),
      "No trend in the last 21 days and medium concentration"
    )
  })
})

describe("wwscan_trend_detail()", {
  it("reports their p-value for a non-significant window", {
    expect_equal(wwscan_trend_detail(fixture_status()),
                 "not statistically significant (p = 0.55)")
  })

  it("names the direction for a significant one", {
    status <- wwscan_published_status(fixture_categories(), uid = "aa11bb22")
    expect_match(wwscan_trend_detail(status), "significant upward slope")
  })

  it("says when there is nothing to report", {
    expect_equal(wwscan_trend_detail(wwscan_published_status(list())),
                 "not available from WastewaterSCAN")
  })
})

describe("wwscan_summary()", {
  it("takes the level and trend from WastewaterSCAN", {
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           fixture_status())

    expect_equal(info$latest_date, as.Date("2026-08-05"))
    expect_equal(info$level, "medium")
    expect_equal(info$level_source, "wastewaterscan")
    expect_equal(info$trend$direction, "none")
    expect_false(info$trend$significant)
  })

  it("names the rise the change figure shows instead of calling it flat", {
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           fixture_status())

    # The old headline said "No trend" here while the line underneath said
    # "up 15%". Both now describe the same movement.
    expect_match(info$headline, "^Up 15% in the last 21 days")
    expect_match(info$headline, "not a statistically significant trend",
                 fixed = TRUE)
    expect_match(wwscan_change_sentence(info), "up 15%", fixed = TRUE)
  })

  it("compares the trailing window with the one before it", {
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           fixture_status())

    expect_false(is.na(info$recent_mean))
    expect_false(is.na(info$prior_mean))
    expect_equal(info$pct_change,
                 100 * (info$recent_mean - info$prior_mean) / info$prior_mean)
  })

  it("falls back to the copied cutoffs only for the level, never the trend", {
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           wwscan_published_status(list()))

    # Latest value is 65.4, between the two cutoffs.
    expect_equal(info$level, "medium")
    expect_equal(info$level_source, "local cutoffs")
    expect_equal(info$trend$direction, "unknown")
    expect_match(info$headline, "trend not available", fixed = TRUE)
  })

  it("errors on an empty data frame", {
    empty <- data.frame(date = as.Date(character(0)), covid = numeric(0),
                        covid_unsmoothed = numeric(0))
    expect_error(wwscan_summary(empty, fixture_status()),
                 "No WastewaterSCAN data")
  })

  it("finds the newest sample even when rows arrive out of order", {
    ordered <- extract_wwscan_target(fixture_payload())
    shuffled <- ordered[c(10, 30, 1, 25, 5, seq_len(nrow(ordered))[-c(1, 5, 10, 25, 30)]), ]

    info <- wwscan_summary(shuffled, fixture_status())

    expect_equal(info$latest_date, as.Date("2026-08-05"))
    expect_equal(info$level, "medium")
  })
})

describe("save_wwscan_summary()", {
  it("records the trend numbers their site published", {
    path <- file.path(withr::local_tempdir(), "wwscan_summary.json")
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           fixture_status())

    save_wwscan_summary(info, path)
    written <- jsonlite::read_json(path)

    expect_equal(written$trend, "none")
    expect_false(written$trend_significant)
    expect_equal(written$trend_p, 0.5466)
    expect_equal(written$trend_source, "wastewaterscan")
    expect_equal(written$trend_as_of, "2026-08-05")
    expect_equal(written$headline, info$headline)
  })

  it("keeps every field the email script on the server requires", {
    # That script lives outside this repo and fetches this file from main. It
    # hard-fails on a missing field, and nothing else spans the boundary, so
    # this is the only place a rename gets caught before the emails break.
    required <- c("latest_date", "level", "trend", "headline", "change_text")

    path <- file.path(withr::local_tempdir(), "wwscan_summary.json")
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           fixture_status())
    save_wwscan_summary(info, path)

    expect_true(all(required %in% names(jsonlite::read_json(path))))
  })

  it("keeps those fields even when the trend is unavailable", {
    required <- c("latest_date", "level", "trend", "headline", "change_text")

    path <- file.path(withr::local_tempdir(), "wwscan_summary.json")
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           wwscan_published_status(list()))
    save_wwscan_summary(info, path)

    expect_true(all(required %in% names(jsonlite::read_json(path))))
  })

  it("omits absent numbers rather than writing {} or the string NA", {
    path <- file.path(withr::local_tempdir(), "wwscan_summary.json")
    info <- wwscan_summary(extract_wwscan_target(fixture_payload()),
                           wwscan_published_status(list()))

    save_wwscan_summary(info, path)

    written <- jsonlite::read_json(path)
    expect_null(written$trend_p)
    expect_null(written$trend_slope)
    expect_equal(written$trend_source, "unavailable")

    raw <- paste(readLines(path), collapse = "")
    expect_false(grepl('"NA"', raw, fixed = TRUE))
    expect_false(grepl("{}", raw, fixed = TRUE))
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
