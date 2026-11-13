# Tests for R/08_cdc.R
#
# Fixtures are real responses from data.cdc.gov, saved 2026-11-13: the newest
# eight Massachusetts weeks from each dataset, newest first, exactly as the
# API returns them (every value a string).
#
#   cdc_ed.json    rdmq-nq56, percent of ED visits with Covid and CDC's trend
#   cdc_hosp.json  ua7e-t2fy, weekly new Covid hospital admissions

stub_json <- function(text, frame = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = frame)
  writeLines(text, path)
  local_fetch_stubs(path, frame = frame)
}

describe("cdc_query_url()", {
  it("percent-encodes parameter names and values", {
    url <- cdc_query_url("abcd-1234", list(
      geography = "New York",
      `$order` = "week_end DESC",
      `$limit` = 5
    ))

    expect_equal(url, paste0(
      "https://data.cdc.gov/resource/abcd-1234.json",
      "?geography=New%20York&%24order=week_end%20DESC&%24limit=5"
    ))
  })
})

describe("fetch_cdc_ed()", {
  it("parses the feed into a series sorted oldest to newest", {
    local_fetch_stubs(test_fixture_path("cdc_ed.json"))

    data <- fetch_cdc_ed()

    expect_equal(nrow(data), 8)
    expect_named(data, c("week_end", "ed_pct_covid", "ed_trend"))
    expect_false(is.unsorted(data$week_end))
    expect_equal(tail(data$week_end, 1), as.Date("2026-11-07"))
    expect_equal(tail(data$ed_pct_covid, 2), c(0.49, 0.55))
    expect_equal(tail(data$ed_trend, 1), "Increasing")
  })

  it("errors when the response is not JSON", {
    stub_json("<html><body>Service unavailable</body></html>")

    expect_error(fetch_cdc_ed(), "not valid JSON")
  })

  it("errors when the dataset returns no rows", {
    stub_json("[]")

    expect_error(fetch_cdc_ed(), "no rows")
  })

  it("errors when the date field has been renamed", {
    stub_json('[{"week_ending": "2026-11-07", "percent_visits_covid": "0.55"}]')

    expect_error(fetch_cdc_ed(), "missing field.*week_end")
  })

  it("fills a field Socrata omitted with NA instead of failing", {
    # Socrata leaves a null field out of the row entirely.
    stub_json('[{"week_end": "2026-11-07T00:00:00.000"}]')

    data <- fetch_cdc_ed()

    expect_true(is.na(data$ed_pct_covid))
    expect_true(is.na(data$ed_trend))
  })
})

describe("fetch_cdc_hosp()", {
  it("parses admissions and hospital reporting coverage", {
    local_fetch_stubs(test_fixture_path("cdc_hosp.json"))

    data <- fetch_cdc_hosp()

    expect_equal(nrow(data), 8)
    expect_named(data, c("week_end", "hosp_admissions", "hosp_pct_reporting"))
    expect_equal(tail(data$week_end, 1), as.Date("2026-11-07"))
    expect_equal(tail(data$hosp_admissions, 2), c(84, 71))
    expect_equal(tail(data$hosp_pct_reporting, 1), 88.89)
  })
})

describe("saving and reading the CDC series", {
  it("round-trips the emergency department series", {
    local_fetch_stubs(test_fixture_path("cdc_ed.json"))
    path <- file.path(withr::local_tempdir(), "nested", "ed.csv")
    data <- fetch_cdc_ed()

    save_cdc_data(data, path)

    expect_equal(read_cdc_ed_data(path), data)
  })

  it("round-trips the hospital admission series", {
    local_fetch_stubs(test_fixture_path("cdc_hosp.json"))
    path <- file.path(withr::local_tempdir(), "hosp.csv")
    data <- fetch_cdc_hosp()

    save_cdc_data(data, path)

    expect_equal(read_cdc_hosp_data(path), data)
  })

  it("returns NULL when nothing has been stored yet", {
    missing <- file.path(withr::local_tempdir(), "none.csv")

    expect_null(read_cdc_ed_data(missing))
    expect_null(read_cdc_hosp_data(missing))
  })
})

describe("cdc_trend_word()", {
  it("repeats the three labels that name a direction", {
    expect_equal(cdc_trend_word("Increasing"), "increasing")
    expect_equal(cdc_trend_word("Decreasing"), "decreasing")
    expect_equal(cdc_trend_word("No Change"), "no change")
  })

  it("returns NA when CDC didn't call a trend", {
    for (label in c("Sparse", "Limited Data", "Data Unavailable", NA)) {
      expect_true(is.na(cdc_trend_word(label)))
    }
  })
})

describe("cdc_ed_line()", {
  ed_fixture <- function() {
    local_fetch_stubs(test_fixture_path("cdc_ed.json"))
    fetch_cdc_ed()
  }

  it("writes the line in the approved wording, with CDC's trend", {
    line <- cdc_ed_line(ed_fixture())

    expect_equal(
      line$text,
      "ER visits with Covid: 0.55%, up from 0.49% (CDC trend: increasing)"
    )
    expect_equal(line$week_end, as.Date("2026-11-07"))
    expect_equal(line$source, "cdc_ed")
  })

  it("leaves the trend out when CDC didn't call one", {
    data <- ed_fixture()
    data$ed_trend[nrow(data)] <- "Sparse"

    expect_equal(cdc_ed_line(data)$text,
                 "ER visits with Covid: 0.55%, up from 0.49%")
  })

  it("repeats CDC's label even when it differs from the weekly change", {
    # The label describes CDC's smoothed series, so this can really happen.
    # We report both and never overrule theirs.
    data <- ed_fixture()
    data$ed_trend[nrow(data)] <- "Decreasing"

    expect_match(cdc_ed_line(data)$text, "up from 0.49% (CDC trend: decreasing)",
                 fixed = TRUE)
  })

  it("returns NULL when there is no data", {
    expect_null(cdc_ed_line(NULL))
  })
})

describe("cdc_hosp_line()", {
  it("writes the line in the approved wording", {
    local_fetch_stubs(test_fixture_path("cdc_hosp.json"))

    line <- cdc_hosp_line(fetch_cdc_hosp())

    expect_equal(line$text, "New hospital admissions: 71, down from 84")
    expect_equal(line$source, "cdc_hosp")
  })

  it("skips a newest week that has no admissions figure yet", {
    local_fetch_stubs(test_fixture_path("cdc_hosp.json"))
    data <- fetch_cdc_hosp()
    data$hosp_admissions[nrow(data)] <- NA

    line <- cdc_hosp_line(data)

    expect_equal(line$week_end, as.Date("2026-10-31"))
    expect_match(line$text, "^New hospital admissions: 84, up from 54$")
  })

  it("returns NULL when there is no data", {
    expect_null(cdc_hosp_line(NULL))
  })
})
