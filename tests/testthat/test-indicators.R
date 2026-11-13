# Tests for R/06_indicators.R
#
# The summary tests build on the same fixtures as test-dph.R and test-cdc.R,
# whose newest week all end 2026-11-07, and pin "today" to 2026-11-13 -- the
# day the wording was approved against these exact numbers.

FIXTURE_TODAY <- as.Date("2026-11-13")

fixture_sources <- function(frame = parent.frame()) {
  dph <- extract_dph_series(test_fixture_path("dph_dashboard.xlsx"))

  local_fetch_stubs(test_fixture_path("cdc_ed.json"), frame = frame)
  ed <- fetch_cdc_ed()
  local_fetch_stubs(test_fixture_path("cdc_hosp.json"), frame = frame)
  hosp <- fetch_cdc_hosp()

  list(dph = dph, cdc_ed = ed, cdc_hosp = hosp)
}

fixture_summary <- function(today = FIXTURE_TODAY) {
  sources <- fixture_sources()
  indicators_summary(sources$dph, sources$cdc_ed, sources$cdc_hosp,
                     today = today)
}

weekly <- function(values, last = "2026-11-07") {
  data.frame(
    week_end = as.Date(last) - 7 * rev(seq_along(values) - 1),
    value = values
  )
}

describe("indicator_date_label()", {
  it("abbreviates months the AP way", {
    expect_equal(indicator_date_label(as.Date("2026-11-07")), "Nov. 7")
    expect_equal(indicator_date_label(as.Date("2026-09-05")), "Sept. 5")
    expect_equal(indicator_date_label(as.Date("2026-01-31")), "Jan. 31")
  })

  it("spells out March through July", {
    expect_equal(indicator_date_label(as.Date("2026-03-01")), "March 1")
    expect_equal(indicator_date_label(as.Date("2026-07-04")), "July 4")
  })
})

describe("indicator_with_change()", {
  it("says up or down against the week before", {
    expect_equal(indicator_with_change(1123, 978, indicator_count),
                 "1,123, up from 978")
    expect_equal(indicator_with_change(71, 84, indicator_count),
                 "71, down from 84")
  })

  it("puts the label straight after the current value", {
    expect_equal(
      indicator_with_change(7.018, 5.504, indicator_pct1, " of tests positive"),
      "7.0% of tests positive, up from 5.5%"
    )
  })

  it("calls it unchanged when the two round to the same figure", {
    # 7.04 and 6.96 differ, but both print as 7.0%.
    expect_equal(indicator_with_change(7.04, 6.96, indicator_pct1),
                 "7.0%, unchanged from the week before")
  })

  it("gives the value alone when there is no previous week", {
    expect_equal(indicator_with_change(1123, NA, indicator_count), "1,123")
  })
})

describe("indicator_latest_pair()", {
  it("pairs the newest week with the one seven days earlier", {
    pair <- indicator_latest_pair(weekly(c(10, 20, 30)), "value")

    expect_equal(pair$week_end, as.Date("2026-11-07"))
    expect_equal(pair$current, 30)
    expect_equal(pair$previous, 20)
  })

  it("skips a newest week with no value", {
    pair <- indicator_latest_pair(weekly(c(10, 20, NA)), "value")

    expect_equal(pair$week_end, as.Date("2026-10-31"))
    expect_equal(pair$current, 20)
    expect_equal(pair$previous, 10)
  })

  it("won't compare across a gap in the weeks", {
    data <- weekly(c(10, 20, 30))[-2, ]

    expect_true(is.na(indicator_latest_pair(data, "value")$previous))
  })

  it("returns NULL when there is nothing usable", {
    expect_null(indicator_latest_pair(NULL, "value"))
    expect_null(indicator_latest_pair(weekly(c(NA_real_, NA_real_)), "value"))
    expect_null(indicator_latest_pair(weekly(1), "no_such_column"))
  })
})

describe("indicator_section()", {
  it("puts a shared week in the heading", {
    section <- indicator_section("Massachusetts", list(
      indicator_line("Reported cases: 10", "2026-11-07", "dph"),
      indicator_line("New hospital admissions: 5", "2026-11-07", "cdc_hosp")
    ))

    expect_equal(section$heading, "Massachusetts, week ending Nov. 7:")
    expect_equal(section$lines,
                 list("Reported cases: 10", "New hospital admissions: 5"))
  })

  it("dates each line separately when the sources cover different weeks", {
    # Between DPH's Thursday update and CDC's Friday one, DPH is a week ahead.
    section <- indicator_section("Massachusetts", list(
      indicator_line("Reported cases: 10", "2026-11-14", "dph"),
      indicator_line("New hospital admissions: 5", "2026-11-07", "cdc_hosp")
    ))

    expect_equal(section$heading, "Massachusetts:")
    expect_equal(section$lines, list(
      "Reported cases: 10 (week ending Nov. 14)",
      "New hospital admissions: 5 (week ending Nov. 7)"
    ))
  })

  it("returns NULL when it has no lines", {
    expect_null(indicator_section("Massachusetts", list()))
  })
})

describe("indicators_summary()", {
  it("produces the approved block from all three sources", {
    payload <- fixture_summary()$payload

    expect_equal(payload$heading, "Other Covid indicators")
    expect_equal(payload$sections, list(
      list(
        heading = "Massachusetts, week ending Nov. 7:",
        lines = list(
          "Reported cases: 1,123, up from 978",
          "ER visits with Covid: 0.55%, up from 0.49% (CDC trend: increasing)",
          "New hospital admissions: 71, down from 84"
        )
      ),
      list(
        heading = "Closer to home, week ending Nov. 7:",
        lines = list(
          "Middlesex County: 7.0% of tests positive, up from 5.5%",
          "Framingham: 6 of 165 tests positive (3.6%)"
        )
      )
    ))
    expect_equal(payload$newest_week_end, "2026-11-07")
  })

  it("reports nothing unavailable when every source is current", {
    expect_length(fixture_summary()$unavailable, 0)
  })

  it("credits each source that contributed, with a link", {
    sources <- fixture_summary()$payload$sources

    expect_length(sources, 3)
    expect_equal(sources[[1]]$name, "Mass. Department of Public Health")
    expect_equal(sources[[1]]$url, DPH_PAGE_URL)
    for (source in sources) expect_match(source$url, "^https://")
  })

  it("never points readers at this repo or its dashboard", {
    # The figures aren't shown on the GitHub Pages dashboard, so nothing in
    # the block may suggest they can be seen there.
    text <- as.character(indicators_json(fixture_summary()$payload))

    expect_no_match(text, "github", ignore.case = TRUE)
    expect_no_match(text, "dashboard", ignore.case = TRUE)
  })

  it("drops a missing source's lines and keeps the rest", {
    sources <- fixture_sources()
    result <- indicators_summary(sources$dph, NULL, sources$cdc_hosp,
                                 today = FIXTURE_TODAY)

    expect_equal(result$payload$sections[[1]]$lines, list(
      "Reported cases: 1,123, up from 978",
      "New hospital admissions: 71, down from 84"
    ))
    expect_equal(result$unavailable, "CDC emergency department visits")
    expect_length(result$payload$sources, 2)
  })

  it("drops the undercount caveat along with the DPH lines", {
    sources <- fixture_sources()
    result <- indicators_summary(NULL, sources$cdc_ed, sources$cdc_hosp,
                                 today = FIXTURE_TODAY)

    expect_null(result$payload$caveat)
    expect_length(result$payload$sections, 1)
    expect_equal(result$unavailable, "Mass. DPH cases and testing")
  })

  it("leaves out a source whose newest week has gone stale", {
    sources <- fixture_sources()
    # DPH stops on 11/7; CDC keeps publishing for another four weeks.
    sources$cdc_ed$week_end <- sources$cdc_ed$week_end + 28
    sources$cdc_hosp$week_end <- sources$cdc_hosp$week_end + 28

    result <- indicators_summary(sources$dph, sources$cdc_ed, sources$cdc_hosp,
                                 today = as.Date("2026-12-11"))

    expect_equal(result$unavailable, "Mass. DPH cases and testing")
    expect_length(result$payload$sections, 1)
    expect_equal(result$payload$sections[[1]]$heading,
                 "Massachusetts, week ending Dec. 5:")
  })

  it("still counts a week as current on the last day of the limit", {
    limit_day <- as.Date("2026-11-07") + INDICATORS_MAX_AGE_DAYS

    expect_length(fixture_summary(today = limit_day)$unavailable, 0)
    expect_length(fixture_summary(today = limit_day + 1)$unavailable, 3)
  })

  it("returns an empty block when nothing is current", {
    result <- fixture_summary(today = as.Date("2027-03-01"))

    expect_length(result$payload$sections, 0)
    expect_null(result$payload$newest_week_end)
    expect_length(result$unavailable, 3)
  })
})

describe("save_indicators_summary()", {
  it("writes the summary with a generated timestamp", {
    path <- file.path(withr::local_tempdir(), "nested", "summary.json")

    expect_true(save_indicators_summary(fixture_summary()$payload, path))

    written <- jsonlite::read_json(path)
    expect_match(written$generated, "^\\d{4}-\\d{2}-\\d{2}T")
    expect_equal(written$sections[[2]]$lines[[2]],
                 "Framingham: 6 of 165 tests positive (3.6%)")
  })

  it("leaves the file alone when nothing has changed", {
    path <- file.path(withr::local_tempdir(), "summary.json")
    payload <- fixture_summary()$payload
    save_indicators_summary(payload, path)
    before <- readLines(path)

    expect_false(save_indicators_summary(payload, path))
    expect_equal(readLines(path), before)
  })

  it("rewrites the file when the content changes", {
    path <- file.path(withr::local_tempdir(), "summary.json")
    sources <- fixture_sources()
    save_indicators_summary(fixture_summary()$payload, path)

    changed <- indicators_summary(sources$dph, NULL, sources$cdc_hosp,
                                  today = FIXTURE_TODAY)$payload

    expect_true(save_indicators_summary(changed, path))
    expect_length(jsonlite::read_json(path)$sections[[1]]$lines, 2)
  })

  it("keeps a one-line section as a JSON array", {
    # auto_unbox would turn a length-one vector into a bare string, and the
    # email script loops over `lines`.
    path <- file.path(withr::local_tempdir(), "summary.json")
    sources <- fixture_sources()
    payload <- indicators_summary(NULL, sources$cdc_ed, NULL,
                                  today = FIXTURE_TODAY)$payload

    save_indicators_summary(payload, path)

    expect_match(paste(readLines(path), collapse = ""),
                 '"lines": \\[\\s*"ER visits with Covid')
  })

  it("writes an empty block as empty arrays, not nulls", {
    path <- file.path(withr::local_tempdir(), "summary.json")
    payload <- fixture_summary(today = as.Date("2027-03-01"))$payload

    save_indicators_summary(payload, path)

    expect_equal(jsonlite::read_json(path)$sections, list())
  })
})
