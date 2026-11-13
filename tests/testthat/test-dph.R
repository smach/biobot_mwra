# Tests for R/07_dph.R
#
# Fixture: dph_dashboard.xlsx, a trimmed copy of the real DPH workbook posted
# 2026-11-12 -- the three sheets the parser reads, the last eight weeks, and a
# few places (Middlesex, Worcester, and "Unknown" counties; Framingham, Natick,
# and Wrentham). Values and suppression markers ("1-4", "*") are the real ones.
# It was written with writexl, so it checks the parser's logic rather than
# DPH's exact cell formatting; the full workbook was checked by hand when this
# was written.

fixture_workbook <- function() test_fixture_path("dph_dashboard.xlsx")

fixture_dph <- function() extract_dph_series(fixture_workbook())

rows_for <- function(data, level) data[data$level == level, ]

describe("validate_dph_workbook()", {
  it("accepts the real workbook layout", {
    expect_equal(validate_dph_workbook(fixture_workbook()), fixture_workbook())
  })

  it("rejects a bot-wall page saved under an .xlsx name", {
    fake <- withr::local_tempfile(fileext = ".xlsx")
    writeLines("<html><title>Not allowed | Mass Gov</title></html>", fake)

    expect_error(validate_dph_workbook(fake), "not an Excel workbook")
  })

  it("rejects a workbook that has lost a sheet we need", {
    original <- DPH_SHEETS
    withr::defer(assign("DPH_SHEETS", original, envir = .GlobalEnv))
    assign("DPH_SHEETS", c(original, extra = "Sheet DPH Removed"),
           envir = .GlobalEnv)

    expect_error(validate_dph_workbook(fixture_workbook()),
                 "missing sheet.*Sheet DPH Removed")
  })
})

describe("fetch_dph_workbook()", {
  it("returns the path of a validated download", {
    local_fetch_stubs(fixture_workbook())
    dest <- withr::local_tempfile(fileext = ".xlsx")

    expect_equal(fetch_dph_workbook(dest), dest)
    expect_true(file.exists(dest))
  })

  it("errors when mass.gov serves a page instead of the file", {
    page <- withr::local_tempfile(fileext = ".html")
    writeLines("<html><body>Not allowed</body></html>", page)
    local_fetch_stubs(page)

    expect_error(fetch_dph_workbook(withr::local_tempfile()),
                 "not an Excel workbook")
  })
})

describe("dph_number()", {
  it("reads plain numbers, with or without padding", {
    expect_equal(dph_number(c("12", " 7 ", "0.0701")), c(12, 7, 0.0701))
  })

  it("treats every suppression marker as missing", {
    expect_true(all(is.na(dph_number(c("1-4", "*", "**", NA)))))
  })
})

describe("dph_date()", {
  it("converts an Excel serial day number", {
    expect_equal(dph_date("46333"), as.Date("2026-11-07"))
  })

  it("accepts ISO dates, with or without a time", {
    expect_equal(dph_date(c("2026-11-07", "2026-11-07 00:00:00")),
                 as.Date(c("2026-11-07", "2026-11-07")))
  })

  it("returns NA for text that isn't a date", {
    expect_true(is.na(dph_date("week 45")))
  })
})

describe("extract_dph_series()", {
  it("returns one row per week for the state, county, and town", {
    data <- fixture_dph()

    expect_equal(unique(data$level), c("state", "county", "town"))
    expect_equal(unique(data$geography),
                 c("Massachusetts", "Middlesex County", "Framingham"))
    expect_equal(as.vector(table(data$level)[c("state", "county", "town")]),
                 c(8, 8, 8))
    expect_equal(max(data$week_end), as.Date("2026-11-07"))
  })

  it("sorts each place oldest to newest", {
    data <- fixture_dph()
    for (level in unique(data$level)) {
      expect_false(is.unsorted(rows_for(data, level)$week_end))
    }
  })

  it("reads the statewide case counts and leaves testing columns empty", {
    state <- rows_for(fixture_dph(), "state")

    expect_equal(tail(state$cases, 2), c(978, 1123))
    expect_true(all(is.na(state$tests)))
  })

  it("reads the county's testing figures", {
    latest <- tail(rows_for(fixture_dph(), "county"), 1)

    expect_equal(latest$cases, 216)
    expect_equal(latest$tests, 3662)
    expect_equal(latest$positive_tests, 257)
    expect_equal(latest$positivity, 257 / 3662, tolerance = 1e-6)
  })

  it("keeps a suppressed count as NA with the original text beside it", {
    town <- rows_for(fixture_dph(), "town")
    suppressed <- town[town$week_end == as.Date("2026-10-31"), ]

    expect_true(is.na(suppressed$cases))
    expect_equal(suppressed$cases_raw, "1-4")
    # Tests are published exactly even when the case count is hidden.
    expect_equal(suppressed$tests, 147)
    expect_equal(suppressed$positive_tests, 2)
  })

  it("records the date DPH last updated the file", {
    expect_equal(unique(fixture_dph()$updated), as.Date("2026-11-12"))
  })

  it("errors when the town isn't in the workbook", {
    expect_error(extract_dph_series(fixture_workbook(), town = "Atlantis"),
                 "No rows for Atlantis")
  })

  it("errors when the county isn't in the workbook", {
    expect_error(extract_dph_series(fixture_workbook(), county = "Wessex"),
                 "No rows for Wessex County")
  })
})

describe("save_dph_data() and read_dph_data()", {
  it("round-trips the series through CSV", {
    dir <- withr::local_tempdir()
    path <- file.path(dir, "nested", "dph_covid.csv")
    data <- fixture_dph()

    save_dph_data(data, path)

    expect_equal(read_dph_data(path), data)
  })

  it("returns NULL when nothing has been stored yet", {
    expect_null(read_dph_data(file.path(withr::local_tempdir(), "none.csv")))
  })
})

describe("dph_lines()", {
  it("writes the three lines in the approved wording", {
    lines <- dph_lines(fixture_dph())

    expect_equal(lines$state$text, "Reported cases: 1,123, up from 978")
    expect_equal(lines$county$text,
                 "Middlesex County: 7.0% of tests positive, up from 5.5%")
    expect_equal(lines$town$text, "Framingham: 6 of 165 tests positive (3.6%)")
  })

  it("dates every line to the week it covers and credits DPH", {
    for (line in dph_lines(fixture_dph())) {
      expect_equal(line$week_end, as.Date("2026-11-07"))
      expect_equal(line$source, "dph")
    }
  })

  it("drops the town line when no tests were reported", {
    data <- fixture_dph()
    data$tests[data$level == "town"] <- 0

    expect_null(dph_lines(data)$town)
  })

  it("gives the state line without a comparison when last week is missing", {
    data <- fixture_dph()
    data <- data[!(data$level == "state" &
                     data$week_end == as.Date("2026-10-31")), ]

    expect_equal(dph_lines(data)$state$text, "Reported cases: 1,123")
  })

  it("returns no lines when there is no data", {
    expect_equal(dph_lines(NULL), list(state = NULL, county = NULL, town = NULL))
  })
})
