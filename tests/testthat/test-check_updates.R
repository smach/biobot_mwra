# Tests for R/01_check_updates.R
# Note: Testing web scraping functions requires careful mocking.
# These tests focus on the logic that can be tested without network calls.

describe("check_for_updates()", {
  it("returns expected structure on error", {
    # Temporarily replace with_retry to simulate network failure
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) stop("Network error"), envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- check_for_updates()

    expect_type(result, "list")
    expect_false(result$is_new)
    expect_null(result$sample_date)
    expect_null(result$pdf_url)
    expect_null(result$full_pdf_url)
    expect_true(!is.null(result$error))
  })

  it("has all expected fields in return value", {
    # Temporarily replace with_retry to simulate network failure (quick way to test structure)
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) stop("Test error"), envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- check_for_updates()

    expect_true("is_new" %in% names(result))
    expect_true("sample_date" %in% names(result))
    expect_true("pdf_url" %in% names(result))
    expect_true("full_pdf_url" %in% names(result))
    expect_true("previous_date" %in% names(result))
    expect_true("error" %in% names(result))
  })
})

# Test date comparison logic separately
describe("date comparison logic", {
  it("correctly identifies newer dates", {
    # Test the date comparison that happens in check_for_updates
    current_date <- as.Date("2024-12-25")
    previous_date <- as.Date("2024-12-20")

    expect_true(current_date > previous_date)
  })

  it("correctly identifies same dates", {
    current_date <- as.Date("2024-12-25")
    previous_date <- as.Date("2024-12-25")

    expect_false(current_date > previous_date)
  })

  it("correctly identifies older dates", {
    current_date <- as.Date("2024-12-20")
    previous_date <- as.Date("2024-12-25")

    expect_false(current_date > previous_date)
  })
})

# Test date parsing logic
describe("date parsing", {
  it("parses MM/DD/YYYY format correctly", {
    date_str <- "12/25/2024"
    parsed <- as.Date(date_str, format = "%m/%d/%Y")

    expect_equal(parsed, as.Date("2024-12-25"))
  })

  it("parses single-digit month/day correctly", {
    date_str <- "1/5/2024"
    parsed <- as.Date(date_str, format = "%m/%d/%Y")

    expect_equal(parsed, as.Date("2024-01-05"))
  })

  it("formats date as YYYY-MM-DD", {
    date <- as.Date("2024-12-25")
    formatted <- format(date, "%Y-%m-%d")

    expect_equal(formatted, "2024-12-25")
  })
})

# Test regex pattern for date extraction
describe("date pattern matching", {
  it("extracts date from 'samples collected through' text", {
    text <- "Some text samples collected through 12/25/2024 more text"
    pattern <- "samples collected through (\\d{1,2}/\\d{1,2}/\\d{4})"
    match <- regmatches(text, regexec(pattern, text))[[1]]

    expect_length(match, 2)
    expect_equal(match[2], "12/25/2024")
  })

  it("handles single-digit dates in pattern", {
    text <- "samples collected through 1/5/2024"
    pattern <- "samples collected through (\\d{1,2}/\\d{1,2}/\\d{4})"
    match <- regmatches(text, regexec(pattern, text))[[1]]

    expect_equal(match[2], "1/5/2024")
  })

  it("returns empty when pattern not found", {
    text <- "No date here"
    pattern <- "samples collected through (\\d{1,2}/\\d{1,2}/\\d{4})"
    match <- regmatches(text, regexec(pattern, text))[[1]]

    expect_length(match, 0)
  })
})

# Test PDF link pattern
describe("PDF link pattern matching", {
  it("matches mwradata-datapdf pattern (old format)", {
    hrefs <- c("/other/file.pdf", "/biobot/mwradata123-datapdf.pdf", "/another.html")
    pattern <- "mwradata.*-data"

    matches <- hrefs[grepl(pattern, hrefs, ignore.case = TRUE)]

    expect_length(matches, 1)
    expect_equal(matches, "/biobot/mwradata123-datapdf.pdf")
  })

  it("matches mwradata-data pattern (new format)", {
    hrefs <- c("/other/file.pdf", "/media/file/mwradata20260220-data", "/another.html")
    pattern <- "mwradata.*-data"

    matches <- hrefs[grepl(pattern, hrefs, ignore.case = TRUE)]

    expect_length(matches, 1)
    expect_equal(matches, "/media/file/mwradata20260220-data")
  })

  it("is case insensitive", {
    hrefs <- c("/biobot/MWRADATA-DATAPDF.PDF")
    pattern <- "mwradata.*-data"

    matches <- hrefs[grepl(pattern, hrefs, ignore.case = TRUE)]

    expect_length(matches, 1)
  })
})
