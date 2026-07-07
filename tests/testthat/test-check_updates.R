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

# Test PDF URL construction
describe("build_pdf_url()", {
  it("builds the dated /media/file/ URL", {
    url <- build_pdf_url(as.Date("2026-06-25"))
    expect_equal(url, "https://www.mwra.com/media/file/mwradata20260625-datapdf")
  })

  it("zero-pads single-digit months and days", {
    url <- build_pdf_url(as.Date("2026-01-05"))
    expect_equal(url, "https://www.mwra.com/media/file/mwradata20260105-datapdf")
  })
})

# Test extracting the posting date back out of a PDF URL
describe("parse_pdf_url_date()", {
  it("parses the date from an absolute URL", {
    d <- parse_pdf_url_date("https://www.mwra.com/media/file/mwradata20260625-datapdf")
    expect_equal(d, as.Date("2026-06-25"))
  })

  it("parses the date from a relative URL", {
    d <- parse_pdf_url_date("/media/file/mwradata20260625-datapdf")
    expect_equal(d, as.Date("2026-06-25"))
  })

  it("returns NA when no date is present", {
    expect_true(is.na(parse_pdf_url_date("/some/other/path")))
    expect_true(is.na(parse_pdf_url_date(NULL)))
  })
})

# Test the PDF magic-byte check used to distinguish real PDFs from challenge pages
describe("is_pdf_file()", {
  it("returns TRUE for a file starting with %PDF", {
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f), add = TRUE)
    writeBin(charToRaw("%PDF-1.7\nbinary..."), f)
    expect_true(is_pdf_file(f))
  })

  it("returns FALSE for an HTML bot-challenge page", {
    f <- tempfile(fileext = ".html")
    on.exit(unlink(f), add = TRUE)
    writeChar("<html>Please wait while your request is being verified...</html>",
              f, eos = NULL)
    expect_false(is_pdf_file(f))
  })

  it("returns FALSE for a missing or empty file", {
    expect_false(is_pdf_file(tempfile()))
    f <- tempfile()
    file.create(f)
    on.exit(unlink(f), add = TRUE)
    expect_false(is_pdf_file(f))
  })
})
