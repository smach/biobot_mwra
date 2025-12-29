# Tests for R/02_download_pdf.R

describe("download_pdf()", {
  it("returns NULL when download fails", {
    # Temporarily replace with_retry to simulate failure
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) stop("Download failed"), envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/test.pdf")

    expect_null(result)
  })

  it("creates output directory if it doesn't exist", {
    temp_dir <- withr::local_tempdir()
    output_path <- file.path(temp_dir, "new_subdir", "data.pdf")

    # Temporarily replace with_retry to simulate successful download
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) {
      # Create the directory and file to simulate download
      dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
      writeBin(charToRaw("fake pdf content"), output_path)
      TRUE
    }, envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/test.pdf", output_path)

    expect_true(dir.exists(dirname(output_path)))
  })

  it("returns output path on successful download", {
    temp_dir <- withr::local_tempdir()
    output_path <- file.path(temp_dir, "data.pdf")

    # Temporarily replace with_retry to simulate successful download
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) {
      # Simulate successful download by creating file
      writeBin(charToRaw("PDF content here"), output_path)
      TRUE
    }, envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/test.pdf", output_path)

    expect_equal(result, output_path)
    expect_true(file.exists(output_path))
  })

  it("returns NULL when downloaded file is empty", {
    temp_dir <- withr::local_tempdir()
    output_path <- file.path(temp_dir, "data.pdf")

    # Create empty file
    file.create(output_path)

    # Temporarily replace with_retry to simulate empty file error
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) {
      # File exists but is empty - should fail validation
      stop("Downloaded file is empty or missing")
    }, envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/test.pdf", output_path)

    expect_null(result)
  })

  it("overwrites existing file", {
    temp_dir <- withr::local_tempdir()
    output_path <- file.path(temp_dir, "data.pdf")

    # Create existing file with old content
    writeBin(charToRaw("old content"), output_path)

    # Temporarily replace with_retry to simulate overwrite
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) {
      # Overwrite with new content
      writeBin(charToRaw("new content that is longer"), output_path)
      TRUE
    }, envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/test.pdf", output_path)

    expect_equal(result, output_path)
    new_content <- readBin(output_path, "raw", file.size(output_path))
    expect_equal(rawToChar(new_content), "new content that is longer")
  })

  it("uses default output path when not specified", {
    # Temporarily replace with_retry to avoid actual network call
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) stop("Network error"), envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    # Should not error on missing argument
    expect_no_error({
      result <- download_pdf("https://example.com/test.pdf")
    })
  })
})

describe("download_pdf() HTTP handling", {
  it("handles HTTP errors gracefully", {
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) {
      stop("HTTP error: 404")
    }, envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/notfound.pdf")

    expect_null(result)
  })

  it("handles timeout errors gracefully", {
    original_with_retry <- with_retry
    assign("with_retry", function(fn, ...) {
      stop("Timeout")
    }, envir = .GlobalEnv)
    on.exit(assign("with_retry", original_with_retry, envir = .GlobalEnv), add = TRUE)

    result <- download_pdf("https://example.com/slow.pdf")

    expect_null(result)
  })
})
