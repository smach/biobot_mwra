# Tests for R/utils.R

describe("load_state()", {
  it("returns empty list when file doesn't exist", {
    withr::local_tempdir()
    result <- load_state("nonexistent/state.json")
    expect_type(result, "list")
    expect_length(result, 0)
  })

  it("loads valid JSON state file", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "state.json")

    # Create a test state file
    test_state <- list(
      last_sample_date = "2024-12-25",
      last_pdf_url = "/biobot/test.pdf"
    )
    jsonlite::write_json(test_state, state_file, auto_unbox = TRUE)

    result <- load_state(state_file)

    expect_equal(result$last_sample_date, "2024-12-25")
    expect_equal(result$last_pdf_url, "/biobot/test.pdf")
  })
})

describe("save_state()", {
  it("creates directory if it doesn't exist", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "new_dir", "state.json")

    save_state(list(test = "value"), state_file)

    expect_true(dir.exists(dirname(state_file)))
    expect_true(file.exists(state_file))
  })

  it("saves state as valid JSON", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "state.json")

    test_state <- list(
      last_sample_date = "2024-12-25",
      last_pdf_url = "/biobot/data.pdf",
      count = 42
    )
    save_state(test_state, state_file)

    # Read back and verify
    loaded <- jsonlite::read_json(state_file)
    expect_equal(loaded$last_sample_date, "2024-12-25")
    expect_equal(loaded$last_pdf_url, "/biobot/data.pdf")
    expect_equal(loaded$count, 42)
  })

  it("overwrites existing state file", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "state.json")

    save_state(list(value = "first"), state_file)
    save_state(list(value = "second"), state_file)

    loaded <- jsonlite::read_json(state_file)
    expect_equal(loaded$value, "second")
  })
})

describe("update_state()", {
  it("creates state with sample date and URL", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "state.json")

    # Temporarily replace save_state function
    original_save_state <- save_state
    assign("save_state", function(state, sf = "state/last_update.json") {
      jsonlite::write_json(state, state_file, auto_unbox = TRUE, pretty = TRUE)
    }, envir = .GlobalEnv)
    on.exit(assign("save_state", original_save_state, envir = .GlobalEnv), add = TRUE)

    result <- update_state("2024-12-25", "/biobot/test.pdf")

    expect_equal(result$last_sample_date, "2024-12-25")
    expect_equal(result$last_pdf_url, "/biobot/test.pdf")
    expect_true(!is.null(result$last_check_time))
    expect_true(!is.null(result$last_download_time))
  })

  it("formats timestamps in ISO 8601 UTC format", {
    # Temporarily replace save_state function
    original_save_state <- save_state
    assign("save_state", function(state, sf) invisible(NULL), envir = .GlobalEnv)
    on.exit(assign("save_state", original_save_state, envir = .GlobalEnv), add = TRUE)

    result <- update_state("2024-12-25", "/biobot/test.pdf")

    # Check timestamp format: YYYY-MM-DDTHH:MM:SSZ
    expect_match(result$last_check_time, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
    expect_match(result$last_download_time, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  })
})

describe("with_retry()", {
  it("returns result on first success", {
    call_count <- 0
    result <- with_retry(function() {
      call_count <<- call_count + 1
      "success"
    }, max_attempts = 3, delay = 1)

    expect_equal(result, "success")
    expect_equal(call_count, 1)
  })

  it("retries on failure and succeeds", {
    call_count <- 0
    # Note: delay must be integer for sprintf %d format in with_retry
    result <- with_retry(function() {
      call_count <<- call_count + 1
      if (call_count < 3) stop("temporary error")
      "success after retries"
    }, max_attempts = 3, delay = 1)

    expect_equal(result, "success after retries")
    expect_equal(call_count, 3)
  })

  it("stops after max_attempts failures", {
    call_count <- 0
    expect_error(
      with_retry(function() {
        call_count <<- call_count + 1
        stop("persistent error")
      }, max_attempts = 3, delay = 1),
      "All 3 attempts failed"
    )
    expect_equal(call_count, 3)
  })

  it("includes last error message in final error", {
    expect_error(
      with_retry(function() {
        stop("specific error message")
      }, max_attempts = 2, delay = 1),
      "specific error message"
    )
  })
})

describe("set_gha_output()", {
  it("does nothing when GITHUB_OUTPUT is not set", {
    withr::local_envvar(GITHUB_OUTPUT = "")

    # Should not error
    expect_no_error(set_gha_output("test_name", "test_value"))
  })

  it("writes to GITHUB_OUTPUT file when set", {
    temp_file <- withr::local_tempfile()
    withr::local_envvar(GITHUB_OUTPUT = temp_file)

    set_gha_output("my_output", "my_value")

    content <- readLines(temp_file)
    expect_equal(content, "my_output=my_value")
  })

  it("appends multiple outputs to file", {
    temp_file <- withr::local_tempfile()
    withr::local_envvar(GITHUB_OUTPUT = temp_file)

    set_gha_output("first", "value1")
    set_gha_output("second", "value2")

    content <- readLines(temp_file)
    expect_equal(content[1], "first=value1")
    expect_equal(content[2], "second=value2")
  })
})

describe("log_check()", {
  it("updates last_check_time in existing state", {
    temp_dir <- withr::local_tempdir()
    state_file <- file.path(temp_dir, "state.json")

    # Create initial state
    initial_state <- list(
      last_sample_date = "2024-12-20",
      last_pdf_url = "/biobot/old.pdf",
      last_check_time = "2024-12-20T10:00:00Z"
    )
    jsonlite::write_json(initial_state, state_file, auto_unbox = TRUE)

    # Temporarily replace functions to use temp file
    original_load_state <- load_state
    original_save_state <- save_state
    assign("load_state", function(sf = "state/last_update.json") {
      jsonlite::read_json(state_file)
    }, envir = .GlobalEnv)
    assign("save_state", function(state, sf = "state/last_update.json") {
      jsonlite::write_json(state, state_file, auto_unbox = TRUE, pretty = TRUE)
    }, envir = .GlobalEnv)
    on.exit({
      assign("load_state", original_load_state, envir = .GlobalEnv)
      assign("save_state", original_save_state, envir = .GlobalEnv)
    }, add = TRUE)

    log_check()

    # Verify state was updated
    updated <- jsonlite::read_json(state_file)
    expect_equal(updated$last_sample_date, "2024-12-20")
    expect_equal(updated$last_pdf_url, "/biobot/old.pdf")
    expect_true(updated$last_check_time != "2024-12-20T10:00:00Z")
  })
})
