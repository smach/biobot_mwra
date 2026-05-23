# Utility functions for MWRA Biobot Data Monitor

#' Load state from JSON file
#'
#' @param state_file Path to the state JSON file
#' @return List with state data, or empty list if file doesn't exist
load_state <- function(state_file = "state/last_update.json") {
  if (file.exists(state_file)) {
    jsonlite::read_json(state_file)
  } else {
    list()
  }
}

#' Save state to JSON file
#'
#' @param state List containing state data
#' @param state_file Path to the state JSON file
save_state <- function(state, state_file = "state/last_update.json") {
  # Create directory if it doesn't exist
  dir_path <- dirname(state_file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  jsonlite::write_json(state, state_file, auto_unbox = TRUE, pretty = TRUE)
}

#' Update state after successful download
#'
#' @param sample_date The sample collection date (character, YYYY-MM-DD format)
#' @param pdf_url The relative PDF URL
#' @return The updated state list
update_state <- function(sample_date, pdf_url) {
  state <- list(
    last_sample_date = sample_date,
    last_pdf_url = pdf_url,
    last_check_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    last_download_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  save_state(state)
  state
}

#' Log a check without download (just update check time)
log_check <- function() {
  state <- load_state()
  state$last_check_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  save_state(state)
}

#' Retry wrapper for network operations
#'
#' @param fn Function to execute
#' @param max_attempts Maximum number of retry attempts
#' @param delay Delay in seconds between retries
#' @return Result of the function, or stops with error after all attempts fail
with_retry <- function(fn, max_attempts = 3, delay = 5) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      fn(),
      error = function(e) {
        if (attempt < max_attempts) {
          message(sprintf("Attempt %d failed: %s. Retrying in %ds...",
                          attempt, e$message, delay))
          Sys.sleep(delay)
          NULL
        } else {
          stop(sprintf("All %d attempts failed. Last error: %s",
                       max_attempts, e$message))
        }
      }
    )
    if (!is.null(result)) return(result)
  }
}

#' Fetch a URL using curl-impersonate (Chrome TLS fingerprint)
#'
#' MWRA's site is gated by an Imperva-style JS bot challenge that rejects
#' plain httr2/libcurl traffic based on TLS fingerprint. curl-impersonate
#' ships a patched curl whose ClientHello matches a real Chrome build, which
#' is usually enough to bypass the challenge without running a headless
#' browser.
#'
#' @param url URL to fetch
#' @param output_path Optional file path. If supplied, the response body is
#'   written there and the path is returned invisibly. If NULL, the body is
#'   read back as a UTF-8 string and returned.
#' @param timeout Request timeout in seconds
#' @param browser Which browser to impersonate (matches the wrapper script
#'   name, e.g. "chrome116" -> curl_chrome116). Override via the
#'   CURL_IMPERSONATE_BIN env var if you need a non-standard path.
impersonate_fetch <- function(url,
                              output_path = NULL,
                              timeout = 60,
                              browser = "chrome116") {
  curl_bin <- Sys.getenv("CURL_IMPERSONATE_BIN", unset = paste0("curl_", browser))

  to_temp <- is.null(output_path)
  if (to_temp) {
    output_path <- tempfile(fileext = ".bin")
    on.exit(unlink(output_path), add = TRUE)
  }

  args <- c(
    "-sS", "--fail", "--location",
    "--max-time", as.character(timeout),
    "--output", shQuote(output_path),
    shQuote(url)
  )
  status <- suppressWarnings(system2(curl_bin, args))
  if (status != 0) {
    stop(sprintf("curl-impersonate (%s) failed with exit code %d for URL: %s",
                 curl_bin, status, url))
  }

  if (to_temp) {
    paste(readLines(output_path, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n")
  } else {
    invisible(output_path)
  }
}

#' Set GitHub Actions output variable
#'
#' @param name Name of the output variable
#' @param value Value to set
set_gha_output <- function(name, value) {
  output_file <- Sys.getenv("GITHUB_OUTPUT")
  if (output_file != "") {
    cat(sprintf("%s=%s\n", name, value),
        file = output_file, append = TRUE)
  }
}
