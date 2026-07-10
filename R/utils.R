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

#' Days since the most recent successfully processed sample date
#'
#' Used to decide whether a bot challenge can be tolerated as a transient
#' no-op: `last_sample_date` only advances on successful data runs and (unlike
#' any per-run counter) is committed to the repo, so it survives across fresh
#' CI checkouts. If the newest data is old AND we're being challenged, the
#' pipeline hasn't truly succeeded in a while and should fail loudly.
#'
#' @param state State list (defaults to the persisted state)
#' @return Numeric days since last_sample_date, or NA if never recorded
data_staleness_days <- function(state = load_state()) {
  if (is.null(state$last_sample_date)) {
    return(NA_real_)
  }
  as.numeric(Sys.Date() - as.Date(state$last_sample_date))
}

#' Retry wrapper for network operations
#'
#' Retries with exponential backoff and jitter so the attempts span a wider
#' time window. MWRA's Imperva bot challenge is transient and often clears
#' within a few minutes, so spreading retries out gives a much better chance
#' of catching a moment when the real page is served.
#'
#' @param fn Function to execute
#' @param max_attempts Maximum number of retry attempts
#' @param delay Base delay in seconds; the wait before attempt N is
#'   `delay * backoff^(N-1)` (capped at `max_delay`) plus random jitter
#' @param backoff Multiplier applied to the delay after each failed attempt
#' @param max_delay Maximum delay in seconds between attempts
#' @param jitter Fraction of random jitter to add to each delay (0 disables it)
#' @return Result of the function, or stops with error after all attempts fail
with_retry <- function(fn, max_attempts = 5, delay = 15,
                       backoff = 2, max_delay = 120, jitter = 0.25) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      fn(),
      error = function(e) {
        if (attempt < max_attempts) {
          wait <- min(max_delay, delay * backoff^(attempt - 1))
          if (jitter > 0) {
            wait <- wait + stats::runif(1, 0, wait * jitter)
          }
          message(sprintf("Attempt %d failed: %s. Retrying in %.0fs...",
                          attempt, e$message, wait))
          Sys.sleep(wait)
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

# Marker embedded in errors raised when a fetch returns a bot-challenge page,
# so callers can tell a transient challenge apart from a genuine failure after
# with_retry() wraps and re-raises the message.
BOT_CHALLENGE_TAG <- "__BOT_CHALLENGE__"

#' Detect an Imperva/Incapsula bot-challenge response
#'
#' MWRA's site sits behind an Imperva bot wall that intermittently serves a
#' small obfuscated-JS "please wait while we verify your request" interstitial
#' instead of the real page, even to curl-impersonate. Such a response is a
#' transient condition (not a genuine error and not real data), so we detect
#' it explicitly to avoid both mis-parsing it and firing false alarms.
#'
#' @param html Character scalar with the response body
#' @return TRUE if the body looks like a bot-challenge/interstitial page
is_bot_challenge <- function(html) {
  if (is.null(html) || length(html) == 0 || is.na(html[1])) {
    return(FALSE)
  }
  signatures <- c(
    "Please wait while your request is being verified",
    "Request unsuccessful. Incapsula",
    "_Incapsula_Resource",
    "window._Incapsula",
    "Powered by Incapsula"
  )
  any(vapply(
    signatures,
    function(sig) grepl(sig, html, fixed = TRUE),
    logical(1)
  ))
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
#' @param cookie_jar Optional path to a cookie file. When supplied it is used
#'   for both reading (`-b`) and writing (`-c`), so cookies an Imperva
#'   challenge hands out on one request are replayed on the next.
impersonate_fetch <- function(url,
                              output_path = NULL,
                              timeout = 60,
                              browser = "chrome116",
                              cookie_jar = NULL) {
  curl_bin <- Sys.getenv("CURL_IMPERSONATE_BIN", unset = paste0("curl_", browser))

  to_temp <- is.null(output_path)
  if (to_temp) {
    output_path <- tempfile(fileext = ".bin")
    on.exit(unlink(output_path), add = TRUE)
  }

  args <- c(
    "-sS", "--fail", "--location",
    "--max-time", as.character(timeout),
    if (!is.null(cookie_jar)) c("-c", shQuote(cookie_jar), "-b", shQuote(cookie_jar)),
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
