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
