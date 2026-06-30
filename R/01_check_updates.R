# Check MWRA website for new Biobot data updates

#' Build the PDF URL for a given posting date
#'
#' MWRA serves each Biobot report as a dated PDF at a stable, predictable path,
#' e.g. https://www.mwra.com/media/file/mwradata20260625-datapdf for the report
#' posted 2026-06-25. Because the URL encodes the date, we can locate the latest
#' report by probing these URLs directly and skip the HTML index page
#' (biobotdata.htm), which sits behind an Imperva JavaScript bot challenge that
#' non-browser clients can't pass.
#'
#' Note: the date in the URL is the *posting* date, which runs a few days after
#' the "samples collected through" date shown inside the PDF. We therefore can't
#' compute it from the collection date; we have to probe for it.
#'
#' @param date A Date object (the posting date)
#' @param base_url MWRA base URL
#' @return Character, the absolute PDF URL
build_pdf_url <- function(date, base_url = "https://www.mwra.com") {
  sprintf("%s/media/file/mwradata%s-datapdf", base_url, format(date, "%Y%m%d"))
}

#' Extract the YYYYMMDD posting date from a Biobot PDF URL
#'
#' @param url A PDF URL (absolute or relative) containing "mwradataYYYYMMDD"
#' @return A Date, or NA if no date could be parsed
parse_pdf_url_date <- function(url) {
  if (is.null(url) || is.na(url)) return(as.Date(NA))
  m <- regmatches(url, regexec("mwradata(\\d{8})", url))[[1]]
  if (length(m) < 2) return(as.Date(NA))
  as.Date(m[2], format = "%Y%m%d")
}

#' Check MWRA for the most recent Biobot data PDF
#'
#' Probes dated PDF URLs backwards from today and returns the newest one that
#' actually exists, comparing its posting date against stored state to decide
#' whether it's new. No HTML scraping is involved.
#'
#' @param max_lookback_days How many days back from today to probe
#' @return List with:
#'   - is_new: logical, TRUE if a newer PDF than last processed is available
#'   - sample_date: character, the PDF posting date (YYYY-MM-DD)
#'   - pdf_url: character, relative URL to the PDF
#'   - full_pdf_url: character, full URL to the PDF
#'   - previous_date: character, the previous posting date (or NULL)
#'   - error: character, error message if the check failed (or NULL)
check_for_updates <- function(max_lookback_days = 30) {
  base_url <- "https://www.mwra.com"

  tryCatch({
    state <- load_state()

    found_date <- NULL
    found_url <- NULL
    saw_challenge <- FALSE

    # Probe newest-first; the first PDF that exists is the latest report.
    for (offset in seq.int(0L, max_lookback_days)) {
      candidate <- Sys.Date() - offset
      pdf_url <- build_pdf_url(candidate, base_url)
      tmp <- tempfile(fileext = ".pdf")

      # Only transport-level failures (no HTTP response) are retried; a clean
      # 404 just means "not posted for that date" and moves on immediately.
      status <- with_retry(function() {
        s <- impersonate_http_status(pdf_url, tmp)
        if (is.na(s)) {
          stop(sprintf("No HTTP response while probing %s", pdf_url))
        }
        s
      }, max_attempts = 3, delay = 10)

      if (status == 200 && is_pdf_file(tmp)) {
        found_date <- candidate
        found_url <- pdf_url
        unlink(tmp)
        break
      }

      # A 200 that isn't a PDF is almost certainly the Imperva challenge page.
      if (status == 200) saw_challenge <- TRUE
      unlink(tmp)
    }

    if (is.null(found_date)) {
      if (saw_challenge) {
        stop(sprintf(paste0(
          "No PDF found in the last %d days; the PDF endpoint returned a ",
          "bot-challenge page instead of a PDF (HTTP 200, non-PDF body)."),
          max_lookback_days))
      }
      stop(sprintf("No Biobot PDF found in the last %d days.", max_lookback_days))
    }

    sample_date_str <- format(found_date, "%Y-%m-%d")
    pdf_url_rel <- sub(base_url, "", found_url, fixed = TRUE)

    # Decide novelty by comparing posting dates. State's last_pdf_url carries
    # the date we last processed; fall back to last_sample_date if needed.
    previous_date <- parse_pdf_url_date(state$last_pdf_url)
    if (is.na(previous_date) && !is.null(state$last_sample_date)) {
      previous_date <- as.Date(state$last_sample_date)
    }

    is_new <- if (is.na(previous_date)) {
      TRUE  # First run, always download
    } else {
      found_date > previous_date
    }

    list(
      is_new = is_new,
      sample_date = sample_date_str,
      pdf_url = pdf_url_rel,
      full_pdf_url = found_url,
      previous_date = if (is.na(previous_date)) NULL else format(previous_date, "%Y-%m-%d"),
      error = NULL
    )

  }, error = function(e) {
    message(sprintf("Error checking for updates: %s", e$message))
    list(
      is_new = FALSE,
      sample_date = NULL,
      pdf_url = NULL,
      full_pdf_url = NULL,
      previous_date = NULL,
      error = e$message
    )
  })
}
