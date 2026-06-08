# Check MWRA website for new Biobot data updates

#' Check MWRA webpage for new Biobot data
#'
#' Scrapes the MWRA Biobot page to find the current sample date and PDF link,
#' then compares with stored state to determine if new data is available.
#'
#' @return List with:
#'   - is_new: logical, TRUE if new data is available
#'   - sample_date: character, the sample collection date (YYYY-MM-DD)
#'   - pdf_url: character, relative URL to the PDF
#'   - full_pdf_url: character, full URL to the PDF
#'   - previous_date: character, the previous sample date (or NULL)
#'   - error: character, error message if check failed (or NULL)
check_for_updates <- function() {
  base_url <- "https://www.mwra.com"
  page_url <- paste0(base_url, "/biobot/biobotdata.htm")

  tryCatch({
    # Fetch webpage and extract date + PDF link with retry logic.
    # Retries cover both network failures AND cases where the page loads
    # but contains unexpected content (e.g., maintenance page).
    result <- with_retry(function() {
      cookie_header <- NULL
      html <- impersonate_fetch(page_url)

      # curl-impersonate clears Imperva's TLS-fingerprint tier roughly 80%
      # of the time. When Imperva escalates to its JS challenge, the body
      # is the "Please wait..." interstitial - fall back to a real headless
      # Chrome that can execute the challenge and earn a clearance cookie.
      if (is_imperva_challenge(html)) {
        message("  Imperva challenge detected, falling back to headless Chrome...")
        ch <- chromote_fetch(page_url)
        html <- ch$html
        cookie_header <- ch$cookie_header
      }

      page <- rvest::read_html(html)

      # Extract date from "samples collected through MM/DD/YYYY"
      page_text <- rvest::html_text2(page)
      date_pattern <- "samples collected through (\\d{1,2}/\\d{1,2}/\\d{4})"
      date_match <- regmatches(page_text, regexec(date_pattern, page_text))[[1]]

      if (length(date_match) < 2) {
        stop(sprintf(
          "Could not find sample date on webpage. Page text starts with: %s",
          substr(page_text, 1, 200)
        ))
      }

      # Extract PDF link
      all_hrefs <- rvest::html_attr(rvest::html_elements(page, "a"), "href")
      pdf_links <- all_hrefs[grepl("mwradata.*-data", all_hrefs, ignore.case = TRUE)]

      if (length(pdf_links) == 0) {
        stop("Could not find PDF link on webpage")
      }

      list(sample_date_raw = date_match[2],
           pdf_url = pdf_links[1],
           cookie_header = cookie_header)
    }, max_attempts = 3, delay = 30)

    # Parse the date (MM/DD/YYYY format)
    sample_date <- as.Date(result$sample_date_raw, format = "%m/%d/%Y")
    sample_date_str <- format(sample_date, "%Y-%m-%d")
    pdf_url <- result$pdf_url
    full_pdf_url <- if (grepl("^https?://", pdf_url)) {
      pdf_url
    } else {
      paste0(base_url, pdf_url)
    }

    # Load previous state and compare
    state <- load_state()

    # Determine if this is new data
    is_new <- if (is.null(state$last_sample_date)) {
      TRUE  # First run, always download
    } else {
      sample_date > as.Date(state$last_sample_date)
    }

    list(
      is_new = is_new,
      sample_date = sample_date_str,
      pdf_url = pdf_url,
      full_pdf_url = full_pdf_url,
      previous_date = state$last_sample_date,
      cookie_header = result$cookie_header,
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
      cookie_header = NULL,
      error = e$message
    )
  })
}
