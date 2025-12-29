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
    # Fetch the webpage with retry logic
    page <- with_retry(function() {
      rvest::read_html(page_url)
    })

    # Get full page text
    page_text <- rvest::html_text2(page)

    # Extract date from "samples collected through MM/DD/YYYY"
    date_pattern <- "samples collected through (\\d{1,2}/\\d{1,2}/\\d{4})"
    date_match <- regmatches(page_text, regexec(date_pattern, page_text))[[1]]

    if (length(date_match) < 2) {
      stop("Could not find sample date on webpage")
    }

    # Parse the date (MM/DD/YYYY format)
    sample_date_raw <- date_match[2]
    sample_date <- as.Date(sample_date_raw, format = "%m/%d/%Y")
    sample_date_str <- format(sample_date, "%Y-%m-%d")

    # Extract PDF link - find links containing "mwradata" and "datapdf"
    all_links <- rvest::html_elements(page, "a")
    all_hrefs <- rvest::html_attr(all_links, "href")

    # Filter for data PDF link
    pdf_links <- all_hrefs[grepl("mwradata.*-datapdf", all_hrefs, ignore.case = TRUE)]

    if (length(pdf_links) == 0) {
      stop("Could not find PDF link on webpage")
    }

    pdf_url <- pdf_links[1]
    full_pdf_url <- paste0(base_url, pdf_url)

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
