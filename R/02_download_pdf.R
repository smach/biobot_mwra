# Download PDF from MWRA website

#' Download PDF from MWRA website
#'
#' Downloads the Biobot data PDF, keeping only the most recent one.
#'
#' @param pdf_url Full URL to the PDF file
#' @param output_path Where to save the PDF (overwrites previous)
#' @return Path to the downloaded file, or NULL if download failed
download_pdf <- function(pdf_url, output_path = "data/latest_data.pdf") {
  tryCatch({
    # Ensure directory exists
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

    # Download with retry logic
    with_retry(function() {
      req <- httr2::request(pdf_url) |>
        httr2::req_headers(
          `User-Agent` = "Mozilla/5.0 (compatible; MWRA-Biobot-Monitor/1.0)"
        ) |>
        httr2::req_timeout(60)

      resp <- httr2::req_perform(req)

      if (httr2::resp_status(resp) != 200) {
        stop(sprintf("HTTP error: %d", httr2::resp_status(resp)))
      }

      # Write binary content to file (overwrites any existing)
      writeBin(httr2::resp_body_raw(resp), output_path)

      if (!file.exists(output_path) || file.size(output_path) == 0) {
        stop("Downloaded file is empty or missing")
      }

      TRUE
    })

    message("Downloaded PDF to: ", output_path)
    output_path

  }, error = function(e) {
    message("Download failed: ", e$message)
    NULL
  })
}
