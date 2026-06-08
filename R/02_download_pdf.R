# Download PDF from MWRA website

#' Download PDF from MWRA website
#'
#' Downloads the Biobot data PDF, keeping only the most recent one.
#'
#' @param pdf_url Full URL to the PDF file
#' @param output_path Where to save the PDF (overwrites previous)
#' @return Path to the downloaded file, or NULL if download failed
download_pdf <- function(pdf_url, output_path = "data/latest_data.pdf",
                         cookie_header = NULL) {
  tryCatch({
    # Ensure directory exists
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

    # Download with retry logic (curl-impersonate writes directly to disk
    # so binary PDFs are preserved without text-mode mangling). If the
    # caller already cleared an Imperva challenge for this host, reusing
    # its cookies lets the PDF download go through the same session.
    with_retry(function() {
      impersonate_fetch(pdf_url, output_path = output_path, timeout = 60,
                        cookie_header = cookie_header)

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
