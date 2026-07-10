# Download PDF from MWRA website

#' Download PDF from MWRA website
#'
#' Downloads the Biobot data PDF, keeping only the most recent one. The
#' payload is validated: Imperva sometimes challenges the PDF URL itself, in
#' which case the "download" is a small HTML interstitial rather than a PDF.
#' That case is classified as a transient challenge (like in
#' `check_for_updates()`) instead of a genuine failure, and the bogus file is
#' removed so it can never be committed as data.
#'
#' @param pdf_url Full URL to the PDF file
#' @param output_path Where to save the PDF (overwrites previous)
#' @return List with:
#'   - path: path to the downloaded file, or NULL if download failed
#'   - status: "ok", "challenge", or "error"
download_pdf <- function(pdf_url, output_path = "data/latest_data.pdf") {
  # Reuse one cookie jar across retries so any Imperva cookie is replayed.
  cookie_jar <- tempfile(fileext = ".cookies")
  on.exit(unlink(cookie_jar), add = TRUE)

  tryCatch({
    # Ensure directory exists
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

    # Download with retry logic (curl-impersonate writes directly to disk
    # so binary PDFs are preserved without text-mode mangling).
    with_retry(function() {
      impersonate_fetch(pdf_url, output_path = output_path, timeout = 60,
                        cookie_jar = cookie_jar)

      if (!file.exists(output_path) || file.size(output_path) == 0) {
        stop("Downloaded file is empty or missing")
      }

      # Validate the payload really is a PDF before accepting it.
      magic <- rawToChar(readBin(output_path, "raw", n = 5L))
      if (!identical(magic, "%PDF-")) {
        body <- paste(readLines(output_path, warn = FALSE, encoding = "UTF-8"),
                      collapse = "\n")
        unlink(output_path)
        if (is_bot_challenge(body)) {
          stop(BOT_CHALLENGE_TAG)
        }
        stop(sprintf("Downloaded file is not a PDF. Content starts with: %s",
                     substr(body, 1, 200)))
      }

      TRUE
    })

    message("Downloaded PDF to: ", output_path)
    list(path = output_path, status = "ok")

  }, error = function(e) {
    is_challenge <- grepl(BOT_CHALLENGE_TAG, e$message, fixed = TRUE)
    if (is_challenge) {
      message("MWRA served a bot-challenge page for the PDF on every attempt.")
    } else {
      message("Download failed: ", e$message)
    }
    list(path = NULL, status = if (is_challenge) "challenge" else "error")
  })
}
