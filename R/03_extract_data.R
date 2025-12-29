# Extract data from MWRA Biobot PDF

#' Extract data tables from MWRA Biobot PDF
#'
#' The PDF contains columns:
#' - Sample Date
#' - Southern (copies/mL)
#' - Northern (copies/mL)
#' - Southern 7 day avg
#' - Northern 7 day avg
#' - Southern Low Confidence Interval
#' - Southern High Confidence Interval
#' - Northern Low Confidence Interval
#' - Northern High Confidence Interval
#'
#' @param pdf_path Path to the downloaded PDF
#' @return List with data frames: north, south, combined
extract_pdf_data <- function(pdf_path) {
  # Extract text from PDF using pdftools
  text <- pdftools::pdf_text(pdf_path)

  # Combine all pages
  all_text <- paste(text, collapse = "\n")

  # Split into lines
  lines <- strsplit(all_text, "\n")[[1]]

  # Pattern to match data rows: date followed by numbers
  # Date format: M/D/YYYY or MM/DD/YYYY
  date_pattern <- "^\\s*(\\d{1,2}/\\d{1,2}/\\d{4})"

  # Filter lines that start with a date
  data_lines <- lines[grepl(date_pattern, lines)]

  # Parse each line
  parsed_data <- lapply(data_lines, parse_data_line)

  # Remove NULL entries (failed parses)
  parsed_data <- parsed_data[!sapply(parsed_data, is.null)]

  # Combine into data frame
  df <- do.call(rbind, lapply(parsed_data, as.data.frame))

  if (nrow(df) == 0) {
    stop("No data could be extracted from PDF")
  }

  # Convert date column
  df$date <- as.Date(df$date, format = "%m/%d/%Y")

  # Filter out rows with no actual data (future dates with empty values)
  df <- df[!is.na(df$south_copies) | !is.na(df$north_copies), ]

  # Sort by date

  df <- df[order(df$date), ]

  # Create separate data frames for each system
  north <- data.frame(
    date = df$date,
    copies_per_ml = df$north_copies,
    seven_day_avg = df$north_7day_avg,
    lower_ci = df$north_low_ci,
    upper_ci = df$north_high_ci,
    system = "North"
  )

  south <- data.frame(
    date = df$date,
    copies_per_ml = df$south_copies,
    seven_day_avg = df$south_7day_avg,
    lower_ci = df$south_low_ci,
    upper_ci = df$south_high_ci,
    system = "South"
  )

  # Remove rows where copies_per_ml is NA
  north <- north[!is.na(north$copies_per_ml), ]
  south <- south[!is.na(south$copies_per_ml), ]

  # Combined data
  combined <- rbind(north, south)
  combined <- combined[order(combined$date, combined$system), ]

  list(
    north = north,
    south = south,
    combined = combined,
    raw = df
  )
}

#' Parse a single data line from the PDF
#'
#' @param line Character string containing one row of data
#' @return Named list with parsed values, or NULL if parsing fails
parse_data_line <- function(line) {
  # Trim and normalize whitespace
  line <- trimws(line)

  # Split by whitespace
  parts <- strsplit(line, "\\s+")[[1]]

  # First element should be the date
  if (length(parts) < 1) return(NULL)

  date <- parts[1]

  # Validate date format

  if (!grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date)) return(NULL)

  # Remaining elements are numeric values
  # Expected order: south_copies, north_copies, south_7day, north_7day,
  #                 south_low_ci, south_high_ci, north_low_ci, north_high_ci
  values <- suppressWarnings(as.numeric(parts[-1]))

  # Pad with NAs if needed
  if (length(values) < 8) {
    values <- c(values, rep(NA, 8 - length(values)))
  }

  list(
    date = date,
    south_copies = values[1],
    north_copies = values[2],
    south_7day_avg = values[3],
    north_7day_avg = values[4],
    south_low_ci = values[5],
    south_high_ci = values[6],
    north_low_ci = values[7],
    north_high_ci = values[8]
  )
}

#' Save extracted data to CSV files
#'
#' @param data_list List containing north, south, and combined data frames
#' @param output_dir Directory to save CSV files
save_data <- function(data_list, output_dir = "data/processed") {
  # Create directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save individual system files
  readr::write_csv(data_list$north, file.path(output_dir, "north_system.csv"))
  message(sprintf("Saved %d rows to north_system.csv", nrow(data_list$north)))

  readr::write_csv(data_list$south, file.path(output_dir, "south_system.csv"))
  message(sprintf("Saved %d rows to south_system.csv", nrow(data_list$south)))

  # Combined file
  readr::write_csv(data_list$combined, file.path(output_dir, "combined_data.csv"))
  message(sprintf("Saved %d rows to combined_data.csv", nrow(data_list$combined)))

  invisible(data_list)
}

#' Main extraction function - extract and save in one call
#'
#' @param pdf_path Path to the PDF file
#' @param output_dir Directory to save CSV files
#' @return List of extracted data frames
extract_and_save <- function(pdf_path, output_dir = "data/processed") {
  message("Extracting data from PDF...")
  data <- extract_pdf_data(pdf_path)

  message("Saving data to CSV files...")
  save_data(data, output_dir)

  data
}
