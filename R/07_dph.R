# Mass. Department of Public Health COVID-19 dashboard data
#
# DPH publishes the workbook behind its COVID-19 dashboard once a week, on
# Thursdays, for weeks that run Sunday to Saturday. It holds reported cases,
# tests, and percent positivity by state, county, and city or town. It does NOT
# hold wastewater, emergency department, or hospital figures.
#
# Two things to know before trusting any number in it:
#   - Reported cases badly undercount infections, because home tests are never
#     reported. The direction means something; the level doesn't.
#   - Small counts are suppressed. A town's weekly case count is usually the
#     string "1-4", and rates built on a suppressed count are "*". Tests and
#     positive tests are published exactly, which is why the Framingham line is
#     built from those instead of from cases.
#
# mass.gov sits behind a bot wall that refuses plain automated requests, so the
# download goes through impersonate_fetch() like everything else. It can stop
# working without notice; the stale handling in run_indicators.R is what makes
# that tolerable.

DPH_DOWNLOAD_URL <-
  "https://www.mass.gov/doc/covid-19-reporting-dashboard-data/download"

# The page readers are sent to. Also the source credit in the summary.
DPH_PAGE_URL <- "https://www.mass.gov/info-details/covid-19-reporting"

DPH_COUNTY <- "Middlesex"
DPH_TOWN <- "Framingham"

DPH_CSV_PATH <- "data/processed/dph_covid.csv"

DPH_SHEETS <- c(
  state = "Weekly Cases and Deaths",
  county = "County Data",
  town = "City and Town Data"
)

#' Check that a downloaded file really is the DPH workbook
#'
#' A bot-wall page or an error page saved under an .xlsx name would otherwise
#' fail somewhere deep in the parser with a confusing message.
#'
#' @param path Path to the downloaded file
#' @return The path, invisibly; stops if the file isn't the workbook
validate_dph_workbook <- function(path) {
  # An .xlsx file is a zip archive, which always starts with "PK\003\004".
  magic <- readBin(path, "raw", n = 4)
  if (!identical(magic, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
    stop("DPH download is not an Excel workbook -- mass.gov may have served ",
         "an error or bot-check page instead.")
  }

  missing <- setdiff(DPH_SHEETS, readxl::excel_sheets(path))
  if (length(missing) > 0) {
    stop("DPH workbook is missing sheet(s): ", paste(missing, collapse = ", "),
         " -- its layout may have changed.")
  }

  invisible(path)
}

#' Download the DPH workbook
#'
#' @param dest Where to save it
#' @return Path to the validated workbook
fetch_dph_workbook <- function(dest = tempfile(fileext = ".xlsx")) {
  with_retry(function() {
    impersonate_fetch(DPH_DOWNLOAD_URL, output_path = dest)
    validate_dph_workbook(dest)
    dest
  }, max_attempts = 3)
}

#' Convert a DPH value to a number, treating suppressed values as missing
#'
#' @param x Character vector as read from the workbook
#' @return Numeric vector; "1-4", "*", and "**" all become NA
dph_number <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

#' Convert a DPH date cell to a Date
#'
#' Reading every column as text (needed to keep "1-4" intact) makes Excel date
#' cells arrive as serial day numbers. ISO strings are accepted too, in case
#' DPH ever stores the dates as text.
#'
#' @param x Character vector as read from the workbook
#' @return Date vector
dph_date <- function(x) {
  x <- as.character(x)
  serial <- suppressWarnings(as.numeric(x))
  out <- as.Date(serial, origin = "1899-12-30")

  iso <- is.na(serial) & !is.na(x)
  out[iso] <- as.Date(substr(x[iso], 1, 10), format = "%Y-%m-%d")
  out
}

#' Read one sheet as text and check it has the columns we rely on
#'
#' Columns are matched by name because their order differs between sheets.
#'
#' @param path Path to the workbook
#' @param sheet Sheet name
#' @param required Column names that must be present
#' @return Data frame of character columns
read_dph_sheet <- function(path, sheet, required) {
  data <- as.data.frame(
    readxl::read_excel(path, sheet = sheet, col_types = "text"),
    stringsAsFactors = FALSE
  )

  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf("DPH sheet '%s' is missing column(s): %s", sheet,
                 paste(missing, collapse = ", ")))
  }

  data
}

#' Shape the rows for one place into the published layout
#'
#' @param rows Rows of a county or town sheet for a single place
#' @param geography Label for the place
#' @param level "county" or "town"
#' @return Data frame in the dph_covid.csv layout
dph_place_series <- function(rows, geography, level) {
  data.frame(
    geography = geography,
    level = level,
    week_end = dph_date(rows[["Week End Date"]]),
    cases = dph_number(rows[["Cases during this week"]]),
    # The count exactly as DPH printed it, so "1-4" isn't lost to NA.
    cases_raw = as.character(rows[["Cases during this week"]]),
    tests = dph_number(rows[["Tests during the week"]]),
    positive_tests = dph_number(rows[["Positive tests during this week"]]),
    # DPH's own figure, as a fraction (0.07 is 7%).
    positivity = dph_number(rows[["Week percent positivity"]]),
    updated = dph_date(rows[["Last updated"]]),
    stringsAsFactors = FALSE
  )
}

#' Pull the statewide, county, and town series out of the workbook
#'
#' @param path Path to the workbook
#' @param county County to keep
#' @param town City or town to keep
#' @return Data frame sorted by level then week, one row per place per week
extract_dph_series <- function(path, county = DPH_COUNTY, town = DPH_TOWN) {
  place_columns <- c("Week End Date", "Last updated", "Cases during this week",
                     "Tests during the week", "Positive tests during this week",
                     "Week percent positivity")

  state_rows <- read_dph_sheet(
    path, DPH_SHEETS[["state"]],
    c("Week End Date", "Last updated", "Confirmed and probable cases")
  )
  county_rows <- read_dph_sheet(path, DPH_SHEETS[["county"]],
                                c("County", place_columns))
  town_rows <- read_dph_sheet(path, DPH_SHEETS[["town"]],
                              c("City/Town", place_columns))

  county_rows <- county_rows[which(county_rows[["County"]] == county), ,
                             drop = FALSE]
  town_rows <- town_rows[which(town_rows[["City/Town"]] == town), ,
                         drop = FALSE]

  if (nrow(county_rows) == 0) {
    stop(sprintf("No rows for %s County in the DPH workbook.", county))
  }
  if (nrow(town_rows) == 0) {
    stop(sprintf("No rows for %s in the DPH workbook.", town))
  }

  # The statewide sheet reports cases only; it has no testing columns.
  state <- data.frame(
    geography = "Massachusetts",
    level = "state",
    week_end = dph_date(state_rows[["Week End Date"]]),
    cases = dph_number(state_rows[["Confirmed and probable cases"]]),
    cases_raw = as.character(state_rows[["Confirmed and probable cases"]]),
    tests = NA_real_,
    positive_tests = NA_real_,
    positivity = NA_real_,
    updated = dph_date(state_rows[["Last updated"]]),
    stringsAsFactors = FALSE
  )

  result <- rbind(
    state,
    dph_place_series(county_rows, paste(county, "County"), "county"),
    dph_place_series(town_rows, town, "town")
  )

  result <- result[!is.na(result$week_end), ]
  result$level <- factor(result$level, levels = c("state", "county", "town"))
  result <- result[order(result$level, result$week_end), ]
  result$level <- as.character(result$level)
  rownames(result) <- NULL
  result
}

#' Write the DPH series to CSV
#'
#' @param data Data frame from extract_dph_series()
#' @param path Output path
#' @return The path, invisibly
save_dph_data <- function(data, path = DPH_CSV_PATH) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  readr::write_csv(data, path, na = "")
  invisible(path)
}

#' Read the stored DPH series back
#'
#' @param path CSV path
#' @return Data frame, or NULL when the file doesn't exist yet
read_dph_data <- function(path = DPH_CSV_PATH) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.data.frame(readr::read_csv(
    path,
    col_types = readr::cols(
      geography = readr::col_character(),
      level = readr::col_character(),
      week_end = readr::col_date(),
      cases = readr::col_double(),
      cases_raw = readr::col_character(),
      tests = readr::col_double(),
      positive_tests = readr::col_double(),
      positivity = readr::col_double(),
      updated = readr::col_date()
    )
  ))
}

#' Build the three DPH lines of the summary
#'
#' @param data Data frame from read_dph_data(), or NULL
#' @return List with `state`, `county`, and `town`, each an indicator_line() or
#'   NULL when that figure isn't available
dph_lines <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(state = NULL, county = NULL, town = NULL))
  }

  rows_for <- function(level) data[data$level == level, , drop = FALSE]

  # Statewide: reported cases against the week before.
  state <- indicator_latest_pair(rows_for("state"), "cases")
  state_line <- if (is.null(state)) NULL else indicator_line(
    paste("Reported cases:",
          indicator_with_change(state$current, state$previous, indicator_count)),
    state$week_end, "dph",
    list(measure = "state_cases", current = state$current,
         previous = state$previous),
    what = "reported cases"
  )

  # County: DPH's own percent positivity, shown as a percentage.
  county_rows <- rows_for("county")
  county_rows$positivity_pct <- county_rows$positivity * 100
  county <- indicator_latest_pair(county_rows, "positivity_pct")
  county_line <- if (is.null(county)) NULL else indicator_line(
    sprintf("%s: %s", county$row$geography,
            indicator_with_change(county$current, county$previous,
                                  indicator_pct1, " of tests positive")),
    county$week_end, "dph",
    list(measure = "county_positivity_pct", current = county$current,
         previous = county$previous),
    what = "county test results"
  )

  # Town: the two exact counts, so the small denominator stays visible. No
  # week-over-week comparison -- on this few tests it would mostly be noise.
  town <- indicator_latest_pair(rows_for("town"), "tests")
  town_usable <- !is.null(town) && town$current > 0 &&
    !is.na(town$row$positive_tests)
  town_line <- if (!town_usable) NULL else indicator_line(
    sprintf("%s: %s of %s tests positive (%s)", town$row$geography,
            indicator_count(town$row$positive_tests),
            indicator_count(town$current),
            indicator_pct1(100 * town$row$positive_tests / town$current)),
    town$week_end, "dph",
    list(measure = "town_tests", positive_tests = town$row$positive_tests,
         tests = town$current),
    what = "town test results"
  )

  list(state = state_line, county = county_line, town = town_line)
}
