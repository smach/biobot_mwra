# Other Covid indicators: Mass. DPH case data and CDC hospital data
#
# A third pipeline alongside Biobot (retired) and WastewaterSCAN (live). It
# produces one small JSON file of ready-made sentences that the email script on
# Sharon's server prints under the wastewater chart. Nothing here touches the
# wastewater series, and these numbers must never be merged with it.
#
# This file holds what the two sources share: date and number formatting, the
# "up from / down from" wording, the staleness rule, and the assembly of the
# summary. R/07_dph.R and R/08_cdc.R each know how to fetch one source and how
# to phrase its lines.
#
# Every sentence is written here, in the repo, so the wording exists in exactly
# one place. The server prints `heading`, each section's `heading`, `lines`,
# and `source_note`, and the `caveat`, and decides nothing.

INDICATORS_SUMMARY_PATH <- "data/processed/indicators_summary.json"

# A source whose newest week is older than this is left out of the summary
# rather than presented as current. Both sources publish weekly, so three weeks
# means two missed updates in a row.
INDICATORS_MAX_AGE_DAYS <- 21

# AP style: March through July are spelled out, the rest abbreviated.
INDICATOR_MONTHS <- c("Jan.", "Feb.", "March", "April", "May", "June", "July",
                      "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")

#' Format a date the way the email refers to a week ("Nov. 7")
#'
#' @param date A Date
#' @return Character scalar
indicator_date_label <- function(date) {
  date <- as.Date(date)
  paste(INDICATOR_MONTHS[as.integer(format(date, "%m"))],
        as.integer(format(date, "%d")))
}

#' Format a whole number with thousands separators ("1,123")
indicator_count <- function(x) {
  format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Format a percentage to one decimal place ("7.0%")
indicator_pct1 <- function(x) sprintf("%.1f%%", x)

#' Format a percentage to two decimal places ("0.55%")
indicator_pct2 <- function(x) sprintf("%.2f%%", x)

#' Describe a value and how it compares with the week before
#'
#' The comparison is made on the formatted strings, not the raw numbers, for
#' the same reason the WastewaterSCAN headline compares rounded percentages:
#' "7.0%, up from 7.0%" is what an unrounded comparison would print.
#'
#' @param current This week's value
#' @param previous Last week's value, or NA when there isn't one
#' @param format_value Function turning a value into the text shown
#' @param label Text placed straight after the current value, e.g.
#'   " of tests positive"
#' @return Character scalar such as "1,123, up from 978"
indicator_with_change <- function(current, previous, format_value, label = "") {
  shown <- paste0(format_value(current), label)

  if (length(previous) != 1 || is.na(previous)) {
    return(shown)
  }

  shown_previous <- format_value(previous)
  if (identical(format_value(current), shown_previous)) {
    return(paste0(shown, ", unchanged from the week before"))
  }

  paste0(shown, if (current > previous) ", up from " else ", down from ",
         shown_previous)
}

#' The newest usable week in a series, and the week before it
#'
#' "The week before" means exactly seven days earlier. If that week is missing
#' the previous value is NA, so a gap in the data can't turn into a comparison
#' against some older week presented as last week.
#'
#' @param data Data frame with a `week_end` Date column
#' @param column Name of the value column
#' @return List with `week_end`, `current`, `previous`, and the full `row`, or
#'   NULL when the series has no usable value
indicator_latest_pair <- function(data, column) {
  if (is.null(data) || nrow(data) == 0 || !column %in% names(data)) {
    return(NULL)
  }

  usable <- data[!is.na(data$week_end) & !is.na(data[[column]]), , drop = FALSE]
  if (nrow(usable) == 0) {
    return(NULL)
  }

  latest <- usable[which.max(usable$week_end), , drop = FALSE]
  before <- usable[usable$week_end == latest$week_end - 7, , drop = FALSE]

  list(
    week_end = latest$week_end,
    current = latest[[column]],
    previous = if (nrow(before) == 1) before[[column]] else NA_real_,
    row = latest
  )
}

#' One line of the summary
#'
#' @param text The sentence fragment to print
#' @param week_end Week the figure covers
#' @param source Source key: "dph", "cdc_ed", or "cdc_hosp"
#' @param values Named list of the raw numbers behind the text, for auditing
#' @param what Short name for the figure, used when a section has to say which
#'   figure came from which source ("reported cases from ...")
#' @return List
indicator_line <- function(text, week_end, source, values = list(),
                           what = "figures") {
  list(text = text, week_end = as.Date(week_end), source = source,
       values = values, what = what)
}

#' Is this line recent enough to present as current?
indicator_is_fresh <- function(line, today = Sys.Date(),
                               max_age = INDICATORS_MAX_AGE_DAYS) {
  !is.null(line) && as.numeric(as.Date(today) - line$week_end) <= max_age
}

#' Turn a group of lines into a titled section
#'
#' When every line covers the same week the date goes in the heading once.
#' When they don't -- DPH posts on Thursdays and CDC on Fridays, so for a day
#' each week DPH is a week ahead -- each line carries its own date instead, so
#' no figure is ever shown under another figure's week.
#'
#' Say where a section's figures come from, as one Markdown sentence
#'
#' A section drawn from a single source gets "Source: <link>." One that mixes
#' sources names each figure with its own, so nobody has to guess which number
#' is the state's and which is CDC's.
#'
#' @param lines List of indicator_line() results
#' @return Character scalar of Markdown
indicator_source_note <- function(lines) {
  credits <- indicator_source_credits()
  link <- function(source) {
    sprintf("[%s](%s)", credits[[source]]$name, credits[[source]]$url)
  }

  sources <- vapply(lines, function(l) l$source, character(1))

  if (length(unique(sources)) == 1) {
    return(sprintf("Source: %s.", link(sources[1])))
  }

  parts <- vapply(lines, function(l) {
    sprintf("%s from the %s", l$what, link(l$source))
  }, character(1))

  paste0("Sources: ", paste(parts, collapse = "; "), ".")
}

#' @param title Section title, e.g. "Massachusetts"
#' @param lines List of indicator_line() results (NULLs already removed)
#' @return List with `heading`, `lines`, and `source_note`, or NULL when there
#'   are no lines
indicator_section <- function(title, lines) {
  if (length(lines) == 0) {
    return(NULL)
  }

  weeks <- as.Date(vapply(lines, function(l) format(l$week_end), character(1)))
  texts <- vapply(lines, function(l) l$text, character(1))

  if (length(unique(weeks)) == 1) {
    heading <- sprintf("%s, week ending %s:", title,
                       indicator_date_label(weeks[1]))
  } else {
    heading <- paste0(title, ":")
    texts <- sprintf("%s (week ending %s)", texts,
                     vapply(weeks, indicator_date_label, character(1)))
  }

  # as.list() so a single line still serializes as a JSON array, not a string.
  list(heading = heading, lines = as.list(unname(texts)),
       source_note = indicator_source_note(lines))
}

# Who to credit, keyed the same way as the lines' `source` field.
indicator_source_credits <- function() {
  list(
    dph = list(name = "Mass. Department of Public Health", url = DPH_PAGE_URL),
    # Both are CDC to a reader; the links go to the two different datasets.
    cdc_ed = list(name = "CDC", url = CDC_ED_PAGE_URL),
    cdc_hosp = list(name = "CDC", url = CDC_HOSP_PAGE_URL)
  )
}

# Plain names for the log and the stale-data issue.
INDICATOR_SOURCE_NAMES <- c(
  dph = "Mass. DPH cases and testing",
  cdc_ed = "CDC emergency department visits",
  cdc_hosp = "CDC hospital admissions"
)

#' Build the summary the email prints
#'
#' Each argument is that source's stored series, or NULL when there isn't one.
#' Sources are independent: a missing or stale one loses only its own lines.
#'
#' @param dph Data frame from read_dph_data()
#' @param cdc_ed Data frame from read_cdc_ed_data()
#' @param cdc_hosp Data frame from read_cdc_hosp_data()
#' @param today Date to measure staleness against
#' @return List: `payload` (what gets written as JSON) and `unavailable` (names
#'   of sources that contributed nothing current)
indicators_summary <- function(dph = NULL, cdc_ed = NULL, cdc_hosp = NULL,
                               today = Sys.Date()) {
  dph_parts <- dph_lines(dph)

  candidates <- list(
    state = list(dph_parts$state, cdc_ed_line(cdc_ed), cdc_hosp_line(cdc_hosp)),
    local = list(dph_parts$county, dph_parts$town)
  )

  # Drop anything missing or too old to call current.
  fresh <- lapply(candidates, function(lines) {
    Filter(function(l) indicator_is_fresh(l, today), lines)
  })
  all_lines <- c(fresh$state, fresh$local)

  sections <- Filter(Negate(is.null), list(
    indicator_section("Massachusetts", fresh$state),
    indicator_section("Closer to home", fresh$local)
  ))

  used <- unique(vapply(all_lines, function(l) l$source, character(1)))
  credits <- indicator_source_credits()
  unavailable <- setdiff(names(INDICATOR_SOURCE_NAMES), used)

  # The undercount caveat is about reported cases, so it only appears when a
  # DPH line does.
  caveat <- if ("dph" %in% used) {
    paste("Reported cases undercount actual numbers, since home tests aren't",
          "reported to the state.")
  } else {
    NULL
  }

  weeks <- vapply(all_lines, function(l) format(l$week_end), character(1))

  payload <- list(
    heading = "Other Covid indicators",
    sections = sections,
    caveat = caveat,
    sources = unname(credits[intersect(names(credits), used)]),
    newest_week_end = if (length(weeks) > 0) max(weeks) else NULL,
    # The numbers behind each sentence, so a surprising line can be checked
    # against the source without re-running anything.
    values = lapply(all_lines, function(l) {
      c(list(source = l$source, week_end = format(l$week_end)), l$values)
    })
  )

  list(
    payload = Filter(Negate(is.null), payload),
    unavailable = unname(INDICATOR_SOURCE_NAMES[unavailable])
  )
}

#' Serialize a summary payload the same way every time
indicators_json <- function(payload) {
  # na = "null" because jsonlite would otherwise write a missing number as the
  # string "NA".
  jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, digits = NA,
                   null = "null", na = "null")
}

#' Write the summary, but only when its content has changed
#'
#' The `generated` timestamp would otherwise differ on every run and produce a
#' commit a day with nothing in it. So the existing file is compared with the
#' new one minus that field, and left alone when they match.
#'
#' @param payload `payload` element from indicators_summary()
#' @param path Output path
#' @return TRUE if the file was written, FALSE if it was already current
save_indicators_summary <- function(payload, path = INDICATORS_SUMMARY_PATH) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  if (file.exists(path)) {
    existing <- tryCatch(
      jsonlite::read_json(path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    existing$generated <- NULL
    if (!is.null(existing) &&
        identical(as.character(indicators_json(existing)),
                  as.character(indicators_json(payload)))) {
      return(FALSE)
    }
  }

  payload$generated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  writeLines(indicators_json(payload), path, useBytes = TRUE)
  TRUE
}
