# Generate visualizations for MWRA Biobot data

#' Create a plot with confidence intervals
#'
#' @param data Data frame with columns: date, copies_per_ml, lower_ci, upper_ci, system
#' @param days Number of days to include (NULL for all data)
#' @param title Plot title
#' @param show_legend Whether to show the legend
#' @return ggplot object
create_plot <- function(data, days = NULL, title = "COVID-19 Wastewater Data",
                        show_legend = TRUE) {
  # Filter to time period if specified
  if (!is.null(days)) {
    cutoff_date <- max(data$date, na.rm = TRUE) - days
    data <- data[data$date >= cutoff_date, ]
  }

  # Determine date breaks based on time span
  date_range <- as.numeric(diff(range(data$date, na.rm = TRUE)))
  if (date_range > 365) {
    date_breaks <- "3 months"
    date_format <- "%b %Y"
  } else if (date_range > 90) {
    date_breaks <- "1 month"
    date_format <- "%b %d"
  } else {
    date_breaks <- "2 weeks"
    date_format <- "%b %d"
  }

  # Check if we have multiple systems
  systems_present <- unique(data$system)
  n_systems <- length(systems_present)

  # Build the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = copies_per_ml))

  # Check if we have CI data
  has_ci <- all(c("lower_ci", "upper_ci") %in% names(data)) &&
            any(!is.na(data$lower_ci)) && any(!is.na(data$upper_ci))

  # Add bars and error bars
  if (n_systems > 1) {
    p <- p +
      ggplot2::geom_col(ggplot2::aes(fill = system), position = "dodge", width = 0.8)

    # Add error bars if CI data exists
    # CI values are deltas, so calculate actual bounds
    if (has_ci) {
      ci_data <- data[!is.na(data$lower_ci) & !is.na(data$upper_ci), ]
      if (nrow(ci_data) > 0) {
        p <- p + ggplot2::geom_errorbar(
          data = ci_data,
          ggplot2::aes(
            ymin = copies_per_ml - lower_ci,
            ymax = copies_per_ml + upper_ci,
            group = system
          ),
          position = ggplot2::position_dodge(width = 0.8),
          width = 0.3, linewidth = 0.4
        )
      }
    }

    p <- p +
      ggplot2::scale_fill_manual(
        values = c("North" = "#1a5d1a", "South" = "#2166ac"),
        name = "System"
      )
  } else {
    system_color <- if (systems_present == "North") "#1a5d1a" else "#2166ac"
    p <- p +
      ggplot2::geom_col(fill = system_color, width = 0.8)

    # Add error bars if CI data exists
    # CI values are deltas, so calculate actual bounds
    if (has_ci) {
      ci_data <- data[!is.na(data$lower_ci) & !is.na(data$upper_ci), ]
      if (nrow(ci_data) > 0) {
        p <- p + ggplot2::geom_errorbar(
          data = ci_data,
          ggplot2::aes(
            ymin = copies_per_ml - lower_ci,
            ymax = copies_per_ml + upper_ci
          ),
          width = 0.3, linewidth = 0.4
        )
      }
    }
  }

  # Axis formatting
  p <- p +
    ggplot2::scale_x_date(
      date_labels = date_format,
      date_breaks = date_breaks,
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0.05, 0.1))
    )

  # Labels and theme
  subtitle_text <- sprintf("Data through %s", format(max(data$date, na.rm = TRUE), "%B %d, %Y"))
  caption_text <- "Source: MWRA Biobot Data | Shaded regions show 95% confidence intervals"

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle_text,
      x = NULL,
      y = "Viral Copies per mL",
      caption = caption_text
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray40", size = 10),
      plot.caption = ggplot2::element_text(color = "gray50", size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = if (show_legend && n_systems > 1) "bottom" else "none"
    )

  p
}

#' Generate plots for North and South systems
#'
#' Creates 90-day plots for North and South systems
#'
#' @param data_dir Directory containing CSV data files
#' @param output_dir Directory to save plot PNG files
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Plot resolution
generate_all_plots <- function(data_dir = "data/processed",
                               output_dir = "output/plots",
                               width = 10, height = 6, dpi = 150) {
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load data
  combined_file <- file.path(data_dir, "combined_data.csv")
  if (!file.exists(combined_file)) {
    stop("Combined data file not found: ", combined_file)
  }

  combined <- readr::read_csv(combined_file, show_col_types = FALSE)
  combined$date <- as.Date(combined$date)

  north <- combined[combined$system == "North", ]
  south <- combined[combined$system == "South", ]

  # Generate 90-day plots
  message("Generating 90-day plots...")

  p_north <- create_plot(north, days = 90, title = "North System: Last 90 Days")
  ggplot2::ggsave(file.path(output_dir, "north_90days.png"), p_north,
                  width = width, height = height, dpi = dpi)

  p_south <- create_plot(south, days = 90, title = "South System: Last 90 Days")
  ggplot2::ggsave(file.path(output_dir, "south_90days.png"), p_south,
                  width = width, height = height, dpi = dpi)

  message(sprintf("Generated 2 plots in %s", output_dir))

  invisible(list(north = p_north, south = p_south))
}
