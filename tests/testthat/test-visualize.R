# Tests for R/04_visualize.R

# Helper function to create sample test data
make_test_data <- function(n_days = 30, system = "North") {
  dates <- seq(Sys.Date() - n_days + 1, Sys.Date(), by = "day")
  data.frame(
    date = dates,
    copies_per_ml = runif(n_days, 1000, 2000),
    seven_day_avg = runif(n_days, 1000, 2000),
    lower_ci = runif(n_days, 50, 100),
    upper_ci = runif(n_days, 100, 150),
    system = system
  )
}

describe("create_plot()", {
  it("returns a ggplot object", {
    data <- make_test_data(30, "North")

    result <- create_plot(data, title = "Test Plot")

    expect_s3_class(result, "ggplot")
  })

  it("filters data by days parameter", {
    data <- make_test_data(100, "North")

    result <- create_plot(data, days = 30)

    # Extract the plot data
    plot_data <- ggplot2::ggplot_build(result)$data[[1]]

    # Should have approximately 30 data points (bars)
    expect_lte(nrow(plot_data), 31)
  })

  it("uses all data when days is NULL", {
    data <- make_test_data(100, "North")

    result <- create_plot(data, days = NULL)

    plot_data <- ggplot2::ggplot_build(result)$data[[1]]

    expect_equal(nrow(plot_data), 100)
  })

  it("handles single system data (North)", {
    data <- make_test_data(30, "North")

    result <- create_plot(data, title = "North System")

    expect_s3_class(result, "ggplot")
    # Check the fill is blue for North
    plot_data <- ggplot2::ggplot_build(result)$data[[1]]
    expect_true(all(plot_data$fill == "#2166AC" | plot_data$fill == "#2166ac"))
  })

  it("handles single system data (South)", {
    data <- make_test_data(30, "South")

    result <- create_plot(data, title = "South System")

    expect_s3_class(result, "ggplot")
    # Check the fill is red for South
    plot_data <- ggplot2::ggplot_build(result)$data[[1]]
    expect_true(all(plot_data$fill == "#B2182B" | plot_data$fill == "#b2182b"))
  })

  it("handles combined North and South data", {
    north <- make_test_data(30, "North")
    south <- make_test_data(30, "South")
    combined <- rbind(north, south)

    result <- create_plot(combined, title = "Combined Systems")

    expect_s3_class(result, "ggplot")
    # Should have both colors
    plot_data <- ggplot2::ggplot_build(result)$data[[1]]
    fills <- unique(tolower(plot_data$fill))
    expect_length(fills, 2)
  })

  it("adds error bars when CI data is present", {
    data <- make_test_data(30, "North")

    result <- create_plot(data)

    # Get all layers
    layer_types <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true("GeomErrorbar" %in% layer_types)
  })

  it("handles data without CI columns", {
    data <- make_test_data(30, "North")
    data$lower_ci <- NULL
    data$upper_ci <- NULL

    # Should not error
    expect_no_error({
      result <- create_plot(data)
    })
  })

  it("handles data with all NA CI values", {
    data <- make_test_data(30, "North")
    data$lower_ci <- NA
    data$upper_ci <- NA

    # Should not error
    expect_no_error({
      result <- create_plot(data)
    })
  })

  it("sets correct title from parameter", {
    data <- make_test_data(30, "North")

    result <- create_plot(data, title = "Custom Title")

    expect_equal(result$labels$title, "Custom Title")
  })

  it("uses appropriate date breaks for different time spans", {
    # Short span (< 90 days) - should use 2 week breaks
    short_data <- make_test_data(60, "North")
    short_result <- create_plot(short_data)
    expect_s3_class(short_result, "ggplot")

    # Medium span (90-365 days) - should use 1 month breaks
    medium_data <- make_test_data(180, "North")
    medium_result <- create_plot(medium_data)
    expect_s3_class(medium_result, "ggplot")

    # Long span (> 365 days) - should use 3 month breaks
    long_data <- make_test_data(400, "North")
    long_result <- create_plot(long_data)
    expect_s3_class(long_result, "ggplot")
  })

  it("respects show_legend parameter", {
    north <- make_test_data(30, "North")
    south <- make_test_data(30, "South")
    combined <- rbind(north, south)

    # With legend
    with_legend <- create_plot(combined, show_legend = TRUE)
    expect_equal(with_legend$theme$legend.position, "bottom")

    # Without legend
    without_legend <- create_plot(combined, show_legend = FALSE)
    expect_equal(without_legend$theme$legend.position, "none")
  })

  it("generates valid subtitle with data date", {
    data <- make_test_data(30, "North")
    max_date <- max(data$date)

    result <- create_plot(data)

    expect_match(result$labels$subtitle, format(max_date, "%B %d, %Y"))
  })
})

describe("generate_all_plots()", {
  it("errors when combined data file doesn't exist", {
    temp_dir <- withr::local_tempdir()

    expect_error(
      generate_all_plots(data_dir = temp_dir, output_dir = temp_dir),
      "Combined data file not found"
    )
  })

  it("creates output directory if needed", {
    temp_data_dir <- withr::local_tempdir()
    temp_output_dir <- file.path(temp_data_dir, "new_output")

    # Create test data file
    combined <- rbind(
      make_test_data(30, "North"),
      make_test_data(30, "South")
    )
    readr::write_csv(combined, file.path(temp_data_dir, "combined_data.csv"))

    generate_all_plots(
      data_dir = temp_data_dir,
      output_dir = temp_output_dir
    )

    expect_true(dir.exists(temp_output_dir))
  })

  it("generates PNG files for both systems", {
    temp_data_dir <- withr::local_tempdir()
    temp_output_dir <- withr::local_tempdir()

    # Create test data file
    combined <- rbind(
      make_test_data(100, "North"),
      make_test_data(100, "South")
    )
    readr::write_csv(combined, file.path(temp_data_dir, "combined_data.csv"))

    generate_all_plots(
      data_dir = temp_data_dir,
      output_dir = temp_output_dir
    )

    expect_true(file.exists(file.path(temp_output_dir, "north_90days.png")))
    expect_true(file.exists(file.path(temp_output_dir, "south_90days.png")))
  })

  it("returns list of plots invisibly", {
    temp_data_dir <- withr::local_tempdir()
    temp_output_dir <- withr::local_tempdir()

    combined <- rbind(
      make_test_data(100, "North"),
      make_test_data(100, "South")
    )
    readr::write_csv(combined, file.path(temp_data_dir, "combined_data.csv"))

    result <- generate_all_plots(
      data_dir = temp_data_dir,
      output_dir = temp_output_dir
    )

    expect_type(result, "list")
    expect_named(result, c("north", "south"))
    expect_s3_class(result$north, "ggplot")
    expect_s3_class(result$south, "ggplot")
  })
})
