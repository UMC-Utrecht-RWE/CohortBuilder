suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
})

# ---- fixtures ---------------------------------------------------------------

make_simple_cohort <- function() {
  data.table::data.table(
    person_id = c(1L, 1L, 2L, 2L, 3L, 4L),
    group = c(
      "EXPOSED",
      "EXPOSED",
      "CONTROL",
      "CONTROL",
      "UNMATCHED",
      "EXPOSED"
    ),
    match_id = c(1L, 1L, 1L, 1L, NA_integer_, 2L),
    T0 = as.Date(c(
      "2020-01-01",
      "2020-02-01",
      "2020-01-01",
      "2020-02-01",
      "2020-03-01",
      "2020-01-15"
    )),
    year_of_birth = c(1980L, 1980L, 1980L, 1980L, 1975L, 1985L),
    SV_SEX = c("M", "M", "M", "M", "F", "M"),
    SV_REGION = c("North", "North", "North", "North", "South", "North")
  )
}

make_cohort_with_missing <- function() {
  data.table::data.table(
    person_id = c(1L, 2L, 3L),
    group = c("EXPOSED", "CONTROL", "UNMATCHED"),
    match_id = c(NA_integer_, NA_integer_, NA_integer_),
    T0 = c(NA_character_, "2020-01-01", "2020-01-01")
  )
}

# ---- summary.matching_study_cohort tests -----------------------------------

test_that("summary works with valid matching_study_cohort data", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  result <- summary(cohort)

  expect_is(result, "list")
  expect_true("episodes_by_group" %in% names(result))
  expect_true("control_usage_distribution" %in% names(result))
})

test_that("summary converts non-data.table to data.table", {
  cohort <- as.data.frame(make_simple_cohort())
  class(cohort) <- c("matching_study_cohort", "data.frame")

  result <- summary(cohort)

  expect_is(result, "list")
  expect_true("episodes_by_group" %in% names(result))
})

test_that("summary fails with missing 'group' column", {
  cohort <- make_simple_cohort()[, -"group"]
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  expect_error(
    summary(cohort),
    "Missing required columns"
  )
})

test_that("summary fails with missing 'person_id' column", {
  cohort <- make_simple_cohort()[, -"person_id"]
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  expect_error(
    summary(cohort),
    "Missing required columns"
  )
})

test_that("summary calculates correct episodes by group", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  result <- summary(cohort)
  episodes <- result$episodes_by_group

  # Filter by group and count
  exposed_count <- nrow(cohort[group == "EXPOSED"])
  control_count <- nrow(cohort[group == "CONTROL"])
  unmatched_count <- nrow(cohort[group == "UNMATCHED"])

  expect_equal(episodes[group == "EXPOSED", N], exposed_count)
  expect_equal(episodes[group == "CONTROL", N], control_count)
  expect_equal(episodes[group == "UNMATCHED", N], unmatched_count)
})

test_that("summary calculates correct percentage distribution", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  result <- summary(cohort)
  episodes <- result$episodes_by_group

  # Check that percentages sum to 100
  total_pct <- sum(episodes$pct, na.rm = TRUE)
  expect_equal(total_pct, 100)
})

test_that("summary with no controls returns NULL for control_usage_distribution", {
  cohort <- make_simple_cohort()[group != "CONTROL"]
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  result <- summary(cohort)

  expect_null(result$control_usage_distribution)
})

test_that("summary calculates control usage distribution correctly", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  result <- summary(cohort)
  control_dist <- result$control_usage_distribution

  expect_is(control_dist, "data.table")
  expect_true("times_used" %in% names(control_dist))
  expect_true("n_controls" %in% names(control_dist))
  expect_true("pct_controls" %in% names(control_dist))
})

# ---- plot.matching_study_cohort tests ------------------------------------

test_that("plot works with valid matching_study_cohort data", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  p <- plot(cohort)
  expect_s3_class(p, "ggplot")
})

test_that("plot fails with missing 'group' column", {
  cohort <- make_simple_cohort()[, -"group"]
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  expect_error(
    plot(cohort),
    "Missing required columns"
  )
})

test_that("plot fails with missing by_var column", {
  cohort <- make_simple_cohort()[, -"T0"]
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  expect_error(
    plot(cohort, by_var = "T0"),
    "Missing required columns"
  )
})

test_that("plot accepts custom by_var parameter", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  # Plot by a categorical variable
  expect_silent(suppressMessages(withCallingHandlers(
    plot(cohort, by_var = "SV_REGION"),
    warning = function(w) {}
  )))
})

test_that("plot handles non-Date T0 column", {
  cohort <- make_simple_cohort()
  cohort$T0 <- as.character(cohort$T0)
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  # Should work because plot converts character to Date
  expect_silent(suppressMessages(withCallingHandlers(
    plot(cohort),
    warning = function(w) {}
  )))
})

test_that("plot fails with unconvertible T0 column", {
  cohort <- make_simple_cohort()
  cohort$T0 <- as.character(c(
    "invalid",
    "dates",
    "here",
    "nope",
    "bad",
    "dates"
  ))
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  expect_error(
    plot(cohort),
    "Could not convert"
  )
})

test_that("plot with missing y parameter works", {
  cohort <- make_simple_cohort()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  # y is missing by default, should still work
  expect_silent(suppressMessages(withCallingHandlers(
    plot(cohort),
    warning = function(w) {}
  )))
})

test_that("plot handles missing values in by_var", {
  cohort <- make_cohort_with_missing()
  class(cohort) <- c("matching_study_cohort", "data.table", "data.frame")

  # Should handle NAs gracefully
  expect_silent(suppressMessages(withCallingHandlers(
    plot(cohort, by_var = "T0"),
    warning = function(w) {}
  )))
})

# ---- internal function tests -----------------------------------------------

test_that(".cb_sensible_hist_bins returns integer", {
  dates <- as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
  bins <- CohortBuilder:::.cb_sensible_hist_bins(dates)

  expect_is(bins, "integer")
  expect_gt(bins, 0L)
  expect_lte(bins, 60L)
})

test_that(".cb_sensible_hist_bins handles single value", {
  dates <- as.Date("2020-01-01")
  bins <- CohortBuilder:::.cb_sensible_hist_bins(dates)

  expect_equal(bins, 1L)
})

test_that(".cb_sensible_hist_bins handles empty vector", {
  dates <- as.Date(character(0))
  bins <- CohortBuilder:::.cb_sensible_hist_bins(dates)

  expect_equal(bins, 1L)
})

test_that(".cb_sensible_hist_bins enforces bounds", {
  # Create many dates to generate large bin count
  dates <- as.Date(seq.Date(
    from = as.Date("2000-01-01"),
    to = as.Date("2025-12-31"),
    by = "day"
  ))
  bins <- CohortBuilder:::.cb_sensible_hist_bins(dates)

  expect_gte(bins, 8L)
  expect_lte(bins, 60L)
})

test_that(".plot_msc_hist_ggplot returns ggplot object if ggplot2 available", {
  skip_if_not_installed("ggplot2")

  cohort <- make_simple_cohort()
  cohort$T0 <- as.Date(cohort$T0)

  plot_obj <- CohortBuilder:::.plot_msc_hist_ggplot(
    cohort,
    by_var = "T0",
    n_bins = 10L
  )

  expect_is(plot_obj, "ggplot")
})

test_that(".plot_msc_stacked_ggplot returns ggplot object if ggplot2 available", {
  skip_if_not_installed("ggplot2")

  cohort <- make_simple_cohort()
  plot_data <- cohort[,
    .(n_matches = .N),
    by = .(plot_level = SV_REGION, group)
  ]

  plot_obj <- CohortBuilder:::.plot_msc_stacked_ggplot(
    plot_data,
    by_var = "SV_REGION"
  )

  expect_is(plot_obj, "ggplot")
})

test_that(".log_matching_summary logs without errors", {
  skip_if_not_installed("logger")

  cohort <- make_simple_cohort()

  expect_silent(
    CohortBuilder:::.log_matching_summary(cohort)
  )
})

test_that(".log_matching_summary handles missing required columns", {
  skip_if_not_installed("logger")

  cohort <- make_simple_cohort()[, -"group"]

  expect_silent(
    CohortBuilder:::.log_matching_summary(cohort)
  )
})
