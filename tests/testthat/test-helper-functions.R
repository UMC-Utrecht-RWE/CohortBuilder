suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
})

# ---- test fixtures ----------------------------------------------------------

make_eligible_pop <- function() {
  data.table::data.table(
    person_id = c(1L, 2L, 3L, 4L, 5L),
    eligible_exposed = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    eligible_control = c(FALSE, TRUE, FALSE, TRUE, FALSE),
    SV_SEX = c("M", "F", "M", "F", "M"),
    SV_REGION = c("North", "South", "North", "West", "East"),
    SV_PRIOR_COVID_DG = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    SV_PREG_STATUS = c(NA, TRUE, NA, FALSE, NA),
    SV_IMMUNOCOMPROMISED = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    CDC_RISK = c("Low", "Medium", "High", "Low", "Medium"),
    SV_SES_STATUS = c("High", "Low", "Medium", "High", "Low")
  )
}

make_eligible_pop_subset <- function() {
  # Only includes some matching variables
  data.table::data.table(
    person_id = c(1L, 2L, 3L, 4L, 5L),
    eligible_exposed = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    SV_SEX = c("M", "F", "M", "F", "M"),
    SV_REGION = c("North", "South", "North", "West", "East")
  )
}

# ---- get_matching_variables tests -------------------------------------------

test_that("get_matching_variables returns all variables when all present", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION", "SV_PRIOR_COVID_DG")

  result <- get_matching_variables(eligible_pop, matching_vars)

  expect_equal(result, matching_vars)
})

test_that("get_matching_variables uses default variables when not specified", {
  eligible_pop <- make_eligible_pop()

  result <- get_matching_variables(eligible_pop)

  # Result should be character vector
  expect_type(result, "character")
  expect_gt(length(result), 0)
})

test_that("get_matching_variables returns only available variables", {
  eligible_pop <- make_eligible_pop_subset()
  matching_vars <- c(
    "SV_SEX",
    "SV_REGION",
    "SV_PRIOR_COVID_DG",
    "NONEXISTENT_VAR"
  )

  result <- get_matching_variables(eligible_pop, matching_vars)

  # Should exclude NONEXISTENT_VAR and SV_PRIOR_COVID_DG
  expect_equal(result, c("SV_SEX", "SV_REGION"))
  expect_false("NONEXISTENT_VAR" %in% result)
  expect_false("SV_PRIOR_COVID_DG" %in% result)
})

test_that("get_matching_variables handles all missing variables", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("MISSING_VAR_1", "MISSING_VAR_2", "MISSING_VAR_3")

  result <- get_matching_variables(eligible_pop, matching_vars)

  expect_length(result, 0L)
  expect_equal(result, character(0))
})

test_that("get_matching_variables preserves variable order", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_REGION", "SV_SEX", "SV_PRIOR_COVID_DG")

  result <- get_matching_variables(eligible_pop, matching_vars)

  # Order should be preserved
  expect_equal(result, matching_vars)
})

test_that("get_matching_variables handles empty matching_vars", {
  eligible_pop <- make_eligible_pop()

  result <- get_matching_variables(eligible_pop, character(0))

  expect_length(result, 0L)
  expect_equal(result, character(0))
})

test_that("get_matching_variables handles data frame input", {
  eligible_pop <- as.data.frame(make_eligible_pop())
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_variables(eligible_pop, matching_vars)

  expect_equal(result, matching_vars)
})

test_that("get_matching_variables handles single variable", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- "SV_SEX"

  result <- get_matching_variables(eligible_pop, matching_vars)

  expect_equal(result, "SV_SEX")
  expect_length(result, 1L)
})

test_that("get_matching_variables handles duplicate variables in input", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION", "SV_SEX")

  result <- get_matching_variables(eligible_pop, matching_vars)

  # intersect removes duplicates
  expect_equal(result, c("SV_SEX", "SV_REGION"))
})

# ---- get_profile_table tests -----------------------------------------------

test_that("get_profile_table returns a data.table", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_s3_class(result, "data.table")
})

test_that("get_profile_table includes all matching variables", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  for (var in matching_vars) {
    expect_true(var %in% names(result))
  }
})

test_that("get_profile_table includes groupkey column", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_true("groupkey" %in% names(result))
})

test_that("get_profile_table creates unique combinations", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Check that groupkey values are unique
  expect_equal(data.table::uniqueN(result$groupkey), nrow(result))
})

test_that("get_profile_table groupkey starts from 1", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_equal(min(result$groupkey), 1L)
})

test_that("get_profile_table with single matching variable", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- "SV_SEX"

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_true("SV_SEX" %in% names(result))
  expect_true("groupkey" %in% names(result))
  expect_equal(ncol(result), 2L)
})

test_that("get_profile_table handles multiple matching variables", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION", "SV_PRIOR_COVID_DG", "CDC_RISK")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Number of columns = matching variables + groupkey
  expect_equal(ncol(result), length(matching_vars) + 1L)
})

test_that("get_profile_table removes duplicate combinations", {
  # Create population with duplicate combinations
  eligible_pop <- data.table::data.table(
    person_id = c(1L, 2L, 3L, 4L, 5L),
    SV_SEX = c("M", "F", "M", "F", "M"),
    SV_REGION = c("North", "South", "North", "South", "North")
  )
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Should have 4 unique combinations (M-North, F-South, M-North appears twice but counted once, F-South appears twice but counted once)
  expect_equal(nrow(result), 2L) # M-North and F-South
})

test_that("get_profile_table converts to data.table if needed", {
  eligible_pop <- as.data.frame(make_eligible_pop())
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_s3_class(result, "data.table")
})

test_that("get_profile_table saves to parquet when save_output = TRUE", {
  skip_if_not_installed("arrow")

  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")
  output_dir <- tempdir()
  output_file <- "test_profile_table"

  on.exit({
    unlink(file.path(output_dir, paste0(output_file, ".parquet")))
  })

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = TRUE,
    output_dir = output_dir,
    output_file = output_file
  )

  expect_true(file.exists(file.path(
    output_dir,
    paste0(output_file, ".parquet")
  )))
})

test_that("get_profile_table throws error if save_output = TRUE but output_dir = NULL", {
  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")

  expect_error(
    get_profile_table(
      eligible_pop = eligible_pop,
      matching_vars = matching_vars,
      save_output = TRUE,
      output_dir = NULL,
      output_file = "test"
    ),
    "Output directory must be specified"
  )
})

test_that("get_profile_table uses default output_file name", {
  skip_if_not_installed("arrow")

  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")
  output_dir <- tempdir()
  default_file <- "D3_LOOKUP_TABLE"

  on.exit({
    unlink(file.path(output_dir, paste0(default_file, ".parquet")))
  })

  result <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = TRUE,
    output_dir = output_dir
  )

  expect_true(file.exists(file.path(
    output_dir,
    paste0(default_file, ".parquet")
  )))
})

test_that("get_profile_table handles NULL matching_vars gracefully", {
  eligible_pop <- make_eligible_pop()

  # This should handle NULL matching_vars (will likely error or return empty)
  expect_error(
    get_profile_table(
      eligible_pop = eligible_pop,
      matching_vars = NULL,
      save_output = FALSE
    )
  )
})

test_that("get_profile_table output can be read back from parquet", {
  skip_if_not_installed("arrow")

  eligible_pop <- make_eligible_pop()
  matching_vars <- c("SV_SEX", "SV_REGION")
  output_dir <- tempdir()
  output_file <- "test_profile_read"

  on.exit({
    unlink(file.path(output_dir, paste0(output_file, ".parquet")))
  })

  original <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = TRUE,
    output_dir = output_dir,
    output_file = output_file
  )

  # Read back from file
  read_back <- arrow::read_parquet(file.path(
    output_dir,
    paste0(output_file, ".parquet")
  ))

  # Convert to data.table for comparison
  read_back_dt <- as.data.table(read_back)

  # Should have same dimensions and content
  expect_equal(ncol(original), ncol(read_back_dt))
  expect_equal(nrow(original), nrow(read_back_dt))
})
