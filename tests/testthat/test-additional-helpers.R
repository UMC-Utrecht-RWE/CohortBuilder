suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
})

# ---- test fixtures ----------------------------------------------------------

make_eligible_pop <- function() {
  data.table::data.table(
    person_id = c(1L, 2L, 3L, 4L, 5L, 6L),
    eligible_exposed = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    eligible_control = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    matching_status_start = as.Date(c(
      "2020-01-01",
      "2020-01-01",
      "2020-01-01",
      "2020-01-01",
      "2020-01-01",
      "2020-01-01"
    )),
    matching_status_end = as.Date(c(
      "2020-12-31",
      "2020-12-31",
      "2020-12-31",
      "2020-12-31",
      "2020-12-31",
      "2020-12-31"
    )),
    year_of_birth = c(1980L, 1985L, 1975L, 1990L, 1980L, 1985L),
    SV_SEX = c("M", "F", "M", "F", "M", "F"),
    SV_REGION = c("North", "South", "North", "South", "North", "South")
  )
}

make_profile_table <- function() {
  data.table::data.table(
    SV_SEX = c("M", "F"),
    SV_REGION = c("North", "South"),
    groupkey = c(1L, 2L)
  )
}

# ---- get_matching_population tests ------------------------------------------

test_that("get_matching_population returns a data.table", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_s3_class(result, "data.table")
})

test_that("get_matching_population includes required columns", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_true("person_id" %in% names(result))
  expect_true("group" %in% names(result))
  expect_true("groupkey" %in% names(result))
  expect_true("year_of_birth" %in% names(result))
})

test_that("get_matching_population assigns correct group labels", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Check that groups are either "exposed" or "control"
  expect_true(all(result$group %in% c("exposed", "control")))
})

test_that("get_matching_population assigns exposed to exposed individuals", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Check that people with eligible_exposed = TRUE are marked as "exposed"
  exposed_people <- eligible_pop[eligible_exposed == TRUE, person_id]
  result_exposed <- result[group == "exposed", person_id]

  expect_true(all(exposed_people %in% result_exposed))
})

test_that("get_matching_population assigns control to control individuals", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Check that people with eligible_control = TRUE are marked as "control"
  control_people <- eligible_pop[eligible_control == TRUE, person_id]
  result_control <- result[group == "control", person_id]

  expect_true(all(control_people %in% result_control))
})

test_that("get_matching_population preserves groupkey from profile_table", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # All groupkey values should be present
  expect_true(all(result$groupkey %in% profile_table$groupkey))
})

test_that("get_matching_population uses custom column names", {
  eligible_pop <- make_eligible_pop()
  data.table::setnames(eligible_pop, "person_id", "pid")
  data.table::setnames(eligible_pop, "year_of_birth", "yob")

  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE,
    col_person_id = "pid",
    col_age_iterator = "yob"
  )

  expect_true("pid" %in% names(result))
  expect_true("yob" %in% names(result))
})

test_that("get_matching_population saves to parquet when save_output = TRUE", {
  skip_if_not_installed("arrow")

  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")
  output_dir <- tempdir()
  output_file <- "test_matching_pop"

  on.exit({
    unlink(file.path(output_dir, paste0(output_file, ".parquet")))
  })

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
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

test_that("get_matching_population converts data frame inputs to data.table", {
  eligible_pop <- as.data.frame(make_eligible_pop())
  profile_table <- as.data.frame(make_profile_table())
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_s3_class(result, "data.table")
})

test_that("get_matching_population handles multiple matching variables", {
  eligible_pop <- make_eligible_pop()
  profile_table <- data.table::data.table(
    SV_SEX = c("M", "F"),
    SV_REGION = c("North", "South"),
    groupkey = c(1L, 2L)
  )
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  expect_gt(nrow(result), 0L)
})

test_that("get_matching_population returns correct output structure", {
  eligible_pop <- make_eligible_pop()
  profile_table <- make_profile_table()
  matching_vars <- c("SV_SEX", "SV_REGION")

  result <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = profile_table,
    matching_vars = matching_vars,
    save_output = FALSE
  )

  # Should have person_id, start, end, year_of_birth, group, groupkey columns
  expected_cols <- c(
    "person_id",
    "matching_status_start",
    "matching_status_end",
    "year_of_birth",
    "group",
    "groupkey"
  )
  for (col in expected_cols) {
    expect_true(
      col %in% names(result),
      label = paste("Column", col, "should be present")
    )
  }
})

# ---- .ensure_directory tests ------------------------------------------------

test_that(".ensure_directory creates directory if it doesn't exist", {
  test_dir <- file.path(tempdir(), "test_new_dir")
  on.exit({
    unlink(test_dir, recursive = TRUE)
  })

  expect_false(dir.exists(test_dir))

  CohortBuilder:::.ensure_directory(test_dir, label = "test")

  expect_true(dir.exists(test_dir))
})

test_that(".ensure_directory returns invisible path", {
  test_dir <- file.path(tempdir(), "test_ensure_dir")
  on.exit({
    unlink(test_dir, recursive = TRUE)
  })

  result <- CohortBuilder:::.ensure_directory(test_dir)

  # The function returns invisible(path), not NULL
  expect_equal(result, test_dir)
})

test_that(".ensure_directory doesn't error if directory already exists", {
  test_dir <- tempdir()

  expect_silent(
    CohortBuilder:::.ensure_directory(test_dir)
  )

  expect_true(dir.exists(test_dir))
})

test_that(".ensure_directory handles NULL path", {
  result <- CohortBuilder:::.ensure_directory(NULL)

  # Should return NULL invisibly
  expect_null(result)
})

test_that(".ensure_directory handles empty string path", {
  result <- CohortBuilder:::.ensure_directory("")

  # Should return NULL invisibly for empty path
  expect_null(result)
})

test_that(".ensure_directory creates nested directories with recursive = TRUE", {
  test_dir <- file.path(tempdir(), "parent", "child", "grandchild")
  on.exit({
    unlink(file.path(tempdir(), "parent"), recursive = TRUE)
  })

  CohortBuilder:::.ensure_directory(test_dir)

  expect_true(dir.exists(test_dir))
})

test_that(".ensure_directory uses provided label in logging", {
  skip_if_not_installed("logger")

  test_dir <- file.path(tempdir(), "test_label_dir")
  on.exit({
    unlink(test_dir, recursive = TRUE)
  })

  # Should not error when using custom label
  expect_no_error(
    CohortBuilder:::.ensure_directory(test_dir, label = "custom_output")
  )
})
