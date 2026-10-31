suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
  library(logger)
})

# ---- test fixtures ----------------------------------------------------------

make_test_file <- function(dir = tempdir()) {
  test_file <- file.path(dir, "test_file.txt")
  cat("test content\n", file = test_file)
  test_file
}

make_test_db <- function(dir = tempdir()) {
  test_db <- file.path(dir, "test.duckdb")
  file.create(test_db)
  test_db
}

make_test_bootstrap_dir <- function(base_dir = tempdir()) {
  bootstrap_dir <- file.path(base_dir, "bootstrap_test")
  if (!dir.exists(bootstrap_dir)) {
    dir.create(bootstrap_dir, recursive = TRUE)
  }
  bootstrap_dir
}

# ---- set_matching_environment tests ----------------------------------------

test_that("set_matching_environment requires eligible_pop argument", {
  matching_query <- make_test_file()
  on.exit(unlink(matching_query))

  expect_error(
    set_matching_environment(
      eligible_pop = NULL,
      matching_query = matching_query
    ),
    "Eligible population data is missing"
  )
})

test_that("set_matching_environment requires matching_query argument", {
  eligible_pop <- make_test_file()
  on.exit(unlink(eligible_pop))

  expect_error(
    set_matching_environment(
      eligible_pop = eligible_pop,
      matching_query = NULL
    ),
    "Matching query is missing"
  )
})

test_that("set_matching_environment works with basic inputs", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  db_path <- file.path(tempdir(), "test_basic.duckdb")

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(db_path)
  })

  expect_no_error(
    set_matching_environment(
      eligible_pop = eligible_pop,
      matching_query = matching_query,
      with_bootstrap = FALSE,
      dir_matching_db = db_path
    )
  )
})

test_that("set_matching_environment removes existing database", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  db_path <- make_test_db()

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(db_path)
  })

  expect_true(file.exists(db_path))

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    dir_matching_db = db_path
  )

  expect_false(file.exists(db_path))
})

test_that("set_matching_environment handles non-existent database gracefully", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  db_path <- file.path(tempdir(), "nonexistent.duckdb")

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(db_path)
  })

  # Should not error when database doesn't exist
  expect_no_error(
    set_matching_environment(
      eligible_pop = eligible_pop,
      matching_query = matching_query,
      dir_matching_db = db_path
    )
  )
})

test_that("set_matching_environment requires n_bootstraps when with_bootstrap = TRUE", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  bootstrap_dir <- make_test_bootstrap_dir()

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(bootstrap_dir, recursive = TRUE)
  })

  expect_error(
    set_matching_environment(
      eligible_pop = eligible_pop,
      matching_query = matching_query,
      with_bootstrap = TRUE,
      n_bootstraps = NULL,
      dir_bootstrap = bootstrap_dir
    ),
    "Please specify argument `n_bootstraps`"
  )
})

test_that("set_matching_environment creates bootstrap directory if it doesn't exist", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  bootstrap_dir <- file.path(tempdir(), "new_bootstrap_dir")
  db_path <- file.path(tempdir(), "test_create_dir.duckdb")

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(bootstrap_dir, recursive = TRUE)
    unlink(db_path)
  })

  expect_false(dir.exists(bootstrap_dir))

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    with_bootstrap = TRUE,
    n_bootstraps = 10L,
    dir_bootstrap = bootstrap_dir,
    dir_matching_db = db_path
  )

  expect_true(dir.exists(bootstrap_dir))
})

test_that("set_matching_environment skips bootstrap setup when with_bootstrap = FALSE", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  bootstrap_dir <- file.path(tempdir(), "should_not_create")
  db_path <- file.path(tempdir(), "test_skip_bootstrap.duckdb")

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(bootstrap_dir, recursive = TRUE)
    unlink(db_path)
  })

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    with_bootstrap = FALSE,
    dir_bootstrap = bootstrap_dir,
    dir_matching_db = db_path
  )

  # Directory should not be created if with_bootstrap = FALSE
  expect_false(dir.exists(bootstrap_dir))
})

test_that("set_matching_environment removes old bootstrap files", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  bootstrap_dir <- make_test_bootstrap_dir()
  db_path <- file.path(tempdir(), "test_remove_bootstrap.duckdb")

  # Create some bootstrap files
  bootstrap_file_1 <- file.path(bootstrap_dir, "result_bootstrap_1.parquet")
  bootstrap_file_2 <- file.path(bootstrap_dir, "result_bootstrap_2.parquet")
  non_bootstrap_file <- file.path(bootstrap_dir, "other_file.txt")

  file.create(bootstrap_file_1)
  file.create(bootstrap_file_2)
  file.create(non_bootstrap_file)

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(bootstrap_dir, recursive = TRUE)
    unlink(db_path)
  })

  expect_true(file.exists(bootstrap_file_1))
  expect_true(file.exists(bootstrap_file_2))
  expect_true(file.exists(non_bootstrap_file))

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    with_bootstrap = TRUE,
    n_bootstraps = 10L,
    dir_bootstrap = bootstrap_dir,
    dir_matching_db = db_path
  )

  # Bootstrap files should be removed
  expect_false(file.exists(bootstrap_file_1))
  expect_false(file.exists(bootstrap_file_2))

  # Non-bootstrap files should remain
  expect_true(file.exists(non_bootstrap_file))
})

test_that("set_matching_environment handles bootstrap directory with no bootstrap files", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  bootstrap_dir <- make_test_bootstrap_dir()
  db_path <- file.path(tempdir(), "test_no_bootstrap_files.duckdb")

  # Create a non-matching file
  other_file <- file.path(bootstrap_dir, "other.txt")
  file.create(other_file)

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(bootstrap_dir, recursive = TRUE)
    unlink(db_path)
  })

  # Should not error when no bootstrap files exist
  expect_no_error(
    set_matching_environment(
      eligible_pop = eligible_pop,
      matching_query = matching_query,
      with_bootstrap = TRUE,
      n_bootstraps = 10L,
      dir_bootstrap = bootstrap_dir,
      dir_matching_db = db_path
    )
  )
})

test_that("set_matching_environment matches bootstrap file pattern correctly", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  bootstrap_dir <- make_test_bootstrap_dir()
  db_path <- file.path(tempdir(), "test_pattern.duckdb")

  # Create files with various naming patterns
  bootstrap_file_valid <- file.path(
    bootstrap_dir,
    "result_bootstrap_123.parquet"
  )
  bootstrap_file_valid_2 <- file.path(
    bootstrap_dir,
    "data_bootstrap_999.parquet"
  )
  bootstrap_file_invalid_1 <- file.path(
    bootstrap_dir,
    "result_bootstrap.parquet"
  ) # No number
  bootstrap_file_invalid_2 <- file.path(bootstrap_dir, "bootstrap_123.parquet") # No underscore before bootstrap

  file.create(bootstrap_file_valid)
  file.create(bootstrap_file_valid_2)
  file.create(bootstrap_file_invalid_1)
  file.create(bootstrap_file_invalid_2)

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(bootstrap_dir, recursive = TRUE)
    unlink(db_path)
  })

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    with_bootstrap = TRUE,
    n_bootstraps = 10L,
    dir_bootstrap = bootstrap_dir,
    dir_matching_db = db_path
  )

  # Valid bootstrap files should be removed
  expect_false(file.exists(bootstrap_file_valid))
  expect_false(file.exists(bootstrap_file_valid_2))

  # Invalid pattern files should remain
  expect_true(file.exists(bootstrap_file_invalid_1))
  expect_true(file.exists(bootstrap_file_invalid_2))
})

test_that("set_matching_environment returns NULL invisibly", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  db_path <- file.path(tempdir(), "test_invisible.duckdb")

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(db_path)
  })

  # The function should run without error
  expect_no_error(
    set_matching_environment(
      eligible_pop = eligible_pop,
      matching_query = matching_query,
      dir_matching_db = db_path
    )
  )
})

test_that("set_matching_environment works with all parameters specified", {
  eligible_pop <- make_test_file()
  matching_query <- make_test_file()
  db_path <- make_test_db()
  bootstrap_dir <- make_test_bootstrap_dir()

  # Create a bootstrap file to be cleaned
  bootstrap_file <- file.path(bootstrap_dir, "old_bootstrap_1.parquet")
  file.create(bootstrap_file)

  on.exit({
    unlink(eligible_pop)
    unlink(matching_query)
    unlink(db_path)
    unlink(bootstrap_dir, recursive = TRUE)
  })

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    with_bootstrap = TRUE,
    n_bootstraps = 10L,
    dir_bootstrap = bootstrap_dir,
    dir_matching_db = db_path
  )

  # Database should be removed
  expect_false(file.exists(db_path))

  # Bootstrap directory should exist
  expect_true(dir.exists(bootstrap_dir))

  # Bootstrap file should be removed
  expect_false(file.exists(bootstrap_file))
})
