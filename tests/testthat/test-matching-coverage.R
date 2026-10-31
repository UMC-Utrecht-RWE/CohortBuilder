suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
})

# ---- fixtures ---------------------------------------------------------------

make_matching_fixture <- function(n_spells = 20000L, seed = 42L) {
  d3 <- data.table::as.data.table(
    generate_eligibility_data(
      n = as.integer(n_spells),
      start_seed = as.integer(seed),
      save_output = FALSE,
      output_dir = tempdir(),
      output_file = "D3_COVERAGE_TEST"
    )
  )
  elig <- d3[eligible_exposed == TRUE | eligible_control == TRUE]
  vars <- get_matching_variables(
    eligible_pop = d3,
    matching_vars = c("SV_SEX", "SV_REGION", "SV_PRIOR_COVID_DG")
  )
  prof <- get_profile_table(
    eligible_pop = elig,
    matching_vars = vars,
    save_output = FALSE,
    output_dir = tempdir(),
    output_file = "D3_LOOKUP_COVERAGE"
  )
  pop <- get_matching_population(
    eligible_pop = elig,
    profile_table = prof,
    matching_vars = vars,
    save_output = FALSE,
    output_dir = tempdir(),
    output_file = "D3_POP_COVERAGE",
    col_matching_status_start = "start",
    col_matching_status_end = "end"
  )
  list(pop = pop, prof = prof, vars = vars)
}

fx <- make_matching_fixture()

nr_query <- suppressWarnings(getSQL(system.file(
  "sql_queries",
  "matching_query_without_replacement.sql",
  package = "CohortBuilder"
)))
wr_query <- suppressWarnings(getSQL(system.file(
  "sql_queries",
  "matching_query_with_replacement.sql",
  package = "CohortBuilder"
)))
wr_target_query <- getSQL(system.file(
  "sql_queries",
  "create_matching_target_table.sql",
  package = "CohortBuilder"
))

new_conn <- function() {
  DBI::dbConnect(duckdb::duckdb(), tempfile(fileext = ".duckdb"))
}

with_conn <- function(fn) {
  conn <- new_conn()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  fn(conn)
}

as_msc <- function(x) {
  class(x) <- unique(c("matching_study_cohort", class(x)))
  x
}

with_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expr
}

call_nr <- function(conn, ...) {
  match_cohorts_without_replacement(
    matching_pop_groupkey = fx$pop,
    matching_vars = fx$vars,
    profile_table = fx$prof,
    matching_query = nr_query,
    matching_conn = conn,
    col_matching_status_start = "start",
    col_matching_status_end = "end",
    ...
  )
}

call_wr <- function(conn, ...) {
  match_cohorts_with_replacement(
    matching_pop_groupkey = fx$pop,
    profile_table = fx$prof,
    matching_query = wr_query,
    target_table_query = wr_target_query,
    matching_vars = fx$vars,
    matching_conn = conn,
    n_cores = 2,
    col_matching_status_start = "start",
    col_matching_status_end = "end",
    ...
  )
}

# ---- without replacement ----------------------------------------------------

test_that("without-replacement matching batches exposed spells and control buckets", {
  res <- with_conn(function(conn) {
    call_nr(
      conn,
      n_cores = 2,
      exposed_batch_size = 50L,
      control_batch_count = 2L
    )
  })
  expect_s3_class(res, "data.table")
  expect_true(any(res$group == "EXPOSED"))
})

test_that("without-replacement matching derives control batches from control_batch_size", {
  res <- with_conn(function(conn) {
    call_nr(
      conn,
      n_cores = 2,
      exposed_batch_size = 500L,
      control_batch_size = 200L
    )
  })
  expect_s3_class(res, "data.table")
})

test_that("without-replacement matching writes parquet when save_output is TRUE", {
  out_dir <- file.path(tempdir(), "nr_save")
  dir.create(out_dir, showWarnings = FALSE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  with_conn(function(conn) {
    call_nr(
      conn,
      n_cores = 2,
      save_output = TRUE,
      result_dir = out_dir,
      result_file = "NR_TEST"
    )
  })
  expect_true(file.exists(file.path(out_dir, "NR_TEST.parquet")))
})

test_that("without-replacement matching returns an empty cohort when no exposed spells exist", {
  pop_no_exp <- data.table::copy(fx$pop)
  pop_no_exp[group == "exposed", group := "control"]

  res <- with_conn(function(conn) {
    match_cohorts_without_replacement(
      matching_pop_groupkey = pop_no_exp,
      matching_vars = fx$vars,
      profile_table = fx$prof,
      matching_query = nr_query,
      matching_conn = conn,
      save_output = FALSE,
      col_matching_status_start = "start",
      col_matching_status_end = "end"
    )
  })
  expect_equal(nrow(res), 0L)
})

test_that("without-replacement empty cohort is written when save_output is TRUE", {
  out_dir <- file.path(tempdir(), "nr_empty")
  dir.create(out_dir, showWarnings = FALSE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  pop_no_exp <- data.table::copy(fx$pop)
  pop_no_exp[group == "exposed", group := "control"]

  with_conn(function(conn) {
    match_cohorts_without_replacement(
      matching_pop_groupkey = pop_no_exp,
      matching_vars = fx$vars,
      profile_table = fx$prof,
      matching_query = nr_query,
      matching_conn = conn,
      save_output = TRUE,
      result_dir = out_dir,
      result_file = "NR_EMPTY",
      col_matching_status_start = "start",
      col_matching_status_end = "end"
    )
  })
  expect_true(file.exists(file.path(out_dir, "NR_EMPTY.parquet")))
})

test_that("without-replacement matching uses the packaged default query when none is given", {
  res <- with_conn(function(conn) {
    suppressMessages(match_cohorts_without_replacement(
      matching_pop_groupkey = fx$pop,
      matching_vars = fx$vars,
      profile_table = fx$prof,
      matching_query = NULL,
      matching_conn = conn,
      n_cores = 2,
      col_matching_status_start = "start",
      col_matching_status_end = "end"
    ))
  })
  expect_s3_class(res, "data.table")
})

test_that("without-replacement matching rejects invalid batch and offset arguments", {
  with_conn(function(conn) {
    expect_error(call_nr(conn, exposed_batch_size = 0L), "exposed_batch_size")
    expect_error(call_nr(conn, control_batch_count = 0L), "control_batch_count")
    expect_error(call_nr(conn, control_batch_size = 0L), "control_batch_size")
    expect_error(call_nr(conn, age_offset = -1), "age_offset")
    expect_error(call_nr(conn, age_offset = NULL), "age_offset")
  })
})

# ---- with replacement -------------------------------------------------------

test_that("with-replacement matching forces one iteration when bootstrap is off", {
  res <- with_conn(function(conn) {
    call_wr(conn, n_bootstraps = 5, save_output = FALSE)
  })
  expect_s3_class(res, "data.table")
})

test_that("with-replacement matching accepts NULL n_bootstraps when bootstrap is off", {
  res <- with_conn(function(conn) {
    call_wr(conn, n_bootstraps = NULL, save_output = FALSE)
  })
  expect_s3_class(res, "data.table")
})

test_that("with-replacement matching runs bootstrap iterations and returns NULL", {
  res <- with_conn(function(conn) {
    call_wr(
      conn,
      with_bootstrap = TRUE,
      n_bootstraps = 2,
      start_seed = 7,
      dir_bootstrap = tempdir()
    )
  })
  expect_null(res)
})

test_that("with-replacement matching writes parquet when save_output is TRUE", {
  out_dir <- file.path(tempdir(), "wr_save")
  dir.create(out_dir, showWarnings = FALSE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  with_conn(function(conn) {
    call_wr(
      conn,
      save_output = TRUE,
      result_dir = out_dir,
      result_file = "WR_TEST"
    )
  })
  expect_true(file.exists(file.path(out_dir, "WR_TEST.parquet")))
})

test_that("with-replacement matching supports age_offset = NULL", {
  res <- with_conn(function(conn) {
    call_wr(conn, age_offset = NULL, save_output = FALSE)
  })
  expect_s3_class(res, "data.table")
})

test_that("with-replacement matching rejects a non-numeric age_offset", {
  with_conn(function(conn) {
    expect_error(
      call_wr(conn, age_offset = "x"),
      "age_offset must be 1 or NULL"
    )
  })
})

test_that("with-replacement matching uses packaged default queries when none are given", {
  res <- with_conn(function(conn) {
    suppressMessages(match_cohorts_with_replacement(
      matching_pop_groupkey = fx$pop,
      profile_table = fx$prof,
      matching_query = NULL,
      target_table_query = NULL,
      matching_vars = fx$vars,
      matching_conn = conn,
      n_cores = 2,
      save_output = FALSE,
      col_matching_status_start = "start",
      col_matching_status_end = "end"
    ))
  })
  expect_s3_class(res, "data.table")
})

# ---- cohort summary and plot helpers ----------------------------------------

test_that(".log_matching_summary handles non-data.table input and missing columns", {
  expect_no_error(.log_matching_summary(data.frame(
    group = "EXPOSED",
    person_id = "a"
  )))
  expect_no_error(.log_matching_summary(data.table::data.table(x = 1)))
})

test_that(".log_matching_summary logs with and without matched controls", {
  with_controls <- data.table::data.table(
    group = c("EXPOSED", "CONTROL", "CONTROL"),
    person_id = c("a", "b", "b")
  )
  no_controls <- data.table::data.table(
    group = c("EXPOSED", "UNMATCHED"),
    person_id = c("a", "c")
  )
  expect_no_error(.log_matching_summary(with_controls))
  expect_no_error(.log_matching_summary(no_controls))
})

test_that("summary errors on missing columns", {
  expect_error(
    summary(as_msc(data.table::data.table(x = 1))),
    "Missing required columns"
  )
})

test_that("summary returns tables with and without matched controls", {
  with_controls <- data.table::data.table(
    group = c("EXPOSED", "CONTROL", "CONTROL"),
    person_id = c("a", "b", "b")
  )
  no_controls <- data.table::data.table(
    group = c("EXPOSED", "UNMATCHED"),
    person_id = c("a", "c")
  )

  out_with <- invisible(capture.output(
    res_with <- summary(as_msc(with_controls))
  ))
  out_none <- invisible(capture.output(
    res_none <- summary(as_msc(no_controls))
  ))

  expect_false(is.null(res_with$control_usage_distribution))
  expect_null(res_none$control_usage_distribution)
  expect_named(res_with, c("episodes_by_group", "control_usage_distribution"))
})

test_that("summary accepts a plain data.frame", {
  df <- data.frame(group = c("EXPOSED", "CONTROL"), person_id = c("a", "b"))
  invisible(capture.output(res <- summary(as_msc(df))))
  expect_named(res, c("episodes_by_group", "control_usage_distribution"))
})

cohort_dt <- data.table::data.table(
  group = c("EXPOSED", "CONTROL", "UNMATCHED", "EXPOSED"),
  person_id = c("a", "b", "c", "d"),
  T0 = as.Date(c("2020-01-01", "2020-03-01", "2020-06-01", "2021-01-01")),
  SV_SEX = c("F", NA, "M", "F")
)

test_that("plot draws the default time histogram", {
  with_null_device(expect_no_error(plot(as_msc(cohort_dt))))
})

test_that("plot accepts a plain data.frame and character T0 dates", {
  df <- as.data.frame(cohort_dt)
  df$T0 <- as.character(df$T0)
  with_null_device(expect_no_error(plot(as_msc(df))))
})

test_that("plot draws stacked bars over a categorical variable with missing values", {
  with_null_device(expect_no_error(plot(as_msc(cohort_dt), by_var = "SV_SEX")))
  with_null_device(expect_no_error(plot(as_msc(cohort_dt), by_var = "group")))
})

test_that("plot errors on missing columns and non-date time values", {
  expect_error(
    plot(as_msc(cohort_dt), by_var = "nope"),
    "Missing required columns"
  )
  bad <- data.table::copy(cohort_dt)
  bad[, T0 := c("not", "a", "date", "here")]
  expect_error(with_null_device(plot(as_msc(bad))), "Could not convert")
})

test_that(".cb_sensible_hist_bins returns bounded bin counts", {
  expect_equal(.cb_sensible_hist_bins(as.Date("2020-01-01")), 1L)
  many <- as.Date("2020-01-01") + seq(0, 3000, by = 7)
  bins <- .cb_sensible_hist_bins(many)
  expect_gte(bins, 8L)
  expect_lte(bins, 60L)
})

test_that("base histogram helper draws binned counts and handles a single value", {
  with_null_device(expect_null(.plot_msc_hist_base(
    cohort_dt,
    by_var = "T0",
    n_bins = 4L
  )))
  same_day <- data.table::data.table(
    group = c("EXPOSED", "CONTROL"),
    T0 = as.Date(c("2020-01-01", "2020-01-01"))
  )
  with_null_device(expect_null(.plot_msc_hist_base(same_day, by_var = "T0")))
})

test_that("base histogram helper errors when no finite values exist", {
  empty <- data.table::data.table(group = "EXPOSED", T0 = as.Date(NA))
  expect_error(
    with_null_device(.plot_msc_hist_base(empty, by_var = "T0")),
    "No non-missing values"
  )
})

test_that("base stacked helper draws horizontal stacked bars", {
  plot_data <- data.table::data.table(
    plot_level = factor(c("F", "M", "F")),
    group = c("EXPOSED", "CONTROL", "EXPOSED"),
    n_matches = c(2L, 1L, 3L)
  )
  with_null_device(expect_null(.plot_msc_stacked_base(
    plot_data,
    by_var = "SV_SEX"
  )))
})
