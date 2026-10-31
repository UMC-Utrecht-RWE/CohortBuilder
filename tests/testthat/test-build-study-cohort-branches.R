suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
})

# ---- fixtures ---------------------------------------------------------------

make_small_d3 <- function(n_spells = 2000L, seed = 7L) {
  data.table::as.data.table(
    generate_eligibility_data(
      n = as.integer(n_spells),
      start_seed = as.integer(seed),
      save_output = FALSE,
      output_dir = tempdir(),
      output_file = "D3_ELIGIBILITY_BRANCH_TEST"
    )
  )
}

branch_args <- function(
  d3,
  output_dir = tempdir(),
  save_output = FALSE,
  save_intermediate = FALSE,
  matching_mode = "with_replacement"
) {
  list(
    eligible_pop = d3,
    matching_query = suppressWarnings(getSQL(system.file(
      "sql_queries",
      paste0("matching_query_", matching_mode, ".sql"),
      package = "CohortBuilder"
    ))),
    target_table_query = getSQL(system.file(
      "sql_queries",
      "create_matching_target_table.sql",
      package = "CohortBuilder"
    )),
    matching_vars = c("SV_SEX", "SV_REGION", "SV_PRIOR_COVID_DG"),
    dir_matching_db = tempfile(fileext = ".duckdb"),
    n_cores = NULL,
    log_name = file.path(tempdir(), "log_branch_test"),
    output_pars = list(
      save_output = save_output,
      dir_output = output_dir,
      output_file_name = "D4_MSC_BRANCH_TEST"
    ),
    intermediate_output_pars = list(
      save_intermediate_outputs = save_intermediate,
      dir_intermediate_outputs = output_dir,
      profile_table_name = "D3_LOOKUP_TABLE_BRANCH",
      matching_pop_name = "D3_MATCHING_POP_BRANCH"
    ),
    bootstrap_pars = list(
      with_bootstrap = FALSE,
      n_bootstraps = 1L,
      dir_bootstrap = tempdir(),
      start_seed = 7L
    ),
    input_column_names = list(
      col_person_id = "person_id",
      col_eligible_exposed = "eligible_exposed",
      col_eligible_control = "eligible_control",
      col_matching_status_start = "start",
      col_matching_status_end = "end",
      col_age_iterator = "year_of_birth"
    ),
    output_column_names = list(
      col_person_id = "person_id",
      col_match_id = "match_id",
      col_treatment_group = "group",
      col_T0 = "T0"
    ),
    age_offset = 1,
    matching_mode = matching_mode
  )
}

d3_small <- make_small_d3()

# ---- tests ------------------------------------------------------------------

test_that("build_study_cohort rejects an unknown matching_mode", {
  args <- branch_args(d3_small)
  args$matching_mode <- "bogus"

  expect_error(
    do.call(build_study_cohort, args),
    "matching_mode"
  )
})

test_that("build_study_cohort errors when no eligible exposed spells exist", {
  d3_no_exposed <- data.table::copy(d3_small)
  d3_no_exposed[, eligible_exposed := FALSE]

  expect_error(
    do.call(build_study_cohort, branch_args(d3_no_exposed)),
    "No eligible exposed"
  )
})

test_that("build_study_cohort loads default SQL when queries are empty", {
  args <- branch_args(d3_small)
  args$matching_query <- ""
  args$target_table_query <- ""

  res <- data.table::as.data.table(do.call(build_study_cohort, args))

  expect_s3_class(res, "data.table")
})

test_that("build_study_cohort runs without-replacement mode", {
  args <- branch_args(d3_small, matching_mode = "without_replacement")
  args$target_table_query <- ""

  res <- data.table::as.data.table(do.call(build_study_cohort, args))

  expect_s3_class(res, "data.table")
})

test_that("build_study_cohort warns when no eligible controls exist", {
  d3_no_controls <- data.table::copy(d3_small)
  d3_no_controls[, eligible_control := FALSE]

  expect_warning(
    suppressMessages(do.call(build_study_cohort, branch_args(d3_no_controls))),
    "No eligible controls"
  )
})

test_that("build_study_cohort writes output and intermediate files when requested", {
  out_dir <- file.path(tempdir(), "branch_outputs")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  args <- branch_args(
    d3_small,
    output_dir = out_dir,
    save_output = TRUE,
    save_intermediate = TRUE
  )
  res <- data.table::as.data.table(do.call(build_study_cohort, args))

  expect_s3_class(res, "data.table")
  expect_true(file.exists(file.path(out_dir, "D4_MSC_BRANCH_TEST.parquet")))
})
