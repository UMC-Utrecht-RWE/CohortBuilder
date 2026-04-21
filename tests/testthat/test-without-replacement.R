suppressPackageStartupMessages({
  library(data.table)
  library(CohortBuilder)
})

# ---- helpers ----------------------------------------------------------------

assert_that <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

# ---- fixtures ---------------------------------------------------------------

make_d3 <- function(n_spells = 2e5, seed = 42L) {
  data.table::as.data.table(
    generate_eligibility_data(
      n = as.integer(n_spells),
      start_seed = as.integer(seed),
      save_output = FALSE,
      output_dir = tempdir(),
      output_file = "D3_ELIGIBILITY_TEST"
    )
  )
}

run_pipeline <- function(d3, seed = 42L) {
  matching_vars <- c(
    "SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_PRIOR_COVID_DG", "SV_BRAND_COVID_VACC",
    "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"
  )

  matching_query <- suppressWarnings(
    getSQL(system.file("sql_queries", "matching_query_without_replacement.sql", package = "CohortBuilder"))
  )
  target_table_query <- getSQL(
    system.file("sql_queries", "create_matching_target_table.sql", package = "CohortBuilder")
  )
  db_path <- tempfile(fileext = ".duckdb")

  d4 <- build_study_cohort(
    eligible_pop = d3,
    matching_query = matching_query,
    target_table_query = target_table_query,
    matching_vars = matching_vars,
    dir_matching_db = db_path,
    n_cores = NULL,
    log_name = file.path(tempdir(), "log_test"),
    output_pars = list(
      save_output = FALSE,
      dir_output = tempdir(),
      output_file_name = "D4_MSC_TEST"
    ),
    intermediate_output_pars = list(
      save_intermediate_outputs = FALSE,
      dir_intermediate_outputs = tempdir(),
      profile_table_name = "D3_LOOKUP_TABLE",
      matching_pop_name = "D3_MATCHING_POP"
    ),
    bootstrap_pars = list(
      with_bootstrap = FALSE,
      n_bootstraps = 1L,
      dir_bootstrap = tempdir(),
      start_seed = as.integer(seed)
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
    diagnostic_pars = list(
      compute_diagnostics = FALSE,
      save_diagnostics = FALSE,
      include_plots = FALSE,
      top_n_profiles = 10L
    ),
    matching_mode = "without_replacement"
  )

  list(d3 = d3, d4 = d4, matching_vars = c(
    "SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_PRIOR_COVID_DG", "SV_BRAND_COVID_VACC",
    "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"
  ))
}

# ---- tests ------------------------------------------------------------------

test_that("no-replacement pipeline runs and returns a data.table", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  expect_s3_class(res$d4, "data.table")
  expect_gt(nrow(res$d4), 0L)
})

test_that("exposed accounting: D3 eligible_exposed == D4 EXPOSED + UNMATCHED", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  n_exp_d3 <- d3[eligible_exposed == TRUE, .N]
  n_exp_d4 <- res$d4[group %chin% c("EXPOSED", "UNMATCHED"), .N]
  expect_equal(n_exp_d3, n_exp_d4)
})

test_that("D4 group labels are only EXPOSED, CONTROL, or UNMATCHED", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  expect_true(all(res$d4$group %chin% c("EXPOSED", "CONTROL", "UNMATCHED")))
})

test_that("no person appears in more than one matched row (strict no replacement)", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  matched <- res$d4[!is.na(match_id)]
  dup <- matched[, .N, by = person_id][N > 1L]
  expect_equal(nrow(dup), 0L)
})

test_that("UNMATCHED rows have NA match_id; matched rows have non-NA match_id and valid group", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  expect_true(res$d4[group == "UNMATCHED", all(is.na(match_id))])
  matched <- res$d4[!is.na(match_id)]
  expect_true(matched[, all(group %chin% c("EXPOSED", "CONTROL"))])
})

test_that("each match_id has exactly one EXPOSED and one CONTROL row with identical T0", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  pair_counts <- matched[, .N, by = match_id]
  expect_true(pair_counts[, all(N == 2L)])

  pair_group <- matched[, .N, by = .(match_id, group)]
  expect_true(pair_group[, all(N == 1L)])

  pair_t0 <- matched[, .(n_t0 = data.table::uniqueN(T0)), by = match_id]
  expect_true(pair_t0[, all(n_t0 == 1L)])
})

test_that("no self-match pairs (exposed and control person_id differ within each pair)", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  self_match <- matched[, .(n_person = data.table::uniqueN(person_id)), by = match_id][n_person < 2L]
  expect_equal(nrow(self_match), 0L)
})

test_that("matching variables are identical within each matched pair", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  for (v in res$matching_vars) {
    if (!v %in% colnames(matched)) next
    by_var <- matched[, .(n_unique = data.table::uniqueN(get(v))), by = match_id]
    expect_true(
      by_var[, all(n_unique == 1L)],
      label = paste0("matching variable `", v, "` is equal within each pair")
    )
  }
})

test_that("matching diagnostics return expected tables and variable coverage", {
  d3 <- make_d3(n_spells = 5e4)
  res <- run_pipeline(d3)

  diagnostics <- get_matching_diagnostics(
    eligible_pop = res$d3,
    matched_cohort = res$d4,
    matching_vars = res$matching_vars,
    include_plots = FALSE,
    top_n_profiles = 8L
  )

  expect_s3_class(diagnostics, "matching_diagnostics")
  expect_named(
    diagnostics$tables,
    c("variable_level_summary", "variable_bottleneck_summary", "profile_bottleneck_summary")
  )
  expect_true(all(res$matching_vars %chin% diagnostics$tables$variable_level_summary$matching_variable))
  expect_true(all(res$matching_vars %chin% diagnostics$tables$variable_bottleneck_summary$matching_variable))
  expect_lte(nrow(diagnostics$tables$profile_bottleneck_summary), 8L)
})

test_that("build_study_cohort attaches diagnostics when requested", {
  d3 <- make_d3(n_spells = 5e4)

  matching_vars <- c(
    "SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_PRIOR_COVID_DG", "SV_BRAND_COVID_VACC",
    "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"
  )

  matching_query <- suppressWarnings(
    getSQL(system.file("sql_queries", "matching_query_without_replacement.sql", package = "CohortBuilder"))
  )
  target_table_query <- getSQL(
    system.file("sql_queries", "create_matching_target_table.sql", package = "CohortBuilder")
  )

  d4 <- data.table::as.data.table(
    build_study_cohort(
      eligible_pop = d3,
      matching_query = matching_query,
      target_table_query = target_table_query,
      matching_vars = matching_vars,
      dir_matching_db = tempfile(fileext = ".duckdb"),
      log_name = file.path(tempdir(), "log_test_diag"),
      output_pars = list(
        save_output = FALSE,
        dir_output = tempdir(),
        output_file_name = "D4_MSC_TEST"
      ),
      intermediate_output_pars = list(
        save_intermediate_outputs = FALSE,
        dir_intermediate_outputs = tempdir(),
        profile_table_name = "D3_LOOKUP_TABLE",
        matching_pop_name = "D3_MATCHING_POP"
      ),
      bootstrap_pars = list(
        with_bootstrap = FALSE,
        n_bootstraps = 1L,
        dir_bootstrap = tempdir(),
        start_seed = 42L
      ),
      diagnostic_pars = list(
        compute_diagnostics = TRUE,
        save_diagnostics = FALSE,
        include_plots = FALSE,
        top_n_profiles = 12L
      ),
      matching_mode = "without_replacement"
    )
  )

  diagnostics <- attr(d4, "matching_diagnostics")
  expect_s3_class(diagnostics, "matching_diagnostics")
  expect_true(nrow(diagnostics$tables$variable_level_summary) > 0L)
})

test_that("matching study cohort output has class matching_study_cohort", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  expect_s3_class(res$d4, "matching_study_cohort")
  expect_s3_class(res$d4, "data.table")
})

test_that("summary method works on matching study cohort", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  d4 <- res$d4

  # summary() should return a list with two elements (invisibly)
  summary_result <- capture.output({
    invisible_result <- summary(d4)
  })

  # Check that summary output was printed
  expect_true(length(summary_result) > 0L)
  expect_true(any(grepl("Episodes by Matching Status", summary_result)))
  expect_true(any(grepl("Distribution of Control Usage", summary_result)))
})

test_that("summary method shows correct episode counts", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  d4 <- res$d4

  # Capture output and check control usage stats
  summary_result <- capture.output({
    invisible_result <- summary(d4)
  })

  # Check that key statistics are shown
  summary_text <- paste(summary_result, collapse = "\n")
  expect_true(any(grepl("Total Episodes", summary_text)))
  expect_true(any(grepl("Total Unique Control Person IDs|No matched controls", summary_text)))
})

test_that("plot method works on matching study cohort with default T0 histogram", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  d4 <- res$d4

  # ggplot2 may or may not be installed; both should work without error
  expect_silent({
    plot(d4)
  })
})

test_that("plot method works on matching study cohort with user-specified by_var", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  d4 <- res$d4

  expect_silent({
    plot(d4, by_var = "group")
  })
})

test_that("plot method validates by_var parameter", {
  d3 <- make_d3()
  res <- run_pipeline(d3)
  d4 <- res$d4

  expect_error(
    plot(d4, by_var = "not_a_column"),
    "Missing required columns"
  )
})
