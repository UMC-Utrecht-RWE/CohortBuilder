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

run_pipeline <- function(
  d3, seed = 42L, age_offset = 1,
  matching_vars = c("SV_SEX", "SV_REGION", "SV_PRIOR_COVID_DG", "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"),
  date_match_pars = list(
    col_date_match = NULL,
    date_match_offsets = NULL
  )
) {
  db_path <- tempfile(fileext = ".duckdb")

  d4 <- data.table::as.data.table(
    build_study_cohort(
      eligible_pop = d3,
      matching_vars = matching_vars,
      date_match_pars = date_match_pars,
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
      age_offset = age_offset,
      matching_mode = "with_replacement"
    )
  )

  list(d3 = d3, d4 = d4, matching_vars = c(
    "SV_SEX", "SV_REGION", "SV_PRIOR_COVID_DG", "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"
  ))
}

# ---- tests ------------------------------------------------------------------

d3 <- make_d3()

test_that("with-replacement pipeline runs and returns a data.table", {
  res <- run_pipeline(d3)
  expect_s3_class(res$d4, "data.table")
  expect_gt(nrow(res$d4), 0L)
})

test_that("exposed accounting: D3 eligible_exposed == D4 EXPOSED + UNMATCHED", {
  res <- run_pipeline(d3)
  n_exp_d3 <- d3[eligible_exposed == TRUE, .N]
  n_exp_d4 <- res$d4[group %chin% c("EXPOSED", "UNMATCHED"), .N]
  expect_equal(n_exp_d3, n_exp_d4)
})

test_that("D4 group labels are only EXPOSED, CONTROL, or UNMATCHED", {
  res <- run_pipeline(d3)
  expect_true(all(res$d4$group %chin% c("EXPOSED", "CONTROL", "UNMATCHED")))
})

test_that("matching variables are identical within each matched pair", {
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

test_that("matched pairs have the same year_of_birth if age_offset = 0", {
  res <- run_pipeline(d3, seed = 42L, age_offset = 0)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  by_var <- matched[, .(n_unique = data.table::uniqueN(year_of_birth)), by = match_id]
  expect_true(
    by_var[, all(n_unique == 1L)],
    label = "year_of_birth is equal within each pair"
  )
})

test_that("matched pairs have year_of_birth within age_offset if age_offset > 0", {
  age_offset <- 2
  res <- run_pipeline(d3, seed = 42L, age_offset = age_offset)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  by_var <- matched[, .(min_yob = min(year_of_birth), max_yob = max(year_of_birth)), by = match_id]
  expect_true(
    by_var[, all((max_yob - min_yob) <= age_offset)],
    label = paste0("year_of_birth is within age_offset of ", age_offset, " within each pair")
  )
})

test_that("if age_offset = NULL, year_of_birth values can vary within each pair", {
  res <- run_pipeline(d3, seed = 42L, age_offset = NULL)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  by_var <- matched[, .(min_yob = min(year_of_birth), max_yob = max(year_of_birth)), by = match_id]
  expect_true(
    by_var[, any((max_yob - min_yob) > 1)],
    label = "year_of_birth can vary within each pair when age_offset is NULL"
  )
})

test_that("date matching works with specified offsets", {
  d3_preg <- data.table(
    person_id = 1:8,
    start = as.Date(c("2020-01-01", "2019-12-25", "2020-02-03", "2020-01-04", "2020-03-05", "2020-03-01", "2020-01-15", "2020-01-05")),
    end = as.Date(c("2020-01-01", "2020-01-11", "2020-02-03", "2020-02-13", "2020-03-05", "2020-04-15", "2020-01-15", "2020-05-10")),
    eligible_exposed = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    eligible_control = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    SV_PREG_STATUS = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    SV_SEX = c("F", "F", "F", "F", "M", "M", "F", "M"),
    year_of_birth = c(1990, 1990, 1990, 1990, 1985, 1985, 1992, 1993),
    lmp_date = as.Date(c("2019-10-01", "2019-10-05", "2019-11-01", "2019-11-01", NA, NA, NA, NA))
  )

  # Within 1 day
  res <- run_pipeline(d3_preg,
    seed = 42L,
    matching_vars = c("SV_PREG_STATUS", "SV_SEX"),
    age_offset = 1,
    date_match_pars = list(
      col_date_match = "lmp_date",
      date_match_offsets = c(lmp_date = 1) # Allow a 1-day difference in lmp_date for matching
    )
  )
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  # Check that lmp_date differences are within the specified offset (for pairs where both have lmp_date)
  by_var <- matched[
    !is.na(lmp_date),
    .(lmp_diff = as.integer(max(lmp_date) - min(lmp_date))),
    by = match_id
  ]
  expect_true(
    nrow(by_var) == 0L || by_var[, all(lmp_diff <= 1)],
    label = "lmp_date differences are within offset of 1 day within each matched pair"
  )

  # Within 5 days
  res <- run_pipeline(d3_preg,
    seed = 42L,
    matching_vars = c("SV_PREG_STATUS", "SV_SEX"),
    age_offset = 1,
    date_match_pars = list(
      col_date_match = "lmp_date",
      date_match_offsets = c(lmp_date = 5) # Allow a 1-day difference in lmp_date for matching
    )
  )
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  # Check that lmp_date differences are within the specified offset (for pairs where both have lmp_date)
  by_var <- matched[
    !is.na(lmp_date),
    .(lmp_diff = as.integer(max(lmp_date) - min(lmp_date))),
    by = match_id
  ]
  expect_true(
    nrow(by_var) == 0L || by_var[, all(lmp_diff <= 5)],
    label = "lmp_date differences are within offset of 5 days within each matched pair"
  )
})

test_that("matched pairs have year_of_birth within age_offset if age_offset > 0", {
  age_offset <- 2
  res <- run_pipeline(d3, seed = 42L, age_offset = age_offset)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  by_var <- matched[, .(min_yob = min(year_of_birth), max_yob = max(year_of_birth)), by = match_id]
  expect_true(
    by_var[, all((max_yob - min_yob) <= age_offset)],
    label = paste0("year_of_birth is within age_offset of ", age_offset, " within each pair")
  )
})

test_that("if age_offset = NULL, year_of_birth values can vary within each pair", {
  res <- run_pipeline(d3, seed = 42L, age_offset = NULL)
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  by_var <- matched[, .(min_yob = min(year_of_birth), max_yob = max(year_of_birth)), by = match_id]
  expect_true(
    by_var[, any((max_yob - min_yob) > 1)],
    label = "year_of_birth can vary within each pair when age_offset is NULL"
  )
})

test_that("date matching works with more than one matching dates and offsets", {
  d3_preg <- data.table(
    person_id = 1:8,
    start = as.Date(c("2020-01-01", "2019-12-25", "2020-02-03", "2020-01-04", "2020-03-05", "2020-03-01", "2020-01-15", "2020-01-05")),
    end = as.Date(c("2020-01-01", "2020-01-11", "2020-02-03", "2020-02-13", "2020-03-05", "2020-04-15", "2020-01-15", "2020-05-10")),
    eligible_exposed = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    eligible_control = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    SV_PREG_STATUS = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    SV_SEX = c("F", "F", "F", "F", "M", "M", "F", "M"),
    year_of_birth = c(1990, 1990, 1990, 1990, 1985, 1985, 1992, 1993),
    lmp_date = as.Date(c("2019-10-01", "2019-10-05", "2019-11-01", "2019-11-01", NA, NA, NA, NA)),
    one_more_date = as.Date(c(NA, NA, "2019-10-02", "2019-10-06", "2019-11-02", "2019-11-02", NA, NA))
  )

  # Within 1 day
  res <- run_pipeline(d3_preg,
    seed = 42L,
    matching_vars = c("SV_PREG_STATUS", "SV_SEX"),
    age_offset = 1,
    date_match_pars = list(
      col_date_match = c("lmp_date", "one_more_date"),
      date_match_offsets = c(lmp_date = 1, one_more_date = 30) # Allow a 1-day difference in lmp_date and one_more_date for matching
    )
  )
  matched <- res$d4[!is.na(match_id)]

  if (nrow(matched) == 0L) skip("No matched pairs produced")

  # Check that lmp_date differences are within the specified offset (for pairs where both have lmp_date and one_more_date)
  by_var <- matched[
    !is.na(lmp_date) & !is.na(one_more_date),
    .(lmp_diff = as.integer(max(lmp_date) - min(lmp_date)), one_more_diff = as.integer(max(one_more_date) - min(one_more_date))),
    by = match_id
  ]
  expect_true(
    nrow(by_var) == 0L || by_var[, all(lmp_diff <= 1) & all(one_more_diff <= 30)],
    label = "lmp_date differences are within offset of 1 day within each matched pair"
  )
})
