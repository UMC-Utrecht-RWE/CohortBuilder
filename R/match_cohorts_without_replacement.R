# Match cohorts without replacement ---------------------------------------

#' Match Cohorts Without Replacement Using Greedy Matching
#'
#' Performs greedy exposed-control matching without replacement using DuckDB-backed
#' SQL candidate generation and round-wise person-level pool reduction.
#' Exposed spells are prioritised by earliest `T0` (the exposed spell start
#' date), with deterministic seeded tie-breaking. Once a person is matched, that
#' person is removed from both the exposed and control pools for all subsequent rounds.
#'
#' @param matching_pop_groupkey A data frame or data.table containing the eligible
#'   matching population with at least person identifier, matching interval, year of birth,
#'   group indicator, and `groupkey`. Typically this is the output of
#'   `get_matching_population()`.
#' @param matching_vars A character vector of exact matching variable names to append
#'   back to the final output via the lookup table.
#' @param profile_table A data frame or data.table containing one row per matching
#'   profile and the associated `groupkey`. Typically this is the output of
#'   `get_profile_table()`.
#' @param matching_query SQL query template used for greedy candidate generation.
#'   The query should return accepted candidate pairs and may include the placeholder
#'   string `__START_SEED__`, which will be replaced by `start_seed`.
#' @param matching_conn A DuckDB connection used to create temporary matching tables
#'   and execute the greedy candidate selection queries.
#' @param result_dir Directory where the output file will be written if
#'   `save_output = TRUE`.
#' @param result_file Base filename for the saved output, without extension.
#' @param save_output Logical. If `TRUE`, writes the result to disk as a
#'   `.parquet` file. If `FALSE`, returns the matched cohort as an object.
#' @param n_cores Integer number of DuckDB threads to use. Defaults to all detected
#'   cores minus one.
#' @param start_seed Integer seed used for deterministic tie-breaking in the greedy
#'   matching order and control selection.
#' @param col_person_id Column name for the person identifier. Default is `"person_id"`.
#' @param col_match_id Column name for the match identifier in the returned output.
#'   Default is `"match_id"`.
#' @param col_treatment_group Column name for the output treatment group label.
#'   Default is `"group"`.
#' @param col_T0 Column name for the output index date. Default is `"T0"`.
#' @param col_matching_status_start Column name for the matching interval start date.
#'   Default is `"matching_status_start"`.
#' @param col_matching_status_end Column name for the matching interval end date.
#'   Default is `"matching_status_end"`.
#' @param col_age_iterator Column name for the year-of-birth matching variable.
#'   Default is `"year_of_birth"`.
#'
#' @return If `save_output = FALSE`, returns a data.table with one row per
#'   output cohort record. Matched exposed and matched controls share the same
#'   `match_id`; unmatched exposed spells are returned with `NA` in
#'   `match_id` and group label `"UNMATCHED"`. If `save_output = TRUE`,
#'   the result is written to disk and not returned.
#'
#' @details
#' The algorithm proceeds in greedy rounds:
#' \enumerate{
#'   \item Select all currently available exposed spells and order them by ascending
#'     exposed start date, then apply seeded deterministic tie-breaking.
#'   \item For each exposed spell, identify currently available control spells with
#'     the same `groupkey`, compatible interval overlap, different person identifier,
#'     and year of birth within exposed year ± 1.
#'   \item Select one candidate control per exposed spell using seeded deterministic ranking.
#'   \item Resolve collisions so that a control cannot be assigned to multiple exposed
#'     spells and a person cannot appear more than once within the same round across
#'     either role.
#'   \item Remove all matched persons from both exposed and control pools.
#'   \item Repeat until no additional matches can be formed.
#' }
#'
#' Exposed spells that are never accepted as matched exposed records are returned as
#' `"UNMATCHED"`, including cases where the same person became unavailable because
#' they were matched first as a control.
#'
#' @note
#' This function implements matching without replacement only. It does not perform
#' bootstrap resampling. It is intended to be called through
#' `build_study_cohort(..., matching_mode = "without_replacement")`, although it can
#' also be used directly if the required intermediate objects and DuckDB connection
#' are already available.
#'
#' @examples
#' \dontrun{
#' matching_conn <- DBI::dbConnect(duckdb::duckdb(), "intermediate_outputs/matching.duckdb")
#'
#' D4_MSC <- match_cohorts_without_replacement(
#'   matching_pop_groupkey = D3_MATCHING_POP,
#'   matching_vars = matching_vars,
#'   profile_table = D3_LOOKUP_TABLE,
#'   matching_query = matching_query,
#'   matching_conn = matching_conn,
#'   save_output = FALSE
#' )
#'
#' DBI::dbDisconnect(matching_conn, shutdown = TRUE)
#' }
#'
#' @seealso [build_study_cohort()], [get_profile_table()], [get_matching_population()]
#'
#' @export
match_cohorts_without_replacement <- function(matching_pop_groupkey = NULL,
                                              matching_vars = NULL,
                                              profile_table = NULL,
                                              matching_query = NULL,
                                              matching_conn = NULL,
                                              result_dir = NULL,
                                              result_file = NULL,
                                              save_output = FALSE,
                                              n_cores = NULL,
                                              start_seed = 42,
                                              col_person_id = "person_id",
                                              col_match_id = "match_id",
                                              col_treatment_group = "group",
                                              col_T0 = "T0",
                                              col_matching_status_start = "matching_status_start",
                                              col_matching_status_end = "matching_status_end",
                                              col_age_iterator = "year_of_birth") {
  if (is.null(matching_query)) {
    stop("`matching_query` must be provided for `match_cohorts_without_replacement`.")
  }

  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    logr::log_print(paste0("The parameter `n_cores` was not specified. By default ", n_cores, " will be used in the SQL matching procedure."))
  }

  DBI::dbExecute(matching_conn, paste0("PRAGMA threads=", n_cores, ";"))

  logr::log_print("[MATCHING-NR] - Preparing no-replacement matching pool")

  pool_dt <- data.table::as.data.table(matching_pop_groupkey)[
    , .(
      person_id = as.character(get(col_person_id)),
      groupkey = as.integer(groupkey),
      year_of_birth = as.integer(get(col_age_iterator)),
      startdateINT = as.integer(as.Date(get(col_matching_status_start)) - as.Date("1970-01-01")),
      enddateINT = as.integer(as.Date(get(col_matching_status_end)) - as.Date("1970-01-01")),
      group = as.character(group)
    )
  ]

  pool_dt <- pool_dt[!is.na(person_id) & !is.na(groupkey) & !is.na(startdateINT) & !is.na(enddateINT)]
  pool_dt[, spell_id := .I]
  exposed_pool_all <- pool_dt[group == "exposed"]

  if (nrow(pool_dt[group == "exposed"]) == 0L) {
    logr::log_print("[MATCHING-NR] - No exposed spells in matching population")
    empty <- data.table::as.data.table(profile_table)[0]
    empty[, (col_person_id) := character()]
    empty[, (col_match_id) := integer()]
    empty[, boot_id := integer()]
    empty[, (col_treatment_group) := character()]
    empty[, (col_T0) := as.Date(character())]
    empty[, (col_matching_status_start) := as.Date(character())]
    empty[, (col_matching_status_end) := as.Date(character())]
    out_cols <- c(
      col_person_id, col_match_id, "boot_id", col_treatment_group,
      col_T0, col_matching_status_start, col_matching_status_end, matching_vars
    )
    out <- empty[, ..out_cols]
    if (isTRUE(save_output)) {
      output_file_path <- file.path(result_dir, paste0(result_file, ".parquet"))
      arrow::write_parquet(out, output_file_path)
    }
    return(out)
  }

  DBI::dbWriteTable(matching_conn, "pool_nr", as.data.frame(pool_dt), overwrite = TRUE, temporary = TRUE)
  DBI::dbExecute(matching_conn, "
    CREATE OR REPLACE TEMP TABLE person_state_nr AS
    SELECT DISTINCT person_id, TRUE AS available
    FROM pool_nr
  ")

  all_matches <- list()
  next_match_id <- 1L
  round_id <- 1L

  matching_query_adjusted <- gsub("__START_SEED__", as.character(as.integer(start_seed)), matching_query, fixed = TRUE)

  repeat {
    logr::log_print(paste0("[MATCHING-NR] - Round ", round_id, ": computing greedy proposals"))

    accepted_round <- DBI::dbGetQuery(matching_conn, matching_query_adjusted)
    accepted_round <- data.table::as.data.table(accepted_round)

    if (nrow(accepted_round) == 0L) {
      logr::log_print(paste0("[MATCHING-NR] - Round ", round_id, ": no further pairs found"))
      break
    }

    data.table::setorder(accepted_round, exposed_priority, exp_startdateINT, exp_spell_id)
    exp_ids <- accepted_round$exp_person_id
    ctrl_ids <- accepted_round$ctrl_person_id
    person_levels <- unique(c(exp_ids, ctrl_ids))
    exp_idx <- data.table::chmatch(exp_ids, person_levels)
    ctrl_idx <- data.table::chmatch(ctrl_ids, person_levels)
    used_people <- rep(FALSE, length(person_levels))
    keep_idx <- logical(nrow(accepted_round))

    for (i in seq_len(nrow(accepted_round))) {
      exp_i <- exp_idx[[i]]
      ctrl_i <- ctrl_idx[[i]]
      if (!used_people[[exp_i]] && !used_people[[ctrl_i]]) {
        keep_idx[[i]] <- TRUE
        used_people[[exp_i]] <- TRUE
        used_people[[ctrl_i]] <- TRUE
      }
    }
    accepted_round <- accepted_round[keep_idx]

    if (nrow(accepted_round) == 0L) {
      logr::log_print(paste0("[MATCHING-NR] - Round ", round_id, ": proposals dissolved by person-level tie-breaking"))
      break
    }

    accepted_round[, match_id := seq.int(next_match_id, next_match_id + .N - 1L)]
    next_match_id <- next_match_id + nrow(accepted_round)
    all_matches[[length(all_matches) + 1L]] <- accepted_round

    matched_persons <- data.table::data.table(
      person_id = unique(c(accepted_round$exp_person_id, accepted_round$ctrl_person_id))
    )

    DBI::dbWriteTable(matching_conn, "matched_persons_nr", as.data.frame(matched_persons), overwrite = TRUE, temporary = TRUE)
    DBI::dbExecute(matching_conn, "
      UPDATE person_state_nr
      SET available = FALSE
      WHERE person_id IN (SELECT person_id FROM matched_persons_nr)
    ")

    round_id <- round_id + 1L
  }

  matched_pairs <- if (length(all_matches) > 0L) {
    data.table::rbindlist(all_matches, use.names = TRUE)
  } else {
    data.table::data.table(
      exp_spell_id = integer(), exp_person_id = character(), groupkey = integer(),
      exp_year_of_birth = integer(), exp_startdateINT = integer(), exp_enddateINT = integer(),
      exposed_priority = integer(), ctrl_spell_id = integer(), ctrl_person_id = character(),
      ctrl_startdateINT = integer(), ctrl_enddateINT = integer(), match_id = integer()
    )
  }

  origin_date <- as.Date("1970-01-01")
  profile_dt <- data.table::as.data.table(profile_table)

  exposed_matched <- matched_pairs[
    , .(
      person_id = exp_person_id,
      groupkey = groupkey,
      match_id = match_id,
      boot_id = 1L,
      group = "EXPOSED",
      T0 = exp_startdateINT + origin_date,
      matching_status_start = exp_startdateINT + origin_date,
      matching_status_end = exp_enddateINT + origin_date
    )
  ]

  control_matched <- matched_pairs[
    , .(
      person_id = ctrl_person_id,
      groupkey = groupkey,
      match_id = match_id,
      boot_id = 1L,
      group = "CONTROL",
      T0 = exp_startdateINT + origin_date,
      matching_status_start = ctrl_startdateINT + origin_date,
      matching_status_end = ctrl_enddateINT + origin_date
    )
  ]

  matched_exposed_spell_ids <- unique(matched_pairs$exp_spell_id)
  exposed_unmatched <- exposed_pool_all[
    !spell_id %in% matched_exposed_spell_ids,
    .(
      person_id = person_id,
      groupkey = groupkey,
      match_id = NA_integer_,
      boot_id = 1L,
      group = "UNMATCHED",
      T0 = startdateINT + origin_date,
      matching_status_start = startdateINT + origin_date,
      matching_status_end = enddateINT + origin_date
    )
  ]

  match_results_long <- data.table::rbindlist(
    list(exposed_matched, control_matched, exposed_unmatched),
    use.names = TRUE,
    fill = TRUE
  )

  match_results_long <- profile_dt[
    match_results_long,
    on = "groupkey"
  ]

  data.table::setnames(match_results_long,
    old = c(
      "person_id", "match_id", "group", "T0",
      "matching_status_start", "matching_status_end"
    ),
    new = c(
      col_person_id, col_match_id, col_treatment_group, col_T0,
      col_matching_status_start, col_matching_status_end
    ),
    skip_absent = TRUE
  )

  out_cols <- c(
    col_person_id, col_match_id, "boot_id", col_treatment_group,
    col_T0, col_matching_status_start, col_matching_status_end, matching_vars
  )
  out_cols <- out_cols[out_cols %in% colnames(match_results_long)]
  match_results_long <- match_results_long[, ..out_cols]

  if (isTRUE(save_output)) {
    output_file_path <- file.path(result_dir, paste0(result_file, ".parquet"))
    logr::log_print(paste0("[MATCHING-NR] - Saving ", output_file_path, " to disk..."))
    arrow::write_parquet(match_results_long, output_file_path)
    logr::log_print(paste0("[MATCHING-NR] - ", output_file_path, " saved to disk successfully."))
  }

  logr::log_print("[MATCHING-NR] - END")

  if (!isTRUE(save_output)) {
    return(match_results_long)
  }
}


