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
  # Load packaged default SQL when query is not provided
  if (is_empty_query(matching_query)) {
    default_matching_query_file <- "matching_query_without_replacement.sql"
    default_matching_query_path <- system.file("sql_queries", default_matching_query_file, package = "CohortBuilder")

    if (!nzchar(default_matching_query_path)) {
      stop(paste0("Could not locate default SQL query file: ", default_matching_query_file))
    }

    matching_query <- getSQL(default_matching_query_path)
    msg <- paste0("`matching_query` not specified. Falling back to packaged default: ", default_matching_query_file, ".")
    message(msg)
    logr::log_print(paste0("[MATCHING-NR] - ", msg))
  }

  # Configure number of CPU threads for DuckDB
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    logr::log_print(paste0("The parameter `n_cores` was not specified. By default ", n_cores, " will be used in the SQL matching procedure."))
  }

  # Set DuckDB thread configuration
  DBI::dbExecute(matching_conn, paste0("PRAGMA threads=", n_cores, ";"))

  # Begin pool preparation for greedy matching
  logr::log_print("[MATCHING-NR] - Preparing no-replacement matching pool")

  # Transform matching population to internal format with integer date encoding
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

  # Remove records with missing critical matching fields and assign spell identifiers
  pool_dt <- pool_dt[!is.na(person_id) & !is.na(groupkey) & !is.na(startdateINT) & !is.na(enddateINT)]
  pool_dt[, spell_id := .I]
  # Extract and store all exposed spells for later identification of unmatched records
  exposed_pool_all <- pool_dt[group == "exposed"]

  # Handle edge case: no exposed spells available for matching
  if (nrow(pool_dt[group == "exposed"]) == 0L) {
    logr::log_print("[MATCHING-NR] - No exposed spells in matching population")
    # Create empty output with correct structure but no rows
    empty <- data.table::as.data.table(profile_table)[0]
    empty[, (col_person_id) := character()]
    empty[, (col_match_id) := integer()]
    empty[, boot_id := integer()]
    empty[, (col_treatment_group) := character()]
    empty[, (col_T0) := as.Date(character())]
    empty[, (col_matching_status_start) := as.Date(character())]
    empty[, (col_matching_status_end) := as.Date(character())]
    empty[, (col_age_iterator) := integer()]
    out_cols <- c(
      col_person_id, col_match_id, "boot_id", col_treatment_group,
      col_T0, col_matching_status_start, col_matching_status_end, col_age_iterator, matching_vars
    )
    out_cols <- unique(out_cols)
    out <- empty[, ..out_cols]
    # Optionally save empty result to disk
    if (isTRUE(save_output)) {
      output_file_path <- file.path(result_dir, paste0(result_file, ".parquet"))
      arrow::write_parquet(out, output_file_path)
    }
    return(out)
  }

  # Load matching pool into DuckDB temporary tables for SQL-based candidate generation
  DBI::dbWriteTable(matching_conn, "pool_nr", as.data.frame(pool_dt), overwrite = TRUE, temporary = TRUE)
  # Initialize person availability state table (marks people as available for matching)
  DBI::dbExecute(matching_conn, "
    CREATE OR REPLACE TEMP TABLE person_state_nr AS
    SELECT DISTINCT person_id, TRUE AS available
    FROM pool_nr
  ")

  # Initialize greedy matching state and tracking variables
  all_matches <- list()
  next_match_id <- 1L
  round_id <- 1L

  # Replace seed placeholder in matching query for deterministic reproducibility
  matching_query_adjusted <- gsub("__START_SEED__", as.character(as.integer(start_seed)), matching_query, fixed = TRUE)

  # Begin greedy matching rounds: continue until no more pairs can be formed
  repeat {
    # Generate candidate exposed-control pairs for current pool of available people
    logr::log_print(paste0("[MATCHING-NR] - Round ", round_id, ": computing greedy proposals"))

    accepted_round <- DBI::dbGetQuery(matching_conn, matching_query_adjusted)
    accepted_round <- data.table::as.data.table(accepted_round)

    # Exit matching loop when no more candidate pairs are available
    if (nrow(accepted_round) == 0L) {
      logr::log_print(paste0("[MATCHING-NR] - Round ", round_id, ": no further pairs found"))
      break
    }

    # Sort candidates by exposed spell priority and start date for deterministic ordering
    data.table::setorder(accepted_round, exposed_priority, exp_startdateINT, exp_spell_id)
    # Extract person identifiers and build reverse lookup for collision detection
    exp_ids <- accepted_round$exp_person_id
    ctrl_ids <- accepted_round$ctrl_person_id
    person_levels <- unique(c(exp_ids, ctrl_ids))
    exp_idx <- data.table::chmatch(exp_ids, person_levels)
    ctrl_idx <- data.table::chmatch(ctrl_ids, person_levels)
    # Track which people have already been assigned in this round
    used_people <- rep(FALSE, length(person_levels))
    keep_idx <- logical(nrow(accepted_round))

    # Resolve collisions: ensure each person appears at most once per round
    for (i in seq_len(nrow(accepted_round))) {
      cat(sprintf("\rChecking matched pairs for duplicates, %d%% ready...", round((i / nrow(accepted_round)) * 100)))

      exp_i <- exp_idx[[i]]
      ctrl_i <- ctrl_idx[[i]]
      # Keep pair only if both exposed and control are still available in this round
      if (!used_people[[exp_i]] && !used_people[[ctrl_i]]) {
        keep_idx[[i]] <- TRUE
        used_people[[exp_i]] <- TRUE
        used_people[[ctrl_i]] <- TRUE
      }
    }
    # Filter to non-colliding pairs only
    accepted_round <- accepted_round[keep_idx]

    # Exit if all candidate pairs dissolved due to collisions
    if (nrow(accepted_round) == 0L) {
      logr::log_print(paste0("[MATCHING-NR] - Round ", round_id, ": proposals dissolved by person-level tie-breaking"))
      break
    }

    # Assign unique match identifiers to accepted pairs and store results
    accepted_round[, match_id := seq.int(next_match_id, next_match_id + .N - 1L)]
    next_match_id <- next_match_id + nrow(accepted_round)
    all_matches[[length(all_matches) + 1L]] <- accepted_round

    # Extract all matched people (both exposed and control) from this round
    matched_persons <- data.table::data.table(
      person_id = unique(c(accepted_round$exp_person_id, accepted_round$ctrl_person_id))
    )

    # Update person availability state: mark matched people as unavailable for future rounds
    DBI::dbWriteTable(matching_conn, "matched_persons_nr", as.data.frame(matched_persons), overwrite = TRUE, temporary = TRUE)
    DBI::dbExecute(matching_conn, "
      UPDATE person_state_nr
      SET available = FALSE
      WHERE person_id IN (SELECT person_id FROM matched_persons_nr)
    ")

    # Increment round counter and continue to next iteration
    round_id <- round_id + 1L
  }

  # Combine all matched pairs from all rounds, or create empty structure if no matches
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

  # Convert integer date encoding back to calendar dates for output
  origin_date <- as.Date("1970-01-01")
  profile_dt <- data.table::as.data.table(profile_table)

  # Extract matched exposed spells with dates decoded and group labeled as "EXPOSED"
  exposed_matched <- matched_pairs[
    , .(
      person_id = exp_person_id,
      groupkey = groupkey,
      match_id = match_id,
      boot_id = 0L,
      group = "EXPOSED",
      T0 = exp_startdateINT + origin_date,
      matching_status_start = exp_startdateINT + origin_date,
      matching_status_end = exp_enddateINT + origin_date
    )
  ]

  # Extract matched control spells with dates decoded and group labeled as "CONTROL"
  control_matched <- matched_pairs[
    , .(
      person_id = ctrl_person_id,
      groupkey = groupkey,
      match_id = match_id,
      boot_id = 0L,
      group = "CONTROL",
      T0 = exp_startdateINT + origin_date,
      matching_status_start = ctrl_startdateINT + origin_date,
      matching_status_end = ctrl_enddateINT + origin_date
    )
  ]

  # Identify all exposed spells that were never successfully matched
  matched_exposed_spell_ids <- unique(matched_pairs$exp_spell_id)
  # Format unmatched exposed records with NA match_id and "UNMATCHED" group label
  exposed_unmatched <- exposed_pool_all[
    !spell_id %in% matched_exposed_spell_ids,
    .(
      person_id = person_id,
      groupkey = groupkey,
      match_id = NA_integer_,
      boot_id = 0L,
      group = "UNMATCHED",
      T0 = startdateINT + origin_date,
      matching_status_start = startdateINT + origin_date,
      matching_status_end = enddateINT + origin_date
    )
  ]

  # Combine all matched and unmatched records into one output table
  match_results_long <- data.table::rbindlist(
    list(exposed_matched, control_matched, exposed_unmatched),
    use.names = TRUE,
    fill = TRUE
  )

  # Left join profile data (matching variables) back to results by groupkey
  match_results_long <- profile_dt[
    match_results_long,
    on = "groupkey"
  ]

  # Rename internal column names to user-specified output column names
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

  # Ensure the requested age iterator column is always available in output
  age_lookup <- data.table::as.data.table(matching_pop_groupkey)[
    , .(
      age_value = {
        vals <- as.integer(get(col_age_iterator))
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0L) vals[1L] else NA_integer_
      }
    ),
    by = .(person_id = as.character(get(col_person_id)))
  ]
  age_idx <- match(match_results_long[[col_person_id]], age_lookup$person_id)
  match_results_long[, (col_age_iterator) := age_lookup$age_value[age_idx]]

  # Select and order final output columns in desired sequence
  out_cols <- c(
    col_person_id, col_match_id, "boot_id", col_treatment_group,
    col_T0, col_matching_status_start, col_matching_status_end, col_age_iterator, matching_vars
  )
  out_cols <- unique(out_cols)
  out_cols <- out_cols[out_cols %in% colnames(match_results_long)]
  match_results_long <- match_results_long[, ..out_cols]

  # Optionally save results to parquet file on disk
  if (isTRUE(save_output)) {
    output_file_path <- file.path(result_dir, paste0(result_file, ".parquet"))
    logr::log_print(paste0("[MATCHING-NR] - Saving ", output_file_path, " to disk..."))
    arrow::write_parquet(match_results_long, output_file_path)
    logr::log_print(paste0("[MATCHING-NR] - ", output_file_path, " saved to disk successfully."))
  }

  # Log completion of matching procedure
  logr::log_print("[MATCHING-NR] - END")

  # Return results to environment if not saving to disk
  if (!isTRUE(save_output)) {
    class(match_results_long) <- unique(c("matching_study_cohort", class(match_results_long)))
    return(match_results_long)
  }
}
