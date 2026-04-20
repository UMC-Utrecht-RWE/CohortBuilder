# Match cohorts (with replacement / bootstrap) ----------------------------

#' Match Cohorts with Replacement and Optional Bootstrapping
#'
#' This function performs matching between exposed and control populations within a study cohort,
#' with optional bootstrapping to account for variability in the results. Matching is performed
#' using a provided SQL query, leveraging DuckDB for efficient data handling.
#'
#' @param matching_pop_groupkey A data frame containing the eligible population to be matched, with a grouping key.
#' @param matching_vars A character vector of variable names to use in the matching process.
#' @param profile_table A data frame containing additional profile information for the population.
#' @param matching_query A SQL query string defining the matching logic.
#' @param target_table_query A SQL query string to create the target table in DuckDB.
#' @param matching_conn A DuckDB database connection object to facilitate SQL execution.
#' @param result_dir A string specifying the directory to save the final matched population results.
#' @param result_file A string specifying the file name for the final matched population results.
#' @param save_output A logical value. If `TRUE`, the matched results are saved to the specified file.
#' @param dir_bootstrap A string specifying the directory to save bootstrap results, if applicable.
#' @param with_bootstrap A logical value. If `TRUE`, bootstrapping is performed on the matching process.
#' @param n_bootstraps An integer specifying the number of bootstrap iterations. Defaults to 500.
#' @param n_cores An integer specifying the number of cores to use for SQL execution. Defaults to the total cores minus one.
#' @param start_seed An integer specifying the starting seed for reproducibility in bootstrapping. Defaults to 42.
#' @param col_person_id A string specifying the column name for the unique person identifier. Defaults to `"person_id"`.
#' @param col_match_id A string specifying the column name for the match identifier. Defaults to `"match_id"`.
#' @param col_treatment_group A string specifying the column name for the treatment group identifier. Defaults to `"group"`.
#' @param col_T0 A string specifying the column name for the treatment initiation time. Defaults to `"T0"`.
#' @param col_matching_status_start A string specifying the column name for the start date of the matching status. Defaults to `"matching_status_start"`.
#' @param col_matching_status_end A string specifying the column name for the end date of the matching status. Defaults to `"matching_status_end"`.
#' @param col_age_iterator A string specifying the column name for the year of birth or age iterator. Defaults to `"year_of_birth"`.
#'
#' @return If `with_bootstrap` is `FALSE` and `save_output` is `FALSE`, the function returns a data.table of the matched population.
#' If `save_output` is `TRUE`, the results are saved to the specified file.
#'
#' @details
#' - The function begins by configuring the DuckDB connection and setting the number of threads for execution.
#' - If bootstrapping is enabled, the population is resampled with replacement in each iteration.
#' - Matching is performed via a provided SQL query, adjusted for each year of birth or other specified iterators.
#' - Results are saved either to a specified directory for bootstrapping or as a single output for the matched population.
#' - Temporary tables and data structures are cleaned up at the end of each iteration to manage memory usage.
#'
#' @note If `with_bootstrap` is `FALSE`, only one iteration is performed, overriding `n_bootstraps` if specified.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
match_cohorts_with_replacement <- function(matching_pop_groupkey = NULL,
                                           matching_vars = NULL,
                                           profile_table = NULL,
                                           matching_query = NULL,
                                           target_table_query = NULL,
                                           matching_conn = NULL,
                                           result_dir = NULL,
                                           result_file = NULL,
                                           save_output = FALSE,
                                           dir_bootstrap = NULL,
                                           with_bootstrap = FALSE,
                                           n_bootstraps = 500,
                                           n_cores = NULL,
                                           start_seed = 42,
                                           col_person_id = "person_id",
                                           col_match_id = "match_id",
                                           col_treatment_group = "group",
                                           col_T0 = "T0",
                                           col_matching_status_start = "matching_status_start",
                                           col_matching_status_end = "matching_status_end",
                                           col_age_iterator = "year_of_birth") {
  # Configure DuckDB connection and thread count for SQL execution
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    logr::log_print(paste0("The parameter `n_cores` was not specified. By default ", n_cores, " will be used in the SQL matching procedure."))
  }

  # Set DuckDB thread configuration
  DBI::dbExecute(matching_conn, paste0("PRAGMA threads=", n_cores, ";"))

  # Initialize matching target table for storing results
  DBI::dbExecute(matching_conn, target_table_query)

  # Override bootstrap iterations when bootstrapping is disabled
  if (!with_bootstrap) {
    if (is.null(n_bootstraps)) {
      n_bootstraps <- 1
    }

    # Force single iteration if bootstrap is disabled
    if (n_bootstraps != 1) {
      logr::log_print(paste0("The parameter `n_bootstraps` was overruled to 1 since no bootstrap will be done (`with_bootstrap` = FALSE)"))
    }
    n_bootstraps <- 1
  }

  # Begin main bootstrap loop: iterate matching process with optional resampling
  for (bootstrap in 1:n_bootstraps) {
    tictoc::tic()

    # Log current bootstrap iteration progress
    logr::log_print(paste0("Doing iteration ", bootstrap, " of ", n_bootstraps, ".... \n"))

    # Conditional resampling with replacement if bootstrapping is enabled
    if (with_bootstrap) {
      set.seed(start_seed + bootstrap)

      cat(sprintf("\r%-50s", "Sampling...."))
      # Sample unique person IDs with replacement for bootstrap iteration
      sampled_ids <- matching_pop_groupkey[
        , .(person_id = unique(get(col_person_id)))
      ][
        , .(person_id = sample(person_id, .N, replace = TRUE))
      ][
        order(person_id)
      ]

      # Build dynamic join condition using specified person_id column name
      join_condition <- setNames("person_id", col_person_id)

      cat(sprintf("\r%-50s", "Sampling back to the source population...."))
      # Join resampled person IDs back to get all their records
      sampled_df <- matching_pop_groupkey[sampled_ids, on = join_condition]

      rm(sampled_ids, join_condition)
      gc()
    } else {
      # No bootstrap: use original population as-is for single iteration
      cat(sprintf("\r%-50s", "No resampling; the source population will be used...."))
      sampled_df <- matching_pop_groupkey

      gc()
    }

    # Convert all matching variables to integer format for efficient SQL processing
    sampled_df[, person_id_int := as.integer(factor(person_id))]
    sampled_df[, startdateINT := as.integer(as.Date(get(col_matching_status_start)) - as.Date("1970-01-01"))]
    sampled_df[, enddateINT := as.integer(as.Date(get(col_matching_status_end)) - as.Date("1970-01-01"))]
    sampled_df[, year_of_birth := as.integer(get(col_age_iterator))]
    sampled_df[, groupkey := as.integer(groupkey)]

    # Determine year-of-birth range for year-wise matching iterations
    min_year <- min(sampled_df$year_of_birth, na.rm = TRUE)
    max_year <- max(sampled_df$year_of_birth, na.rm = TRUE)

    # Extract exposed population and assign random values for stochastic matching
    cat(sprintf("\r%-50s", "Selecting the exposed population...."))
    sampled_df_Exp <- sampled_df[
      group == "exposed",
      .(
        person_id, person_id_int, groupkey, group, get(col_matching_status_start), get(col_matching_status_end),
        year_of_birth, startdateINT, enddateINT
      )
    ]
    sampled_df_Exp[, random := runif(.N, min = 0, max = 10)]
    sampled_df_Exp[, match_id := .I]

    # Extract control population and assign random values for stochastic matching
    cat(sprintf("\r%-50s", "Selecting the unexposed population...."))
    sampled_df_Un <- sampled_df[
      group == "control",
      .(
        person_id, groupkey, group, get(col_matching_status_start), get(col_matching_status_end),
        year_of_birth, startdateINT, enddateINT
      )
    ]
    sampled_df_Un[, random := runif(.N, min = 0, max = 10)]

    rm(sampled_df)
    gc()

    # Load exposed and control populations into DuckDB temporary tables for SQL matching
    df_bootstrap <- data.frame(boot_id = bootstrap)

    cat(sprintf("\r%-50s", "Sending data to DuckDB...."))
    # Write exposed population to DuckDB
    DBI::dbWriteTable(matching_conn, DBI::SQL("dfexp"), sampled_df_Exp, overwrite = TRUE, temporary = TRUE)
    # Write control population to DuckDB
    DBI::dbWriteTable(matching_conn, DBI::SQL("dfun"), sampled_df_Un, overwrite = TRUE, temporary = TRUE)
    # Write bootstrap iteration ID to DuckDB
    DBI::dbWriteTable(matching_conn, DBI::SQL("dfbootstrap"), as.data.frame(df_bootstrap), overwrite = TRUE)

    rm(sampled_df_Exp, sampled_df_Un)
    gc()

    # Execute matching query for each year-of-birth stratum
    for (year in min_year:max_year) {
      cat(sprintf("\rDoing year_of_birth loops, %d%% ready....", round(((year - min_year) / (max_year - min_year)) * 100)))

      # Customize matching query for current year-of-birth stratum
      matching_query_adjusted <- gsub(
        "year_of_birth = 1920",
        paste0("year_of_birth = ", year),
        matching_query
      )
      # Adjust age tolerance window for year-of-birth matching
      matching_query_adjusted <- gsub(
        "year_of_birth BETWEEN 1919 AND 1921",
        paste0("year_of_birth BETWEEN ", year - 1, " AND ", year + 1),
        matching_query_adjusted
      )

      # Modify join type if bootstrap enabled: ensure person used matches once only
      if (with_bootstrap) {
        matching_query_adjusted <- gsub(
          "LEFT JOIN matched_persons M",
          "INNER JOIN matched_persons M",
          matching_query_adjusted
        )
      }

      # Execute adjusted matching query and store results in DuckDB
      DBI::dbExecute(matching_conn, matching_query_adjusted)
    }

    # Log round completion time
    toc_log_print()
  }

  # Signal end of bootstrap iterations
  logr::log_print(c("[MATCHING] - END"))

  # Read all matching results from DuckDB back into R for post-processing
  logr::log_print("[MATCHING] - Reading matching dataset back into R")

  match_results <- DBI::dbReadTable(matching_conn, "match_result")
  data.table::setDT(match_results)

  # Convert integer date encoding back to calendar dates
  origin_date <- as.Date("1970-01-01")

  # Join results with profile table and decode dates, assign match IDs
  match_results_rebuilt <- match_results[
    profile_table,
    on = "groupkey",
    nomatch = 0
  ][
    ,
    `:=`(
      T0                  = startdateINT_exposed + origin_date,
      startdate_exposed   = startdateINT_exposed + origin_date,
      startdate_unexposed = startdateINT_unexposed + origin_date,
      enddate_exposed     = enddateINT_exposed + origin_date,
      enddate_unexposed   = enddateINT_unexposed + origin_date,
      match_id            = .I
    )
  ][
    ,
    !c(
      "groupkey",
      "startdateINT_exposed", "enddateINT_exposed",
      "startdateINT_unexposed", "enddateINT_unexposed"
    ),
    with = FALSE
  ]

  # Format exposed records with treatment group labels (EXPOSED or UNMATCHED)
  match_results_rebuilt_long_exposed <- data.table::copy(match_results_rebuilt)[
    ,
    (col_treatment_group) := data.table::fifelse(
      matched_exposed == 1, "EXPOSED",
      data.table::fifelse(matched_exposed == 0, "UNMATCHED", NA_character_)
    )
  ][
    ,
    !c("idun", "matched_exposed", "startdate_unexposed", "enddate_unexposed"),
    with = FALSE
  ]

  # Format control records with treatment group label as CONTROL (only matched controls)
  match_results_rebuilt_long_control <- match_results_rebuilt[
    !is.na(idun)
  ][
    ,
    (col_treatment_group) := "CONTROL"
  ][
    ,
    !c("idexp", "matched_exposed", "startdate_exposed", "enddate_exposed"),
    with = FALSE
  ]

  # Combine exposed and control records into long format output
  match_results_rebuilt_long <- data.table::rbindlist(
    list(
      match_results_rebuilt_long_exposed,
      match_results_rebuilt_long_control
    ),
    use.names = TRUE,
    fill = TRUE
  )[
    ,
    # Merge date columns from both groups and extract appropriate person ID
    c(
      col_matching_status_start,
      col_matching_status_end,
      col_person_id
    ) :=
      .(
        data.table::fcoalesce(startdate_exposed, startdate_unexposed),
        data.table::fcoalesce(enddate_exposed, enddate_unexposed),
        data.table::fcoalesce(idexp, idun)
      )
  ][
    ,
    # Drop intermediate person-id columns from wide representation
    !c("idexp", "idun"),
    with = FALSE
  ]

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
  age_idx <- match(match_results_rebuilt_long[[col_person_id]], age_lookup$person_id)
  match_results_rebuilt_long[, (col_age_iterator) := age_lookup$age_value[age_idx]]

  # Select and order final output columns
  out_cols <- c(
    col_person_id, col_match_id, "boot_id", col_T0, col_treatment_group,
    col_matching_status_start, col_matching_status_end, col_age_iterator,
    matching_vars
  )
  out_cols <- unique(out_cols)
  out_cols <- out_cols[out_cols %in% colnames(match_results_rebuilt_long)]
  match_results_rebuilt_long <- match_results_rebuilt_long[, ..out_cols]

  logr::log_print("[MATCHING] - Done reading matching dataset back into R")

  # Optionally save results to parquet file on disk
  if (save_output) {
    output_file_path <- if (!with_bootstrap) {
      file.path(result_dir, paste0(result_file, ".parquet"))
    }
    logr::log_print(paste0("[MATCHING] - Saving ", output_file_path, " to disk..."))
    arrow::write_parquet(match_results_rebuilt_long, output_file_path)
    logr::log_print(paste0("[MATCHING] - ", output_file_path, " saved to disk successfully."))
  }

  # Return results if not bootstrapping and not saving to disk
  if (!with_bootstrap && !save_output) {
    logr::log_print("[MATCHING] `save_output` set to FALSE, returning matching results")
    return(match_results_rebuilt_long)
  }
}
