# Match cohorts (with replacement / bootstrap) ----------------------------

#' Match Cohorts with Replacement and Optional Bootstrapping
#'
#' This function performs matching between exposed and control populations within a study cohort,
#' with optional bootstrapping to account for variability in the results. Matching is performed
#' using a provided SQL query, leveraging DuckDB for efficient data handling. Matching is done by batches defined by unique values of col_age_iterator (by default year of birth) of the exposed which is assumed numeric.
#' Candidate control matches are selected based on the profile table, and having col_age_iterator value between col_age_iterator - age_offset and col_age_iterator + age_offset.
#' If no range matching on col_age_iterator is required, set age_offset to NULL. In that case, all candidate controls are selected
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
#' @param col_age_iterator A string specifying the column name for the (numeric) year of birth or age iterator. Defaults to `"year_of_birth"`.
#' @param age_offset A numeric value specifying the range of values of col_age_iterator with which to select candidate controls. Defaults to 1. If no range-matching on this variable required, user should set to NULL.
#' @param col_date_match A character vector of date column names for range matching. Defaults to `NULL`.
#' @param date_match_offsets A named integer vector specifying the offset (in days) for each date column in `col_date_match`.
#'   Names must match the column names. Defaults to `NULL`.
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
                                           col_age_iterator = "year_of_birth",
                                           age_offset = 1,
                                           col_date_match = NULL,
                                           date_match_offsets = NULL,
                                           range_match = NULL) {
  if (is.numeric(age_offset)) {
    msg <- paste("Matching based on profile and", col_age_iterator, "exposed between", col_age_iterator, "+/-", age_offset)
    logger::log_info(paste0("[MATCHING] - ", msg))
  }
  if (is.null(age_offset)) {
    msg <- paste("Matching based on profile only. Iterating batches by", col_age_iterator, "exposed")
    logger::log_info(paste0("[MATCHING] - ", msg))
  }
  if (!(is.null(age_offset) || is.numeric(age_offset))) {
    stop("age_offset must be 1 or NULL")
  }

  # Validate and prepare date matching conditions
  if (!is.null(col_date_match)) {
    # Validate that all date INT columns exist in matching_pop_groupkey (added by get_matching_population)
    expected_int_cols <- paste0(col_date_match, "_int")
    missing_cols <- setdiff(expected_int_cols, names(matching_pop_groupkey))
    if (length(missing_cols) > 0) {
      logger::log_error(paste0("[MATCHING] - The following date match columns are not found in the data: ", paste(gsub("_int$", "", missing_cols), collapse = ", ")))
      stop("Date match columns not found: ", paste(gsub("_int$", "", missing_cols), collapse = ", "))
    }

    # Validate that date_match_offsets has names matching col_date_match
    if (is.null(date_match_offsets)) {
      logger::log_error("[MATCHING] - date_match_offsets must be provided when col_date_match is specified")
      stop("date_match_offsets must be provided when col_date_match is specified")
    }

    if (!all(col_date_match %in% names(date_match_offsets))) {
      logger::log_error(paste0("[MATCHING] - date_match_offsets must have names matching col_date_match. Expected: ", paste(col_date_match, collapse = ", ")))
      stop("date_match_offsets must have names matching col_date_match")
    }
  }

  # Load packaged defaults when SQL queries are not provided
  if (is_empty_query(matching_query)) {
    default_matching_query_file <- "matching_query_with_replacement.sql"
    default_matching_query_path <- system.file("sql_queries", default_matching_query_file, package = "CohortBuilder")

    if (!nzchar(default_matching_query_path)) {
      stop(paste0("Could not locate default SQL query file: ", default_matching_query_file))
    }

    matching_query <- getSQL(default_matching_query_path)
    msg <- paste0("`matching_query` not specified. Falling back to packaged default: ", default_matching_query_file, ".")
    message(msg)
    logger::log_info(paste0("[MATCHING] - ", msg))
  }

  if (is_empty_query(target_table_query)) {
    default_target_table_query_file <- "create_matching_target_table.sql"
    default_target_table_query_path <- system.file("sql_queries", default_target_table_query_file, package = "CohortBuilder")

    if (!nzchar(default_target_table_query_path)) {
      stop(paste0("Could not locate default target-table SQL file: ", default_target_table_query_file))
    }

    target_table_query <- getSQL(default_target_table_query_path)
    msg <- paste0("`target_table_query` not specified. Falling back to packaged default: ", default_target_table_query_file, ".")
    message(msg)
    logger::log_info(paste0("[MATCHING] - ", msg))
  }

  # Configure DuckDB connection and thread count for SQL execution
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    logger::log_info(paste0("The parameter `n_cores` was not specified. By default ", n_cores, " will be used in the SQL matching procedure."))
  }

  # Build dynamic SQL date matching conditions
  date_match_sql_conditions <- ""
  if (!is.null(col_date_match)) {
    date_conditions <- character()
    for (i in seq_along(col_date_match)) {
      date_col <- col_date_match[i]
      date_col_int <- paste0(date_col, "_int")
      offset <- date_match_offsets[[date_col]]
      # Build condition: if exposed date is NULL skip check; otherwise control must be within offset
      # NULL BETWEEN x AND y = NULL (false) in SQL, so we need IS NULL escape
      condition <- paste0(
        "AND (E.", date_col_int, " IS NULL OR U.", date_col_int,
        " BETWEEN E.", date_col_int, " - ", offset,
        " AND E.", date_col_int, " + ", offset, ")"
      )
      date_conditions <- c(date_conditions, condition)
    }
    date_match_sql_conditions <- paste(date_conditions, collapse = "\n            ")
    logger::log_info(paste0("[MATCHING] - Date range matching enabled for: ", paste(col_date_match, collapse = ", ")))
  }


  # Build dynamic SQL spell offset matching conditions
  range_match_sql_conditions <- ""
  if (range_match == TRUE) {

    # Original condition: exact spell match
    range_match_sql_conditions <- "AND E.startdateINT BETWEEN U.startdateINT_ext AND U.enddateINT_ext"
    logger::log_info(paste0("[MATCHING] - Range matching enabled with offset: ", date_match_offsets, " days"))

  } else {
    # No offset: just the basic spell match
    range_match_sql_conditions <- "AND E.startdateINT BETWEEN U.startdateINT AND U.enddateINT"
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
      logger::log_info(paste0("The parameter `n_bootstraps` was overruled to 1 since no bootstrap will be done (`with_bootstrap` = FALSE)"))
    }
    n_bootstraps <- 1
  }

  # Begin main bootstrap loop: iterate matching process with optional resampling
  for (bootstrap in 1:n_bootstraps) {
    tictoc::tic()

    # If no bootstrapping, set bootstrap iteration to 0
    if (!with_bootstrap) {
      bootstrap <- 0
    }

    # Log current bootstrap iteration progress
    logger::log_info(paste0("Doing iteration ", bootstrap, " of ", n_bootstraps, ".... \n"))

    # Conditional resampling with replacement if bootstrapping is enabled
    if (with_bootstrap) {
      set.seed(start_seed + bootstrap)

      cat(sprintf("\r%-50s", "Sampling...."))
      # Sample unique person IDs with replacement for bootstrap iteration
      sampled_ids <- matching_pop_groupkey[
        , .(id = unique(get(col_person_id)))
      ][
        , .(id = sample(get(col_person_id), .N, replace = TRUE))
      ][
        order(id)
      ]

      # Build dynamic join condition using specified person_id column name
      join_condition <- setNames("id", col_person_id)

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
    sampled_df[, id_int := as.integer(factor(get(col_person_id)))]
    sampled_df[, startdateINT := as.integer(as.Date(get(col_matching_status_start)) - as.Date("1970-01-01"))]
    sampled_df[, enddateINT := as.integer(as.Date(get(col_matching_status_end)) - as.Date("1970-01-01"))]
    # Also calcualte additional windows to prepare range matching
    sampled_df[group == "control",
               startdateINT_ext := startdateINT - date_match_offsets]
    sampled_df[group == "control",
               enddateINT_ext := enddateINT + date_match_offsets]

    sampled_df[, year_of_birth := as.integer(get(col_age_iterator))]
    sampled_df[, groupkey := as.integer(groupkey)]

    # Ensure date INT columns are integer type (already computed in get_matching_population)
    if (!is.null(col_date_match)) {
      for (date_col in col_date_match) {
        date_col_int <- paste0(date_col, "_int")
        sampled_df[, (date_col_int) := as.integer(get(date_col_int))]
      }
    }

    # Determine year-of-birth range for year-wise matching iterations
    min_year <- min(sampled_df$year_of_birth, na.rm = TRUE)
    max_year <- max(sampled_df$year_of_birth, na.rm = TRUE)

    # Extract exposed population and assign random values for stochastic matching
    cat(sprintf("\r%-50s", "Selecting the exposed population...."))
    exp_cols <- c(col_person_id, "id_int", "groupkey", "group",
                  "startdateINT","enddateINT",
                  "startdateINT_ext","enddateINT_ext",
                  "year_of_birth")
    #if (!is.null(col_date_match)) exp_cols <- c(exp_cols, paste0(col_date_match, "_int"))
    sampled_df_Exp <- sampled_df[group == "exposed", exp_cols, with = FALSE]
    sampled_df_Exp[, random := runif(.N, min = 0, max = 10)]
    sampled_df_Exp[, match_id := .I]

    # Extract control population and assign random values for stochastic matching
    cat(sprintf("\r%-50s", "Selecting the unexposed population...."))
    un_cols <- c(col_person_id, "groupkey", "group",
                 "startdateINT", "enddateINT",
                 "startdateINT_ext","enddateINT_ext",
                 "year_of_birth")
    #if (!is.null(col_date_match)) un_cols <- c(un_cols, paste0(col_date_match, "_int"))
    sampled_df_Un <- sampled_df[group == "control", un_cols, with = FALSE]
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
      if (is.null(age_offset)) {
        matching_query_adjusted <- gsub(
          "year_of_birth BETWEEN 1919 AND 1921",
          paste0("year_of_birth BETWEEN ", min_year, " AND ", max_year),
          matching_query_adjusted
        )
      } else {
        matching_query_adjusted <- gsub(
          "year_of_birth BETWEEN 1919 AND 1921",
          paste0("year_of_birth BETWEEN ", year - age_offset, " AND ", year + age_offset),
          matching_query_adjusted
        )
      }

      # Add date range matching conditions to the query
      # Handle both -- and /* */ comment styles around the placeholder (formatters may convert between them)
      matching_query_adjusted <- gsub(
        "(?:--|/\\*)\\s*\\{\\{RANGE_MATCH_CONDITIONS\\}\\}(?:\\s*\\*/)?",
        range_match_sql_conditions,
        matching_query_adjusted,
        perl = TRUE
      )

      # DEBUG: Check if placeholder was substituted
      if (!is.null(col_date_match) && grepl("RANGE_MATCH_CONDITIONS", matching_query_adjusted)) {
        logger::log_warn(paste0("[MATCHING] - WARNING: Date match placeholder was not substituted for year ", year))
        logger::log_debug(paste0("[MATCHING] - Date conditions to inject:\n", range_match_sql_conditions))
      } else if (!is.null(col_date_match)) {
        logger::log_debug(paste0("[MATCHING] - Date match conditions injected for year ", year))
      }

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
  logger::log_info(c("[MATCHING] - END"))

  # Read all matching results from DuckDB back into R for post-processing
  logger::log_info("[MATCHING] - Reading matching dataset back into R")

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
    by = col_person_id
  ]

  age_idx <- match(match_results_rebuilt_long[[col_person_id]], age_lookup[[col_person_id]])
  match_results_rebuilt_long[, (col_age_iterator) := age_lookup$age_value[age_idx]]

  # Retrieve original date columns for date matching if specified
  if (!is.null(col_date_match)) {
    pop_dt <- data.table::as.data.table(matching_pop_groupkey)

    for (date_col in col_date_match) {
      # For each matched person, find their date from the matching population
      # by matching on person_id + matching status dates (which define the spell)
      # This ensures we get the date for the SPECIFIC SPELL that was matched, not just any spell for that person

      # Create lookup table from population data with unique person-spell-date combinations
      # Ensure person_id is character to match the original data types
      # date_lookup <- unique(
      #   pop_dt[, list(
      #     (col_person_id) = as.character(get(pid_col)),
      #     start = get(col_matching_status_start),
      #     end = get(col_matching_status_end),
      #     date_value = get(date_col)
      #   )],
      #   by = c(col_person_id, "start", "end")
      # )
      #

      date_lookup <- unique(
        pop_dt[
          ,
          .(
            start = get(col_matching_status_start),
            end = get(col_matching_status_end),
            date_value = get(date_col)
          ),
          by = col_person_id
        ],
        by = c(col_person_id, "start", "end")
      )

      # Merge with match results using the standard spell identifiers
      # All matches should have matching start/end dates from the exposed population
      match_results_rebuilt_long <- merge(
        match_results_rebuilt_long,
        date_lookup,
        by.x = c(col_person_id, col_matching_status_start, col_matching_status_end),
        by.y = c(col_person_id, "start", "end"),
        all.x = TRUE
      )

      # Rename the merged date_value column to the original date column name
      data.table::setnames(match_results_rebuilt_long, "date_value", date_col)
    }
  }

  # Select and order final output columns
  out_cols <- c(
    col_person_id, col_match_id, "boot_id", col_T0, col_treatment_group,
    col_matching_status_start, col_matching_status_end, col_age_iterator,
    matching_vars
  )

  # Add date match columns to output if specified
  if (!is.null(col_date_match)) {
    out_cols <- c(out_cols, col_date_match)
  }

  out_cols <- unique(out_cols)
  out_cols <- out_cols[out_cols %in% colnames(match_results_rebuilt_long)]
  match_results_rebuilt_long <- match_results_rebuilt_long[, ..out_cols]

  # Remove any INT date columns that may have been included
  if (!is.null(col_date_match)) {
    date_int_cols <- paste0(col_date_match, "_int")
    date_int_cols_to_drop <- intersect(date_int_cols, colnames(match_results_rebuilt_long))
    if (length(date_int_cols_to_drop) > 0) {
      match_results_rebuilt_long[, (date_int_cols_to_drop) := NULL]
    }
  }

  logger::log_info("[MATCHING] - Done reading matching dataset back into R")

  # Optionally save results to parquet file on disk
  if (save_output) {
    output_file_path <- if (!with_bootstrap) {
      file.path(result_dir, paste0(result_file, ".parquet"))
    }
    logger::log_info(paste0("[MATCHING] - Saving ", output_file_path, " to disk..."))
    arrow::write_parquet(match_results_rebuilt_long, output_file_path)
    logger::log_info(paste0("[MATCHING] - ", output_file_path, " saved to disk successfully."))
  }

  # Return results if not bootstrapping and not saving to disk
  if (!with_bootstrap && !save_output) {
    logger::log_info("[MATCHING] `save_output` set to FALSE, returning matching results")
    class(match_results_rebuilt_long) <- unique(c("matching_study_cohort", class(match_results_rebuilt_long)))
    return(match_results_rebuilt_long)
  }
}
