# Set up matching environment ---------------------------------------------

#' Set Up Matching Environment
#'
#' This function sets up the environment for the matching process by verifying input files,
#' creating necessary directories, and cleaning up old files or databases.
#'
#' @param eligible_pop A file path to the eligible population data. Must be specified; otherwise, the function will stop.
#' @param matching_query A file path to the matching query. Must be specified; otherwise, the function will stop.
#' @param with_bootstrap Logical. If `TRUE`, enables bootstrapping functionality and manages bootstrap directories. Default is `FALSE`.
#' @param n_bootstraps Integer. Number of bootstrap iterations. Required when `with_bootstrap` is `TRUE`.
#' @param dir_bootstrap A directory path for storing bootstrap outputs. Required if `with_bootstrap` is `TRUE`.
#' @param dir_matching_db A file path to the matching database. If it exists, it will be removed during setup.
#'
#' @return `NULL`, invisibly. Called for its side-effects.
#'
#' @details
#' - **Eligibility Check**: The function ensures that `eligible_pop` and `matching_query` are provided and stops with an error if they are missing.
#' - **Bootstrapping**: If `with_bootstrap` is `TRUE`, the function:
#'   - Creates the bootstrap output folder if it does not exist.
#'   - Deletes existing bootstrap files matching the pattern `*_bootstrap_*.parquet` in the specified directory.
#' - **Database Cleanup**: If a matching database file exists at the specified `dir_matching_db` path, it is deleted.
#' - **Logging**: Logs key actions, including directory creation, file deletion, and setup completion.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
set_matching_environment <- function(eligible_pop = NULL,
                                     matching_query = NULL,
                                     with_bootstrap = FALSE,
                                     n_bootstraps = NULL,
                                     dir_bootstrap = NULL,
                                     dir_matching_db = NULL) {
  logr::log_print(c("[MATCHING] - Setting up matching environment"))

  # Check if the eligible_pop file exists
  if (is.null(eligible_pop)) {
    stop(paste0("Eligible population data is missing. Please specify argument `eligible_pop`."))
  }

  # Check if the matching_query file exists
  if (is.null(matching_query)) {
    stop(paste0("Matching query is missing. Please specify argument `matching_query`."))
  }

  # Check if the bootstrap output folder exists, if not, create it
  if (with_bootstrap) {
    if (is.null(n_bootstraps)) {
      stop(paste0("Please specify argument `n_bootstraps` to perform bootstrapping."))
    }

    # Only if we need bootstrapping
    if (!dir.exists(dir_bootstrap)) {
      dir.create(dir_bootstrap)
      logr::log_print("Created the bootstrap output folder")
    } else {
      # Get the list of files matching the pattern "*_bootstrap_*.parquet" in the directory
      bootstrap_files <- list.files(dir_bootstrap, pattern = ".*_bootstrap_[0-9]+\\.parquet$", full.names = TRUE)

      # Check if there are any matching files and delete them if they exist
      if (length(bootstrap_files) > 0) {
        file.remove(bootstrap_files)
        logr::log_print(paste0("Deleted the following files: ", paste(basename(bootstrap_files), collapse = ", ")))
      } else {
        logr::log_print("No files matching '.*_bootstrap_[0-9]+\\.parquet$' found in the directory.")
      }
    }
  }

  # Remove the database if it exists
  if (file.exists(dir_matching_db)) {
    file.remove(dir_matching_db)
    logr::log_print("Removed existing database.")
  }

  logr::log_print(c("[MATCHING] - Matching environment set up successfully"))
}


# Get matching variables --------------------------------------------------

#' Retrieve and Validate Matching Variables
#'
#' This function retrieves and validates the specified matching variables from the input eligible population.
#' It ensures only variables present in the data are used for matching, and logs warnings for any missing variables.
#'
#' @param eligible_pop A data frame containing the eligible population, including potential matching variables.
#' @param matching_vars A character vector of matching variable names to be retrieved and validated. Defaults to
#' `c("SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_BRAND_COVID_VACC", "SV_PREG_STATUS",
#' "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS", "bivalent_type_received")`.
#'
#' @return A character vector of matching variables that are present in the input data (`eligible_pop`).
#' If some variables are not found, they are excluded, and a warning is logged.
#'
#' @details
#' - The function checks the presence of each variable in `matching_vars` within the columns of `eligible_pop`.
#' - Any variables in `matching_vars` that are missing from the input data are excluded from the result,
#'   and a warning is logged to inform the user.
#' - The final list of variables to be used for matching is logged and returned.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
get_matching_variables <- function(eligible_pop, matching_vars = c(
                                     "SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_BRAND_COVID_VACC",
                                     "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS", "bivalent_type_received"
                                   )) {
  logr::log_print(c("[MATCHING] - Retrieving matching variables"))

  # Check which variables are missing from the input data
  missing_vars <- setdiff(matching_vars, colnames(eligible_pop))
  if (length(missing_vars) > 0) {
    logr::log_print(warning(paste0(
      "The following variables are missing from the input data and will not be used for matching: ",
      paste(missing_vars, collapse = ", ")
    )))
  }

  # Update matching_vars to only include variables present in the input data
  matching_vars <- intersect(matching_vars, colnames(eligible_pop))

  # Inform the user about the variables that will actually be used
  logr::log_print(paste0("The following variables will be used for matching: ", paste(matching_vars, collapse = ", ")))

  logr::log_print(c("[MATCHING] - Matching variables retrieved successfully"))

  return(matching_vars)
}


# Get lookup table --------------------------------------------------------

#' Generate a Profile Table for Matching Variables
#'
#' This function creates a lookup table (profile table) from the eligible population for the specified matching variables.
#' The table assigns a unique key (`groupkey`) to each distinct combination of matching variable values.
#'
#' @param eligible_pop A data frame containing the eligible population, including columns for the matching variables.
#' @param matching_vars A character vector of variable names used to create the profile table.
#' @param save_output A logical value. If `TRUE`, the resulting profile table is saved to the specified directory.
#' @param output_dir A string specifying the directory to save the output if `save_output` is `TRUE`.
#' @param output_file A string specifying the file name for the saved output. Defaults to `"D3_LOOKUP_TABLE"`.
#'
#' @return A data.table containing the distinct combinations of matching variables and a unique integer identifier
#' (`groupkey`) for each combination. If `save_output` is `TRUE`, the profile table is saved to disk in `.parquet` format.
#'
#' @details
#' - The function filters the eligible population (`eligible_pop`) to include only the specified `matching_vars`.
#' - It ensures each combination of matching variable values is unique and assigns a unique integer key (`groupkey`)
#'   using `.I`.
#' - If `save_output` is `TRUE`, the resulting table is saved in `.parquet` format for quick access.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
get_profile_table <- function(eligible_pop = NULL, matching_vars = NULL, save_output = FALSE, output_dir = NULL, output_file = "D3_LOOKUP_TABLE") {
  logr::log_print("[MATCHING] - Creating lookup table")

  # Convert to data.table
  eligible_pop_dt <- data.table::as.data.table(eligible_pop)

  # Filter and create the profile table with unique combinations of matching variables
  profile_table <- eligible_pop_dt[, ..matching_vars][
    , .SD[!duplicated(.SD)],
    .SDcols = matching_vars # Get unique rows
  ][
    , groupkey := .I # Assign a unique integer identifier
  ]

  logr::log_print("[MATCHING] - Lookup table created successfully")

  # Save lookup table to disk if required
  if (save_output) {
    if (is.null(output_dir)) stop("Output directory must be specified when save_output = TRUE")
    output_path <- file.path(output_dir, paste0(output_file, ".parquet"))
    logr::log_print(paste0("[MATCHING] - Saving lookup table to ", output_path))
    arrow::write_parquet(profile_table, output_path)
    logr::log_print("[MATCHING] - Lookup table saved successfully")
  }

  return(profile_table)
}


# Get eligible matching population ----------------------------------------

#' Get the Eligible Matching Population
#'
#' This function creates a subset of the eligible population, prepares the data for matching
#' by grouping individuals into exposed and control groups, and optionally saves the resulting
#' population to disk.
#'
#' @param eligible_pop A data frame containing the eligible population with relevant columns for matching.
#' @param profile_table A data frame containing additional profile information for the population.
#' @param matching_vars A character vector of variable names used for joining `eligible_pop` with `profile_table`.
#' @param save_output A logical value. If `TRUE`, the resulting matching population is saved to the specified directory.
#' @param output_dir A string specifying the directory to save the output if `save_output` is `TRUE`.
#' @param output_file A string specifying the file name for the saved output. Defaults to `"D3_MATCHING_POP"`.
#' @param col_person_id A string specifying the column name for the unique person identifier. Defaults to `"person_id"`.
#' @param col_eligible_exposed A string specifying the column name indicating eligibility for the exposed group. Defaults to `"eligible_exposed"`.
#' @param col_eligible_control A string specifying the column name indicating eligibility for the control group. Defaults to `"eligible_control"`.
#' @param col_matching_status_start A string specifying the column name for the start date of the matching status. Defaults to `"matching_status_start"`.
#' @param col_matching_status_end A string specifying the column name for the end date of the matching status. Defaults to `"matching_status_end"`.
#' @param col_age_iterator A string specifying the column name for the year of birth or age iterator. Defaults to `"year_of_birth"`.
#'
#' @return A data.table of the matching population, with individuals grouped into "exposed" and "control" groups.
#' If `save_output` is `TRUE`, the function also saves the resulting data frame to the specified directory.
#'
#' @details
#' - The function filters and joins the eligible population (`eligible_pop`) with the `profile_table`
#'   based on the variables specified in `matching_vars`.
#' - Individuals are assigned to either the "exposed" or "control" group based on their eligibility columns.
#' - Additional grouping and formatting are performed to prepare the data for downstream matching.
#' - If `save_output` is `TRUE`, the resulting population is saved in `.parquet` format for quick access.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
get_matching_population <- function(eligible_pop = NULL,
                                    profile_table = NULL,
                                    matching_vars = NULL,
                                    save_output = NULL,
                                    output_dir = NULL,
                                    output_file = "D3_MATCHING_POP",
                                    col_person_id = "person_id",
                                    col_eligible_exposed = "eligible_exposed",
                                    col_eligible_control = "eligible_control",
                                    col_matching_status_start = "matching_status_start",
                                    col_matching_status_end = "matching_status_end",
                                    col_age_iterator = "year_of_birth") {
  # Log start of the operation
  logr::log_print(paste0("[MATCHING] - Creating ", output_file))

  # Convert inputs to data.tables if not already
  eligible_pop <- data.table::as.data.table(eligible_pop)
  profile_table <- data.table::as.data.table(profile_table)

  # Perform inner join
  matching_pop_groupkey <- merge(
    x = eligible_pop[, c(
      col_person_id, col_age_iterator, col_matching_status_start,
      col_matching_status_end, matching_vars,
      col_eligible_exposed, col_eligible_control
    ), with = FALSE], # Prevent `with = TRUE` to handle column names dynamically
    y = profile_table,
    by = matching_vars,
    all = FALSE
  )

  # Create the 'group' column based on eligibility
  matching_pop_groupkey[, `:=`(
    group = data.table::fifelse(
      get(col_eligible_exposed) == TRUE, "exposed",
      data.table::fifelse(get(col_eligible_control) == TRUE, "control", NA_character_)
    )
  )]

  # Reorder and select columns dynamically (including inherited groupkey from profile_table)
  matching_pop_groupkey <- matching_pop_groupkey[, c(
    col_person_id, col_matching_status_start, col_matching_status_end,
    col_age_iterator, "group", "groupkey"
  ), with = FALSE]

  # Log successful creation
  logr::log_print(paste0("[MATCHING] - ", output_file, " created successfully"))

  # Save output if required
  if (isTRUE(save_output)) {
    logr::log_print(paste0("[MATCHING] - Saving ", output_file, " to ", output_dir, "/", output_file, ".parquet"))
    arrow::write_parquet(matching_pop_groupkey, file.path(output_dir, paste0(output_file, ".parquet")))
    logr::log_print(paste0("[MATCHING] - ", output_file, " saved successfully"))
  }

  # Return the result
  return(matching_pop_groupkey)
}


# Match cohorts (with replacement / bootstrap) ----------------------------

#' Match Cohorts with Optional Bootstrapping
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
match_cohorts <- function(matching_pop_groupkey = NULL,
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
  #########################################
  ##### Connect to the DuckDB-database ####
  #########################################

  # Check number of cores
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    logr::log_print(paste0("The parameter `n_cores` was not specified. By default ", n_cores, " will be used in the SQL matching procedure."))
  }

  DBI::dbExecute(matching_conn, paste0("PRAGMA threads=", n_cores, ";"))

  # Create our matching target table
  DBI::dbExecute(matching_conn, target_table_query)

  ##################################################
  ##### Overrule number of bootstrap iterations ####
  ##################################################

  if (!with_bootstrap) {
    if (is.null(n_bootstraps)) {
      n_bootstraps <- 1
    }

    if (n_bootstraps != 1) {
      logr::log_print(paste0("The parameter `n_bootstraps` was overruled to 1 since no bootstrap will be done (`with_bootstrap` = FALSE)"))
    }
    n_bootstraps <- 1
  }

  #######################################
  #### Let's start the bootstrapping ####
  #######################################

  for (bootstrap in 1:n_bootstraps) {
    tictoc::tic()

    logr::log_print(paste0("Doing iteration ", bootstrap, " of ", n_bootstraps, ".... \n"))

    if (with_bootstrap) {
      set.seed(start_seed + bootstrap)

      cat(sprintf("\r%-50s", "Sampling...."))
      sampled_ids <- matching_pop_groupkey[
        , .(person_id = unique(get(col_person_id)))
      ][
        , .(person_id = sample(person_id, .N, replace = TRUE))
      ][
        order(person_id)
      ]

      # Prepare the join expression dynamically
      join_condition <- setNames("person_id", col_person_id)

      cat(sprintf("\r%-50s", "Sampling back to the source population...."))
      sampled_df <- matching_pop_groupkey[sampled_ids, on = join_condition]

      rm(sampled_ids, join_condition)
      gc()
    } else {
      cat(sprintf("\r%-50s", "No resampling; the source population will be used...."))
      sampled_df <- matching_pop_groupkey

      gc()
    }

    # Convert variables to int using data.table
    sampled_df[, person_id_int := as.integer(factor(person_id))]
    sampled_df[, startdateINT := as.integer(as.Date(matching_status_start) - as.Date("1970-01-01"))]
    sampled_df[, enddateINT := as.integer(as.Date(matching_status_end) - as.Date("1970-01-01"))]
    sampled_df[, year_of_birth := as.integer(get(col_age_iterator))]
    sampled_df[, groupkey := as.integer(groupkey)]

    min_year <- min(sampled_df$year_of_birth, na.rm = TRUE)
    max_year <- max(sampled_df$year_of_birth, na.rm = TRUE)

    cat(sprintf("\r%-50s", "Selecting the exposed population...."))
    sampled_df_Exp <- sampled_df[
      group == "exposed",
      .(
        person_id, person_id_int, groupkey, group, matching_status_start, matching_status_end,
        year_of_birth, startdateINT, enddateINT
      )
    ]
    sampled_df_Exp[, random := runif(.N, min = 0, max = 10)]
    sampled_df_Exp[, match_id := .I]

    cat(sprintf("\r%-50s", "Selecting the unexposed population...."))
    sampled_df_Un <- sampled_df[
      group == "control",
      .(
        person_id, groupkey, group, matching_status_start, matching_status_end,
        year_of_birth, startdateINT, enddateINT
      )
    ]
    sampled_df_Un[, random := runif(.N, min = 0, max = 10)]

    rm(sampled_df)
    gc()

    ########################################################################
    #### We're going to send these dataframes to Duckdb to do the magic ####
    ########################################################################

    df_bootstrap <- data.frame(boot_id = bootstrap)

    cat(sprintf("\r%-50s", "Sending data to DuckDB...."))
    DBI::dbWriteTable(matching_conn, DBI::SQL("dfexp"), sampled_df_Exp, overwrite = TRUE, temporary = TRUE)
    DBI::dbWriteTable(matching_conn, DBI::SQL("dfun"), sampled_df_Un, overwrite = TRUE, temporary = TRUE)
    DBI::dbWriteTable(matching_conn, DBI::SQL("dfbootstrap"), as.data.frame(df_bootstrap), overwrite = TRUE)

    rm(sampled_df_Exp, sampled_df_Un)
    gc()

    ################################
    #### Now we do the matching ####
    ################################

    for (year in min_year:max_year) {
      cat(sprintf("\rDoing year_of_birth loops, %d%% ready....", round(((year - min_year) / (max_year - min_year)) * 100)))

      matching_query_adjusted <- gsub(
        "year_of_birth = 1920",
        paste0("year_of_birth = ", year),
        matching_query
      )
      matching_query_adjusted <- gsub(
        "year_of_birth BETWEEN 1919 AND 1921",
        paste0("year_of_birth BETWEEN ", year - 1, " AND ", year + 1),
        matching_query_adjusted
      )

      if (with_bootstrap) {
        matching_query_adjusted <- gsub(
          "LEFT JOIN matched_persons M",
          "INNER JOIN matched_persons M",
          matching_query_adjusted
        )
      }

      matched_pop_year <- DBI::dbExecute(matching_conn, matching_query_adjusted)
    }

    toc_log_print()
  }

  logr::log_print(c("[MATCHING] - END"))

  #############################################################################################
  #### Now that we've done the matching / bootstrapping it's time to read it all back to R ####
  #############################################################################################

  logr::log_print("[MATCHING] - Reading matching dataset back into R")

  match_results <- DBI::dbReadTable(matching_conn, "match_result")
  data.table::setDT(match_results)

  origin_date <- as.Date("1970-01-01")

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

  # Exposed + unmatched
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

  # Matched controls
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

  # Combine
  match_results_rebuilt_long <- data.table::rbindlist(
    list(
      match_results_rebuilt_long_exposed,
      match_results_rebuilt_long_control
    ),
    use.names = TRUE,
    fill = TRUE
  )[
    ,
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
    c(
      col_person_id, col_match_id, "boot_id", col_T0, col_treatment_group,
      col_matching_status_start, col_matching_status_end,
      matching_vars
    ),
    with = FALSE
  ]

  logr::log_print("[MATCHING] - Done reading matching dataset back into R")

  if (save_output) {
    output_file_path <- if (!with_bootstrap) {
      file.path(result_dir, paste0(result_file, ".parquet"))
    }
    logr::log_print(paste0("[MATCHING] - Saving ", output_file_path, " to disk..."))
    arrow::write_parquet(match_results_rebuilt_long, output_file_path)
    logr::log_print(paste0("[MATCHING] - ", output_file_path, " saved to disk successfully."))
  }

  if (!with_bootstrap & !save_output) {
    logr::log_print("[MATCHING] `save_output` set to FALSE, returning matching results")
    return(match_results_rebuilt_long)
  }
}


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


# Wrapper for the full matching pipeline ----------------------------------

#' Run Matched Study Cohort Pipeline
#'
#' This function serves as a wrapper to perform matching for a matched study cohort,
#' with optional bootstrapping. It includes multiple steps such as setting up the
#' matching environment, generating matching variables, preparing the profile table,
#' obtaining the eligible matching population, and performing matching.
#'
#' @param eligible_pop A data frame containing the eligible population (e.g., `D3_ELIGIBILITY`).
#' @param matching_query A string defining the matching SQL query.
#' @param target_table_query A string defining the DB table query that will be used to hold the birth-year looped matching results.
#' @param matching_vars A character vector of matching variables. Default includes
#'   demographic and risk-related variables such as `c("SV_SEX", "SV_REGION",
#'   "SV_HIST_COVID_VACC", "SV_BRAND_COVID_VACC", "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED",
#'   "CDC_RISK", "SV_SES_STATUS", "bivalent_type_received")`.
#' @param dir_matching_db The file path for the temporary DuckDB database used for matching.
#'   Default is `"transformations/T3_study_design/intermediate_data_file/matching.duckdb"`.
#' @param n_cores Number of CPU cores to use for parallel processing. Default is `NULL`,
#'   which uses all available cores minus one.
#' @param log_name Name of the log file. Default is `"log_build_study_cohort"`.
#' @param output_pars A list of output parameters:
#'   \describe{
#'     \item{`save_output`}{Logical. If `TRUE`, saves the final matched cohort to disk. Default is `TRUE`.}
#'     \item{`dir_output`}{Directory where the output file will be saved.}
#'     \item{`output_file_name`}{The name of the output file (without extension). Default is `"D4_MSC"`.}
#'   }
#' @param intermediate_output_pars A list of parameters for intermediate outputs:
#'   \describe{
#'     \item{`save_intermediate_outputs`}{Logical. If `TRUE`, saves intermediate files. Default is `TRUE`.}
#'     \item{`dir_intermediate_outputs`}{Directory for intermediate outputs.}
#'     \item{`profile_table_name`}{Name of the profile table file. Default is `"D3_LOOKUP_TABLE"`.}
#'     \item{`matching_pop_name`}{Name of the matching population file. Default is `"D3_MATCHING_POP"`.}
#'   }
#' @param bootstrap_pars A list of parameters for bootstrapping:
#'   \describe{
#'     \item{`with_bootstrap`}{Logical. If `TRUE`, performs bootstrapping during matching. Default is `FALSE`.}
#'     \item{`n_bootstraps`}{Number of bootstrap iterations. Default is `500`.}
#'     \item{`dir_bootstrap`}{Directory for bootstrap files.}
#'     \item{`start_seed`}{Random seed for reproducibility. Default is `42`.}
#'   }
#' @param input_column_names A list specifying the column names in the input data:
#'   \describe{
#'     \item{`col_person_id`}{Column name for the unique person identifier. Default is `"person_id"`.}
#'     \item{`col_eligible_exposed`}{Column name indicating eligibility as exposed. Default is `"eligible_exposed"`.}
#'     \item{`col_eligible_control`}{Column name indicating eligibility as a control. Default is `"eligible_control"`.}
#'     \item{`col_matching_status_start`}{Column name for the start date of matching eligibility. Default is `"matching_status_start"`.}
#'     \item{`col_matching_status_end`}{Column name for the end date of matching eligibility. Default is `"matching_status_end"`.}
#'     \item{`col_age_iterator`}{Column name for the age iterator. Default is `"year_of_birth"`.}
#'   }
#' @param output_column_names A list specifying the column names for the output data:
#'   \describe{
#'     \item{`col_person_id`}{Column name for the unique person identifier. Default is `"person_id"`.}
#'     \item{`col_match_id`}{Column name for the match ID. Default is `"match_id"`.}
#'     \item{`col_treatment_group`}{Column name for the treatment group. Default is `"group"`.}
#'     \item{`col_T0`}{Column name for the T0 variable. Default is `"T0"`.}
#'   }
#' @param matching_mode Matching strategy. Use `"with_replacement"` (default) for the
#'   existing SQL matcher, or `"without_replacement"` for greedy no-replacement matching.
#'
#' @return A data.table containing the matched study cohort. If `output_pars$save_output` is `TRUE`,
#'   the data frame is also saved to disk as a `.parquet` file.
#'
#' @details
#' The function integrates several components of the matching pipeline:
#' \itemize{
#'   \item Setting up the matching environment (including bootstrapping, if specified).
#'   \item Preparing matching variables and profile tables.
#'   \item Filtering and processing the eligible population.
#'   \item Executing the matching process with optional bootstrapping.
#' }
#'
#' @note
#' If no eligible exposed individuals are available (`eligible_exposed = FALSE` for all rows),
#' the function terminates early and logs an error. Similarly, a warning is issued if no eligible
#' controls are found.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
build_study_cohort <- function(eligible_pop = NULL,
                               matching_query = NULL,
                               target_table_query = NULL,
                               matching_vars = c(
                                 "SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_BRAND_COVID_VACC",
                                 "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS", "bivalent_type_received"
                               ),
                               dir_matching_db = "transformations/T3_study_design/intermediate_data_file/matching.duckdb",
                               n_cores = NULL,
                               log_name = "log_build_study_cohort",
                               output_pars = list(
                                 save_output = TRUE,
                                 dir_output = "outputs",
                                 output_file_name = "D4_MSC"
                               ),
                               intermediate_output_pars = list(
                                 save_intermediate_outputs = TRUE,
                                 dir_intermediate_outputs = "intermediate_outputs",
                                 profile_table_name = "D3_LOOKUP_TABLE",
                                 matching_pop_name = "D3_MATCHING_POP"
                               ),
                               bootstrap_pars = list(
                                 with_bootstrap = FALSE,
                                 n_bootstraps = 500,
                                 dir_bootstrap = "intermediate_outputs/bootstraps",
                                 start_seed = 42
                               ),
                               input_column_names = list(
                                 col_person_id = "person_id",
                                 col_eligible_exposed = "eligible_exposed",
                                 col_eligible_control = "eligible_control",
                                 col_matching_status_start = "matching_status_start",
                                 col_matching_status_end = "matching_status_end",
                                 col_age_iterator = "year_of_birth"
                               ),
                               output_column_names = list(
                                 col_person_id = "person_id",
                                 col_match_id = "match_id",
                                 col_treatment_group = "group",
                                 col_T0 = "T0"
                               ),
                               matching_mode = "with_replacement") {
  #########################################
  #### Set up the matching environment ####
  #########################################

  logr::log_open(logdir = TRUE, file_name = log_name)
  logr::log_print(c("[MATCHING] - Setting up matching environment"))

  set_matching_environment(
    eligible_pop = eligible_pop,
    matching_query = matching_query,
    with_bootstrap = bootstrap_pars$with_bootstrap,
    n_bootstraps = bootstrap_pars$n_bootstraps,
    dir_bootstrap = bootstrap_pars$dir_bootstrap,
    dir_matching_db = dir_matching_db
  )

  ################################
  #### Get matching variables ####
  ################################

  matching_vars <- get_matching_variables(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars
  )

  ###########################
  #### Get profile table ####
  ###########################

  if (all(eligible_pop[[input_column_names$col_eligible_exposed]] == FALSE)) {
    matched_population_colnames <- c("person_id", "match_id", "T0", "group", "matching_status_start", "matching_status_end", "year_of_birth")
    matched_population <- data.table::setDT(setNames(
      data.frame(matrix(ncol = length(c(matched_population_colnames, matching_vars)), nrow = 0)),
      c(matched_population_colnames, matching_vars)
    ))

    if (output_pars$save_output) {
      output_file_path <- file.path(output_pars$dir_output, paste0(output_pars$output_file_name, ".parquet"))
      arrow::write_parquet(matched_population, output_file_path)
    }

    logr::log_print(stop(c("No eligible exposed (`eligible_exposed = TRUE`) available in the data. Matching will not take place.")))
  }

  if (all(eligible_pop[[input_column_names$col_eligible_control]] == FALSE)) {
    logr::log_print(warning(c("No eligible controls (`eligible_control = TRUE`) available in the data. Eligible exposed will not be matched.")))
  }

  # Drop non-eligible spells
  eligible_pop <- data.table::as.data.table(eligible_pop)
  eligible_pop <- eligible_pop[
    get(input_column_names$col_eligible_exposed) == TRUE | get(input_column_names$col_eligible_control) == TRUE
  ]

  D3_LOOKUP_TABLE <- get_profile_table(
    eligible_pop = eligible_pop,
    matching_vars = matching_vars,
    save_output = intermediate_output_pars$save_intermediate_outputs,
    output_dir = intermediate_output_pars$dir_intermediate_outputs,
    output_file = intermediate_output_pars$profile_table_name
  )

  ##########################################
  #### Get eligible matching population ####
  ##########################################

  D3_MATCHING_POP <- get_matching_population(
    eligible_pop = eligible_pop,
    profile_table = D3_LOOKUP_TABLE,
    matching_vars = matching_vars,
    save_output = intermediate_output_pars$save_intermediate_outputs,
    output_dir = intermediate_output_pars$dir_intermediate_outputs,
    output_file = intermediate_output_pars$matching_pop_name,
    col_person_id = input_column_names$col_person_id,
    col_eligible_exposed = input_column_names$col_eligible_exposed,
    col_eligible_control = input_column_names$col_eligible_control,
    col_matching_status_start = input_column_names$col_matching_status_start,
    col_matching_status_end = input_column_names$col_matching_status_end,
    col_age_iterator = input_column_names$col_age_iterator
  )

  ###########################################
  #### Match with optional bootstrapping ####
  ###########################################

  if (!matching_mode %in% c("with_replacement", "without_replacement")) {
    stop("`matching_mode` must be either 'with_replacement' or 'without_replacement'.")
  }

  matching_conn <- DBI::dbConnect(duckdb::duckdb(), dir_matching_db)

  if (matching_mode == "with_replacement") {
    D4_MSC <- match_cohorts(
      matching_pop_groupkey = D3_MATCHING_POP,
      profile_table = D3_LOOKUP_TABLE,
      matching_query = matching_query,
      target_table_query = target_table_query,
      matching_vars = matching_vars,
      n_cores = n_cores,
      result_dir = output_pars$dir_output,
      result_file = output_pars$output_file_name,
      save_output = output_pars$save_output,
      matching_conn = matching_conn,
      dir_bootstrap = bootstrap_pars$dir_bootstrap,
      with_bootstrap = bootstrap_pars$with_bootstrap,
      n_bootstraps = bootstrap_pars$n_bootstraps,
      start_seed = bootstrap_pars$start_seed,
      col_person_id = input_column_names$col_person_id,
      col_matching_status_start = input_column_names$col_matching_status_start,
      col_matching_status_end = input_column_names$col_matching_status_end,
      col_age_iterator = input_column_names$col_age_iterator,
      col_match_id = output_column_names$col_match_id,
      col_treatment_group = output_column_names$col_treatment_group,
      col_T0 = output_column_names$col_T0
    )
  } else {
    D4_MSC <- match_cohorts_without_replacement(
      matching_pop_groupkey = D3_MATCHING_POP,
      profile_table = D3_LOOKUP_TABLE,
      matching_vars = matching_vars,
      matching_query = matching_query,
      matching_conn = matching_conn,
      result_dir = output_pars$dir_output,
      result_file = output_pars$output_file_name,
      save_output = output_pars$save_output,
      n_cores = n_cores,
      start_seed = bootstrap_pars$start_seed,
      col_person_id = input_column_names$col_person_id,
      col_matching_status_start = input_column_names$col_matching_status_start,
      col_matching_status_end = input_column_names$col_matching_status_end,
      col_age_iterator = input_column_names$col_age_iterator,
      col_match_id = output_column_names$col_match_id,
      col_treatment_group = output_column_names$col_treatment_group,
      col_T0 = output_column_names$col_T0
    )
  }

  DBI::dbDisconnect(matching_conn, shutdown = TRUE)
  rm(matching_conn)
  invisible(gc())

  logr::log_print(paste0("[MATCHING] - Disconnected from the database (", dir_matching_db, ")."))

  if (file.exists(dir_matching_db)) {
    file.remove(dir_matching_db)
    logr::log_print("[MATCHING] - Removed existing database.")
  }

  logr::log_close(footer = TRUE)

  if (!bootstrap_pars$with_bootstrap & !output_pars$save_output) {
    return(D4_MSC)
  }
}
