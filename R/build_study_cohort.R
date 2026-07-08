# Wrapper for the full matching pipeline ----------------------------------

#' Run Matched Study Cohort Pipeline
#'
#' This function serves as a wrapper to perform matching for a matched study cohort,
#' with optional bootstrapping. It includes multiple steps such as setting up the
#' matching environment, generating matching variables, preparing the profile table,
#' obtaining the eligible matching population, and performing matching.
#'
#' @param eligible_pop A data frame containing the eligible population (e.g., `D3_ELIGIBILITY`).
#' @param matching_query A string defining the matching SQL query. If `NULL`, empty,
#'   or whitespace-only, the packaged default SQL query is loaded automatically based
#'   on `matching_mode`.
#' @param target_table_query A string defining the DB table query that will be used to hold the birth-year looped matching results.
#'   For `matching_mode = "with_replacement"`, if `NULL`, empty, or whitespace-only,
#'   the packaged default target-table SQL is loaded automatically.
#' @param matching_vars A character vector of matching variables. Default includes
#'   demographic and risk-related variables such as `c("SV_SEX", "SV_REGION",
#'   "SV_HIST_COVID_VACC", "SV_BRAND_COVID_VACC", "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED",
#'   "CDC_RISK", "SV_SES_STATUS", "bivalent_type_received")`.
#' @param exposed_batch_size The batch size for processing exposed individuals during matching without replacement. Default is `50000`.
#' @param control_batch_size The batch size for processing control individuals during matching without replacement. Default is `50000`.
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
                               exposed_batch_size = 50000L,
                               control_batch_size = 50000L,
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

  logger::log_info(c("[MATCHING] - Setting up matching environment"))

  .ensure_directory(output_pars$dir_output, "output")
  .ensure_directory(intermediate_output_pars$dir_intermediate_outputs, "intermediate output")
  .ensure_directory(bootstrap_pars$dir_bootstrap, "bootstrap")

  matching_db_dir <- dirname(dir_matching_db)
  if (!identical(matching_db_dir, ".")) {
    .ensure_directory(matching_db_dir, "matching database")
  }

  if (!matching_mode %in% c("with_replacement", "without_replacement")) {
    stop("`matching_mode` must be either 'with_replacement' or 'without_replacement'.")
  }

  if (is_empty_query(matching_query)) {
    default_matching_query_file <- switch(matching_mode,
      with_replacement = "matching_query_with_replacement.sql",
      without_replacement = "matching_query_without_replacement.sql"
    )

    default_matching_query_path <- system.file("sql_queries", default_matching_query_file, package = "CohortBuilder")
    if (!nzchar(default_matching_query_path)) {
      stop(paste0("Could not locate default SQL query file: ", default_matching_query_file))
    }

    matching_query <- getSQL(default_matching_query_path)
    logger::log_info(paste0("[MATCHING] - Loaded default matching SQL from ", default_matching_query_file, "."))
  }

  if (matching_mode == "with_replacement" && is_empty_query(target_table_query)) {
    default_target_table_query_file <- "create_matching_target_table.sql"
    default_target_table_query_path <- system.file("sql_queries", default_target_table_query_file, package = "CohortBuilder")

    if (!nzchar(default_target_table_query_path)) {
      stop(paste0("Could not locate default target-table SQL file: ", default_target_table_query_file))
    }

    target_table_query <- getSQL(default_target_table_query_path)
    logger::log_info(paste0("[MATCHING] - Loaded default target-table SQL from ", default_target_table_query_file, "."))
  }

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
    matched_population_colnames <- c(
      output_column_names$col_person_id,
      output_column_names$col_match_id, output_column_names$col_T0, output_column_names$col_treatment_group, input_column_names$col_matching_status_start, input_column_names$col_matching_status_end, input_column_names$col_age_iterator
    )
    matched_population <- data.table::setDT(setNames(
      data.frame(matrix(ncol = length(c(matched_population_colnames, matching_vars)), nrow = 0)),
      c(matched_population_colnames, matching_vars)
    ))

    if (output_pars$save_output) {
      output_file_path <- file.path(output_pars$dir_output, paste0(output_pars$output_file_name, ".parquet"))
      arrow::write_parquet(matched_population, output_file_path)
    }

    logger::log_info(stop(c("No eligible exposed (`eligible_exposed = TRUE`) available in the data. Matching will not take place.")))
  }

  if (all(eligible_pop[[input_column_names$col_eligible_control]] == FALSE)) {
    logger::log_info(warning(c("No eligible controls (`eligible_control = TRUE`) available in the data. Eligible exposed will not be matched.")))
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

  matching_conn <- DBI::dbConnect(duckdb::duckdb(), dir_matching_db)

  if (matching_mode == "with_replacement") {
    D4_MSC <- match_cohorts_with_replacement(
      matching_pop_groupkey = D3_MATCHING_POP,
      profile_table = D3_LOOKUP_TABLE,
      matching_query = matching_query,
      target_table_query = target_table_query,
      matching_vars = matching_vars,
      n_cores = n_cores,
      save_output = FALSE,
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
      save_output = FALSE,
      n_cores = n_cores,
      start_seed = bootstrap_pars$start_seed,
      col_person_id = input_column_names$col_person_id,
      col_matching_status_start = input_column_names$col_matching_status_start,
      col_matching_status_end = input_column_names$col_matching_status_end,
      col_age_iterator = input_column_names$col_age_iterator,
      col_match_id = output_column_names$col_match_id,
      col_treatment_group = output_column_names$col_treatment_group,
      col_T0 = output_column_names$col_T0,
      exposed_batch_size = exposed_batch_size,
      control_batch_size = control_batch_size
    )
  }

  DBI::dbDisconnect(matching_conn, shutdown = TRUE)
  rm(matching_conn)
  invisible(gc())

  logger::log_info(paste0("[MATCHING] - Disconnected from the database (", dir_matching_db, ")."))

  if (file.exists(dir_matching_db)) {
    file.remove(dir_matching_db)
    logger::log_info("[MATCHING] - Removed existing database.")
  }

  # Write cohort to disk (non-bootstrap runs only)
  if (!bootstrap_pars$with_bootstrap && isTRUE(output_pars$save_output)) {
    output_file_path <- file.path(
      output_pars$dir_output,
      paste0(output_pars$output_file_name, ".parquet")
    )
    logger::log_info(paste0("[MATCHING] - Saving ", output_file_path, " to disk..."))
    arrow::write_parquet(D4_MSC, output_file_path)
    logger::log_info(paste0("[MATCHING] - ", output_file_path, " saved to disk successfully."))
  }

  # Log cohort summary (non-bootstrap runs only)
  if (!bootstrap_pars$with_bootstrap) {
    tryCatch(
      .log_matching_summary(D4_MSC),
      error = function(e) {
        logger::log_info(paste0("[MATCHING] - Could not log cohort summary: ", conditionMessage(e)))
      }
    )
  }

  if (!bootstrap_pars$with_bootstrap) {
    return(D4_MSC)
  }
}
