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


