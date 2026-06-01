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
  logger::log_info(paste0("[MATCHING] - Creating ", output_file))

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
  logger::log_info(paste0("[MATCHING] - ", output_file, " created successfully"))

  # Save output if required
  if (isTRUE(save_output)) {
    logger::log_info(paste0("[MATCHING] - Saving ", output_file, " to ", output_dir, "/", output_file, ".parquet"))
    arrow::write_parquet(matching_pop_groupkey, file.path(output_dir, paste0(output_file, ".parquet")))
    logger::log_info(paste0("[MATCHING] - ", output_file, " saved successfully"))
  }

  # Return the result
  return(matching_pop_groupkey)
}
