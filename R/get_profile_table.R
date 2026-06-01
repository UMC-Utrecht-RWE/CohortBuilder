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
  logger::log_info("[MATCHING] - Creating lookup table")

  # Convert to data.table
  eligible_pop_dt <- data.table::as.data.table(eligible_pop)

  # Filter and create the profile table with unique combinations of matching variables
  profile_table <- eligible_pop_dt[, ..matching_vars][
    , .SD[!duplicated(.SD)],
    .SDcols = matching_vars # Get unique rows
  ][
    , groupkey := .I # Assign a unique integer identifier
  ]

  logger::log_info("[MATCHING] - Lookup table created successfully")

  # Save lookup table to disk if required
  if (save_output) {
    if (is.null(output_dir)) stop("Output directory must be specified when save_output = TRUE")
    output_path <- file.path(output_dir, paste0(output_file, ".parquet"))
    logger::log_info(paste0("[MATCHING] - Saving lookup table to ", output_path))
    arrow::write_parquet(profile_table, output_path)
    logger::log_info("[MATCHING] - Lookup table saved successfully")
  }

  return(profile_table)
}
