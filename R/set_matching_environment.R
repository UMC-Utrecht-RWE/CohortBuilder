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


