# Internal directory helper -----------------------------------------------

# Create a directory if it is missing and log the action.
.ensure_directory <- function(path = NULL, label = "output") {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    logr::log_print(paste0("[MATCHING] - Created ", label, " directory: ", path))
  }

  invisible(path)
}
