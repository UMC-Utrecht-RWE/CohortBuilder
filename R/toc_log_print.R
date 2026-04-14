#' Log Elapsed Time via tictoc and logger
#'
#' Stops the most recent `tictoc::tic()` timer, logs the formatted elapsed-time
#' message via `logger::log_info()`, and clears the tictoc log.
#'
#' @return `NULL`, invisibly. Called for its side-effects.
#'
#' @export
toc_log_print <- function() {
  tictoc::toc(log = TRUE, quiet = TRUE)
  logger::log_info(unlist(tictoc::tic.log(format = TRUE)))
  tictoc::tic.clearlog()
}
