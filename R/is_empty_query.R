# Internal query validator -----------------------------------------------

#' Check if a Query Argument Is Empty
#'
#' Internal helper that treats query inputs as empty when they are `NULL`,
#' length-zero, `NA`, empty strings, or whitespace-only strings.
#'
#' @param query A query argument to validate.
#'
#' @return Logical scalar indicating whether `query` should be treated as empty.
#' @export
#' @keywords internal
#' @noRd
is_empty_query <- function(query) {
  is.null(query) || length(query) == 0 || is.na(query[[1]]) || !nzchar(trimws(query[[1]]))
}
