#' CohortBuilder: Bootstrap SQL-Based Matched Study Cohort Builder
#'
#' Constructs matched study cohorts by matching eligible exposed individuals to
#' eligible controls based on predefined profile variables and time windows.
#' Matching is executed in DuckDB using SQL and optionally repeated via
#' bootstrap resampling.
#'
#' @docType package
#' @name CohortBuilder-package
#'
#' @import data.table
"_PACKAGE"

#' @importFrom stats rpois runif setNames
"_PACKAGE"
