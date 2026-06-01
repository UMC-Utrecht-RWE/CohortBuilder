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
  logger::log_info(c("[MATCHING] - Retrieving matching variables"))

  # Check which variables are missing from the input data
  missing_vars <- setdiff(matching_vars, colnames(eligible_pop))
  if (length(missing_vars) > 0) {
    logger::log_info(warning(paste0(
      "The following variables are missing from the input data and will not be used for matching: ",
      paste(missing_vars, collapse = ", ")
    )))
  }

  # Update matching_vars to only include variables present in the input data
  matching_vars <- intersect(matching_vars, colnames(eligible_pop))

  # Inform the user about the variables that will actually be used
  logger::log_info(paste0("The following variables will be used for matching: ", paste(matching_vars, collapse = ", ")))

  logger::log_info(c("[MATCHING] - Matching variables retrieved successfully"))

  return(matching_vars)
}
