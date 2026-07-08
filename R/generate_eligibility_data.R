#' Generate Simulated Eligibility Data
#'
#' This function generates a synthetic dataset to simulate eligibility data for analysis.
#' It creates attributes such as vaccination history, risk factors, and demographic details.
#' The dataset is customizable in size and can be saved to a specified directory.
#'
#' @param n Integer. Number of rows to generate in the synthetic dataset. Default is 10,000.
#' @param start_seed Integer. Seed for reproducibility of random number generation. Default is 42.
#' @param save_output Logical. If TRUE, the generated dataset will be saved to the specified directory.
#' @param output_dir Character. Directory where the dataset should be saved if `save_output` is TRUE.
#' @param output_file Character. Name of the output file (without extension). Default is `"D3_ELIGIBILITY"`.
#' @param n_spells Integer. Number of spells per person. Default is 2.
#'
#' @return A data.table containing the generated synthetic dataset with detailed eligibility attributes.
#'
#' @details
#' The function generates various attributes, including:
#' - **person_id**: Unique identifier for individuals.
#' - **SV_REGION**: Regional information (categorical).
#' - **CDC_RISK**: CDC-defined risk levels.
#' - **SV_HIST_COVID_VACC**: Historical COVID vaccination count.
#' - **SV_BRAND_COVID_VACC**: Brand of COVID vaccine received.
#' - **SV_IMMUNOCOMPROMISED**: Immunocompromised status.
#' - **SV_PREG_STATUS**: Pregnancy status.
#' - **COMP_COMORBIDITIES**: Presence of comorbidities.
#' - **receives_any_covidvaccine**: Indicates if any COVID vaccine was received.
#' - **receives_bivalent**: Indicates if a bivalent vaccine was received.
#' - **start**: Start date of matching eligibility.
#' - Additional derived variables like age, eligibility, and exposure status.
#'
#' If `save_output` is TRUE, the generated dataset is saved in `.parquet` format in the `output_dir`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a synthetic dataset with default parameters
#' dataset <- generate_eligibility_data()
#'
#' # Generate and save a dataset
#' dataset <- generate_eligibility_data(
#'   n = 5000,
#'   start_seed = 123,
#'   save_output = TRUE,
#'   output_dir = "data/",
#'   output_file = "SyntheticEligibility"
#' )
#' }
generate_eligibility_data <- function(n = 10000, start_seed = 42, save_output, output_dir, output_file = "D3_ELIGIBILITY", n_spells = 2) {
  logger::log_info(paste0("[MATCHING] - Creating synthetic ", output_file))

  # Set seed for reproducibility
  set.seed(start_seed)

  # Generate base data using data.table
  D3_ELIGIBILITY <- data.table::data.table(
    person_id = sample(paste0("Subject_", 1:(n / n_spells)), n, replace = TRUE),
    SV_REGION = sample(c(NA, 1, 2, 3), n, replace = TRUE, prob = c(0.05, 0.3, 0.4, 0.25)),
    CDC_RISK = sample(0:2, n, replace = TRUE),
    SV_HIST_COVID_VACC = rpois(n, 0.2),
    SV_BRAND_COVID_VACC = NA_character_,
    SV_PRIOR_COVID_DG = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
    SV_IMMUNOCOMPROMISED = sample(c(0, 1), n, replace = TRUE, prob = c(0.9, 0.1)),
    SV_PREG_STATUS = sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.95, 0.04, 0.01)),
    SV_SES_STATUS = sample(c(NA, 1, 2, 3), n, replace = TRUE, prob = c(0.05, 0.3, 0.4, 0.25)),
    COMP_COMORBIDITIES = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
    dead = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.01, 0.99)),
    receives_any_covidvaccine = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5)),
    receives_bivalent = FALSE,
    fivemonth_since_lastvac = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5)),
    elevenmonth_since_lastvac = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5)),
    prior_bivalent = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
    receives_first_bivalent = FALSE,
    start = sample(seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day"), n, replace = TRUE)
  )

  # Fill conditional columns
  D3_ELIGIBILITY[, SV_BRAND_COVID_VACC := ifelse(SV_HIST_COVID_VACC > 0,
    sample(c(NA, "Pfizer", "Moderna"), .N, replace = TRUE, prob = c(0.1, 0.45, 0.45)),
    NA
  )]
  D3_ELIGIBILITY[, receives_bivalent := ifelse(receives_any_covidvaccine,
    sample(c(TRUE, FALSE), .N, replace = TRUE, prob = c(0.9, 0.1)),
    FALSE
  )]
  D3_ELIGIBILITY[, receives_first_bivalent := receives_bivalent & !prior_bivalent]

  # Create a person-level table
  person_df <- unique(D3_ELIGIBILITY[, .(person_id)])
  person_df[, `:=`(
    year_of_birth = sample(1920:2024, .N, replace = TRUE),
    SV_SEX = sample(c("F", "M"), .N, replace = TRUE, prob = c(0.5, 0.5)),
    twelve_month_enrolment_or_enrolled_at_birth = sample(c(TRUE, FALSE), .N, replace = TRUE, prob = c(0.9, 0.1))
  )]

  # Join back to main data
  D3_ELIGIBILITY <- merge(D3_ELIGIBILITY, person_df, by = "person_id", all.x = TRUE)

  # Mutate threshold-related columns first
  D3_ELIGIBILITY[, `:=`(
    threshold_ba1_met = (as.numeric(format(start, "%Y")) - year_of_birth) >= 12,
    threshold_ba45_met = (as.numeric(format(start, "%Y")) - year_of_birth) >= 5
  )]

  # Mutate other related columns in sequence to avoid dependencies
  D3_ELIGIBILITY[, `:=`(
    any_age_threshold_met = threshold_ba1_met | threshold_ba45_met,
    comorbid_and_fivemonth = COMP_COMORBIDITIES & fivemonth_since_lastvac,
    notcomorbid_and_elevenmonth = !COMP_COMORBIDITIES & elevenmonth_since_lastvac
  )]

  D3_ELIGIBILITY[, `:=`(
    in_targeted_population = SV_HIST_COVID_VACC == 0 |
      (SV_HIST_COVID_VACC != 0 & any_age_threshold_met &
        (comorbid_and_fivemonth | notcomorbid_and_elevenmonth)),
    complete_information = !is.na(year_of_birth) & !is.na(SV_SEX) & !is.na(SV_REGION) &
      !is.na(SV_HIST_COVID_VACC) & !is.na(SV_BRAND_COVID_VACC) & !is.na(SV_PREG_STATUS) &
      !is.na(SV_IMMUNOCOMPROMISED) & !is.na(CDC_RISK) & !is.na(SV_SES_STATUS)
  )]

  D3_ELIGIBILITY[, `:=`(
    eligible_exposed = receives_first_bivalent & in_targeted_population &
      twelve_month_enrolment_or_enrolled_at_birth & complete_information,
    eligible_control = !receives_any_covidvaccine & !prior_bivalent &
      in_targeted_population & twelve_month_enrolment_or_enrolled_at_birth & complete_information
  )]

  # Add all additional columns (complete information, end date, older_than_sixty, etc.)
  D3_ELIGIBILITY[, `:=`(
    end = as.Date(ifelse(receives_first_bivalent == TRUE, start, start + as.difftime(60, units = "days"))),
    older_than_sixty = (as.numeric(format(start, "%Y")) - year_of_birth) > 60,
    bivalent_type_received = ifelse(
      receives_bivalent,
      sample(c("ba1", "ba45", "unknown", NA), n, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)),
      NA
    )
  )]

  # Filter for a single exposed spell per person
  D3_ELIGIBILITY <- D3_ELIGIBILITY[
    , .SD[eligible_exposed == FALSE | (.I == .I[eligible_exposed][1])],
    by = person_id
  ]

  logger::log_info(paste0("[MATCHING] - ", output_file, " created successfully"))

  if (save_output) {
    logger::log_info(paste0("[MATCHING] - Saving simulated data to ", output_dir, "/", output_file, ".parquet"))
    arrow::write_parquet(D3_ELIGIBILITY, paste0(file.path(output_dir, output_file), ".parquet"))
    logger::log_info(paste0("[MATCHING] - ", output_file, " saved successfully"))
  }

  return(D3_ELIGIBILITY)
}
