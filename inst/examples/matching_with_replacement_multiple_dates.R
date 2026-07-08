################################
#### Set generic parameters ####
################################

library(CohortBuilder)
library(data.table)

# What variables will we match on?
matching_vars <- c(
  "SV_SEX", "SV_REGION", "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"
)

# What column will hold the id to the persons?
col_person_id <- "person_id"

# Directories
dir_input <- "input"
dir_int_outputs <- "intermediate_outputs"
dir_matching_db <- "intermediate_outputs/matching.duckdb"
dir_log <- "log"
dir_bootstrap <- "intermediate_outputs/bootstraps"

# Matching with optional bootstrap
save_output <- TRUE
save_intermediate_matching_ouputs <- TRUE
with_bootstrap <- FALSE
n_bootstraps <- 5

# What will be our start seed
start_seed <- 42

# What will be the memory limit to the matching procedure?
memory_limit <- "8GB"

# What will be our output folder?
dir_outputs <- "outputs"

# create directories if they don't exist
dir.create(dir_input, showWarnings = FALSE)
dir.create(dir_int_outputs, showWarnings = FALSE)
dir.create(dir_log, showWarnings = FALSE)
dir.create(dir_bootstrap, showWarnings = FALSE)
dir.create(dir_outputs, showWarnings = FALSE)

#####################################################
#### Set parameters for the synthetic input data ####
#####################################################

# Will we run the synthetic data generation?
simulate_d3_eligibility <- TRUE

# The folder where we'll put our synthetic output data in
dir_output_sim_data <- "input"

# How many spells will we create
n_simulated_spells <- 1e6

# Will we save the simulated output data?
save_simulated_d3_eligibility <- TRUE

####################################################################################
#### Create the synthetic input data if we set simulate_d3_eligibility to TRUE  ####
####################################################################################

if (simulate_d3_eligibility) {
  D3_ELIGIBILITY <- generate_eligibility_data(
    n = n_simulated_spells,
    start_seed = start_seed,
    save_output = save_simulated_d3_eligibility,
    output_dir = dir_output_sim_data,
    output_file = "D3_ELIGIBILITY"
  )
} else {
  D3_ELIGIBILITY <- arrow::read_parquet(file.path(dir_input, "D3_ELIGIBILITY.parquet"))
}

# Add lmp_date and one_more_date columns to the eligibility data for testing date matching
D3_ELIGIBILITY[SV_PREG_STATUS == TRUE, lmp_date := start + sample(0:3, .N, replace = TRUE)]
# D3_ELIGIBILITY[, lmp_date := start + sample(0:3, .N, replace = TRUE)]
D3_ELIGIBILITY[, one_more_date := start + sample(0:30, .N, replace = TRUE)]

####################################################
#### Run matching pipeline (with-replacement mode) #
####################################################

study_cohort <- build_study_cohort(
  eligible_pop = D3_ELIGIBILITY,
  matching_vars = matching_vars,
  date_match_pars = list(
    col_date_match = c("lmp_date", "one_more_date"),
    date_match_offsets = c(lmp_date = 5, one_more_date = 30) # Allow a 1-day difference in lmp_date and 30-day difference in one_more_date for matching
  ),
  dir_matching_db = dir_matching_db,
  n_cores = NULL,
  log_name = "log_build_study_cohort",
  output_pars = list(
    save_output = save_output,
    dir_output = dir_outputs,
    output_file_name = "D4_StudyCohort"
  ),
  intermediate_output_pars = list(
    save_intermediate_outputs = save_intermediate_matching_ouputs,
    dir_intermediate_outputs = dir_int_outputs,
    profile_table_name = "D3_LOOKUP_TABLE",
    matching_pop_name = "D3_MATCHING_POP"
  ),
  bootstrap_pars = list(
    with_bootstrap = with_bootstrap,
    n_bootstraps = n_bootstraps,
    dir_bootstrap = dir_bootstrap,
    start_seed = start_seed
  ),
  input_column_names = list(
    col_person_id = "person_id",
    col_eligible_exposed = "eligible_exposed",
    col_eligible_control = "eligible_control",
    col_matching_status_start = "start",
    col_matching_status_end = "end",
    col_age_iterator = "year_of_birth"
  ),
  output_column_names = list(
    col_person_id = "person_id",
    col_match_id = "match_id",
    col_treatment_group = "group",
    col_T0 = "T0"
  ),
  age_offset = 1,
  matching_mode = "with_replacement"
)

# Check output
data.table::setorder(study_cohort, match_id, group)
head(study_cohort[group != "UNMATCHED"])
head(study_cohort[group != "UNMATCHED" & SV_PREG_STATUS == TRUE])

# Check the differences in lmp_date and one_more_date within matched pairs
matched_pairs <- study_cohort[group != "UNMATCHED"]
matched_pairs[, lmp_diff := as.integer(max(lmp_date) - min(lmp_date)), by = match_id]
matched_pairs[, one_more_diff := as.integer(max(one_more_date) - min(one_more_date)), by = match_id]

summary(matched_pairs$lmp_diff)
summary(matched_pairs$one_more_diff)
