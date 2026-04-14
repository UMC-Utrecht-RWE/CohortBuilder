################################
#### Set generic parameters ####
################################

library(CohortBuilder)
library(data.table)

# What variables will we match on?
matching_vars <- c(
  "SV_SEX", "SV_REGION", "SV_HIST_COVID_VACC", "SV_PRIOR_COVID_DG", "SV_BRAND_COVID_VACC",
  "SV_PREG_STATUS", "SV_IMMUNOCOMPROMISED", "CDC_RISK", "SV_SES_STATUS"
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

##########################
#### Load SQL queries ####
##########################

matching_query <- suppressWarnings(
  getSQL(system.file("sql_queries", "matching_query_with_replacement.sql", package = "CohortBuilder"))
)
target_table_query <- getSQL(
  system.file("sql_queries", "create_matching_target_table.sql", package = "CohortBuilder")
)

####################################################
#### Run matching pipeline (with-replacement mode) #
####################################################

build_study_cohort(
  eligible_pop = D3_ELIGIBILITY,
  matching_query = matching_query,
  target_table_query = target_table_query,
  matching_vars = matching_vars,
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
    col_matching_status_start = "matching_status_start",
    col_matching_status_end = "matching_status_end",
    col_age_iterator = "year_of_birth"
  ),
  output_column_names = list(
    col_person_id = "person_id",
    col_match_id = "match_id",
    col_treatment_group = "group",
    col_T0 = "T0"
  ),
  matching_mode = "with_replacement"
)
