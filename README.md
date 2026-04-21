# CohortBuilder

An R package for constructing matched study cohorts from an eligible population.

## What the package does

CohortBuilder helps you create an analysis-ready matched cohort by:

- validating matching variables,
- building matching profiles,
- preparing eligible exposed/control pools,
- and running matching in one of two modes:
  - with replacement (DuckDB SQL workflow, optional bootstrap),
  - without replacement (greedy round-based workflow).

The main function is `build_study_cohort()`, which orchestrates the full pipeline.

## Installation

### Using devtools:
1. Download this release and place the .tat.gz file in the desired local directory
2. Install using
```r
# Install from local source
devtools::install("/Users/smildine/Documents/GitHub/CohortBuilder")
```

### Using `renv`:

1. Download this release and place the .tat.gz file in the directory "renv/cellar"
2. Install for the first time using

```r
renv::install()
```

or if already installed, call

```r
renv::install('CohortBuilder', rebuild = TRUE)
```

### Using base install.packages:

1. Download this release and place the .tat.gz file in the desired local directory
2. Install using

```r
install.packages(
  pkgs = "/absolute/path/to/CohortBuilder_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

## Minimal example: without replacement

This example generates synthetic eligibility data and runs the pipeline in without-replacement mode.

```r
library(CohortBuilder)

# 1) Create a small synthetic eligible population
eligible_pop <- generate_eligibility_data(n = 10000, start_seed = 42)

# 2) Pick matching variables available in the synthetic data
matching_vars <- c(
  "SV_SEX",
  "SV_REGION",
  "SV_HIST_COVID_VACC",
  "SV_PRIOR_COVID_DG",
  "SV_BRAND_COVID_VACC",
  "SV_PREG_STATUS",
  "SV_IMMUNOCOMPROMISED",
  "CDC_RISK",
  "SV_SES_STATUS"
)

# 3) Run matching without replacement
cohort <- build_study_cohort(
  eligible_pop = eligible_pop,
  matching_vars = matching_vars,
  dir_matching_db = tempfile(fileext = ".duckdb"),
  output_pars = list(
    save_output = FALSE,
    dir_output = tempdir(),
    output_file_name = "D4_StudyCohort"
  ),
  intermediate_output_pars = list(
    save_intermediate_outputs = FALSE,
    dir_intermediate_outputs = tempdir(),
    profile_table_name = "D3_LOOKUP_TABLE",
    matching_pop_name = "D3_MATCHING_POP"
  ),
  bootstrap_pars = list(
    with_bootstrap = FALSE,
    n_bootstraps = 1,
    dir_bootstrap = tempdir(),
    start_seed = 42
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
  matching_mode = "without_replacement"
)

# 4) Inspect output
head(cohort)
```

## Matching modes

| Mode                | Argument                                | Description                             |
| ------------------- | --------------------------------------- | --------------------------------------- |
| With replacement    | `matching_mode = "with_replacement"`    | SQL birth-year loop, supports bootstrap |
| Without replacement | `matching_mode = "without_replacement"` | Greedy round-based, no bootstrap        |
