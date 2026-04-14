# CohortBuilder

An R package for building matched study cohorts using DuckDB-backed SQL matching, with optional bootstrap resampling.

## Overview

`CohortBuilder` constructs matched study cohorts by matching eligible exposed individuals to eligible controls based on predefined profile variables and time windows. Matching is executed in DuckDB using SQL and optionally repeated via bootstrap resampling.

The main entry point is:

```r
build_study_cohort()
```

which orchestrates all steps from eligibility input to the final matched dataset.

## Installation

```r
# Install from local source
devtools::install("/Users/smildine/Documents/GitHub/CohortBuilder")
```

## Usage

Example scripts are shipped with the package and can be copied to a working directory:

```r
# Matching with replacement (optional bootstrap)
file.copy(
  system.file("examples", "matching.R", package = "CohortBuilder"),
  "matching.R"
)

# Matching without replacement (greedy)
file.copy(
  system.file("examples", "matching_without_replacement.R", package = "CohortBuilder"),
  "matching_without_replacement.R"
)
```

SQL query templates are resolved automatically via `system.file()`:

```r
matching_query <- getSQL(
  system.file("sql_queries", "matching_query_without_replacement.sql", package = "CohortBuilder")
)
```

## Functions

| Function                              | Description                                     |
| ------------------------------------- | ----------------------------------------------- |
| `build_study_cohort()`                | Main wrapper — runs the full matching pipeline  |
| `generate_eligibility_data()`         | Generate synthetic eligibility data for testing |
| `get_matching_variables()`            | Validate and subset matching variables          |
| `get_profile_table()`                 | Build the groupkey lookup table                 |
| `get_matching_population()`           | Prepare the exposed/control pool                |
| `match_cohorts()`                     | SQL matching with optional bootstrap            |
| `match_cohorts_without_replacement()` | Greedy no-replacement matching                  |
| `getSQL()`                            | Read a `.sql` file into a single string         |
| `toc_log_print()`                     | Log elapsed time from a `tictoc` timer          |

## Prerequisites

```r
install.packages(c("arrow", "data.table", "DBI", "duckdb", "logr", "tictoc"))
```

## Matching modes

| Mode                | Argument                                | Description                             |
| ------------------- | --------------------------------------- | --------------------------------------- |
| With replacement    | `matching_mode = "with_replacement"`    | SQL birth-year loop, supports bootstrap |
| Without replacement | `matching_mode = "without_replacement"` | Greedy round-based, no bootstrap        |
