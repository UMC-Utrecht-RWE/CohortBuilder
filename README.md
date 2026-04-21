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
| `get_matching_diagnostics()`          | Summarize matching bottlenecks and diagnostics  |
| `match_cohorts_with_replacement()`    | SQL matching with optional bootstrap            |
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

## Diagnostics

Use `get_matching_diagnostics()` to inspect which matching variables and profile
combinations are driving unmatched exposed records.

```r
diagnostics <- get_matching_diagnostics(
  eligible_pop = D3_ELIGIBILITY,
  matched_cohort = D4_MSC,
  matching_vars = matching_vars
)

diagnostics$tables$variable_level_summary
diagnostics$tables$variable_bottleneck_summary
plot(diagnostics, type = "support_gain_by_variable")
```

You can also request diagnostics from `build_study_cohort()` by setting
`diagnostic_pars$compute_diagnostics = TRUE`. When the cohort is returned to R,
the diagnostics object is attached as the `"matching_diagnostics"` attribute.

## Summary and Plotting

The matched cohort object (returned by `build_study_cohort()`) has S3 methods for 
summary statistics and visualization:

### Summary Method

Get summary statistics including episode counts by group and control usage distribution:

```r
D4_MSC <- build_study_cohort(...)
summary(D4_MSC)
```

This prints:
- **Episodes by Matching Status**: Count and percentage of EXPOSED, CONTROL, and UNMATCHED records
- **Distribution of Control Usage**: How many times each control person was used in matching (unique controls used, min/max/average uses)

### Plot Method

Visualize matched episode counts as stacked plots by group:

```r
# Default: stacked histogram over T0
plot(D4_MSC)

# Stacked bars over any variable levels
plot(D4_MSC, by_var = "group")

# Example with another variable in the cohort output
plot(D4_MSC, by_var = "match_id")
```

By default this shows matched episodes (EXPOSED, CONTROL, UNMATCHED) over the index date (`T0`).
When `by_var` is provided, it shows stacked bars over levels of that variable.
If ggplot2 is installed, produces a publication-ready plot; otherwise falls back to base R graphics.
