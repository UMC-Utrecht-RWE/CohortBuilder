# Matching diagnostics -----------------------------------------------------

.cb_safe_divide <- function(numerator, denominator) {
  result <- rep(NA_real_, length(numerator))
  valid <- !is.na(denominator) & denominator > 0
  result[valid] <- numerator[valid] / denominator[valid]
  result
}

.cb_format_level <- function(values, na_label = "(Missing)") {
  values_chr <- as.character(values)
  values_chr[is.na(values_chr)] <- na_label
  values_chr
}

.cb_build_variable_level_summary <- function(eligible_dt,
                                             matched_dt,
                                             matching_vars,
                                             input_column_names,
                                             output_column_names,
                                             level_na_label) {
  summaries <- vector("list", length(matching_vars))

  for (index in seq_along(matching_vars)) {
    var_name <- matching_vars[[index]]

    eligible_summary <- eligible_dt[
      ,
      .(
        eligible_exposed_records = sum(get(input_column_names$col_eligible_exposed) == TRUE, na.rm = TRUE),
        eligible_control_records = sum(get(input_column_names$col_eligible_control) == TRUE, na.rm = TRUE),
        eligible_exposed_persons = data.table::uniqueN(
          as.character(get(input_column_names$col_person_id))[get(input_column_names$col_eligible_exposed) == TRUE]
        ),
        eligible_control_persons = data.table::uniqueN(
          as.character(get(input_column_names$col_person_id))[get(input_column_names$col_eligible_control) == TRUE]
        )
      ),
      by = .(variable_level = get(var_name))
    ]

    matched_summary <- matched_dt[
      ,
      .(
        matched_exposed_records = sum(get(output_column_names$col_treatment_group) == "EXPOSED", na.rm = TRUE),
        unmatched_exposed_records = sum(get(output_column_names$col_treatment_group) == "UNMATCHED", na.rm = TRUE),
        matched_control_records = sum(get(output_column_names$col_treatment_group) == "CONTROL", na.rm = TRUE),
        matched_exposed_persons = data.table::uniqueN(
          as.character(get(output_column_names$col_person_id))[get(output_column_names$col_treatment_group) == "EXPOSED"]
        ),
        unmatched_exposed_persons = data.table::uniqueN(
          as.character(get(output_column_names$col_person_id))[get(output_column_names$col_treatment_group) == "UNMATCHED"]
        ),
        matched_control_persons = data.table::uniqueN(
          as.character(get(output_column_names$col_person_id))[get(output_column_names$col_treatment_group) == "CONTROL"]
        )
      ),
      by = .(variable_level = get(var_name))
    ]

    merged_summary <- merge(
      eligible_summary,
      matched_summary,
      by = "variable_level",
      all = TRUE,
      sort = FALSE
    )

    count_columns <- setdiff(colnames(merged_summary), "variable_level")
    for (count_column in count_columns) {
      data.table::set(
        merged_summary,
        which(is.na(merged_summary[[count_column]])),
        count_column,
        0
      )
    }

    merged_summary[
      ,
      `:=`(
        matching_variable = var_name,
        variable_level = .cb_format_level(variable_level, na_label = level_na_label),
        exposed_match_rate_records = .cb_safe_divide(matched_exposed_records, eligible_exposed_records),
        exposed_match_rate_persons = .cb_safe_divide(matched_exposed_persons, eligible_exposed_persons),
        unmatched_rate_records = .cb_safe_divide(unmatched_exposed_records, eligible_exposed_records),
        unmatched_rate_persons = .cb_safe_divide(unmatched_exposed_persons, eligible_exposed_persons),
        control_supply_ratio_records = .cb_safe_divide(eligible_control_records, eligible_exposed_records),
        control_supply_ratio_persons = .cb_safe_divide(eligible_control_persons, eligible_exposed_persons)
      )
    ]

    data.table::setcolorder(
      merged_summary,
      c(
        "matching_variable", "variable_level",
        "eligible_exposed_records", "matched_exposed_records", "unmatched_exposed_records",
        "eligible_control_records", "matched_control_records",
        "eligible_exposed_persons", "matched_exposed_persons", "unmatched_exposed_persons",
        "eligible_control_persons", "matched_control_persons",
        "exposed_match_rate_records", "unmatched_rate_records", "control_supply_ratio_records",
        "exposed_match_rate_persons", "unmatched_rate_persons", "control_supply_ratio_persons"
      )
    )

    summaries[[index]] <- merged_summary[]
  }

  summary_dt <- data.table::rbindlist(summaries, use.names = TRUE, fill = TRUE)
  data.table::setorder(summary_dt, matching_variable, -unmatched_exposed_records, variable_level)
  summary_dt
}

.cb_build_profile_summary <- function(eligible_dt,
                                      matched_dt,
                                      matching_vars,
                                      input_column_names,
                                      output_column_names,
                                      top_n_profiles) {
  eligible_profile_summary <- eligible_dt[
    ,
    .(
      eligible_exposed_records = sum(get(input_column_names$col_eligible_exposed) == TRUE, na.rm = TRUE),
      eligible_control_records = sum(get(input_column_names$col_eligible_control) == TRUE, na.rm = TRUE)
    ),
    by = matching_vars
  ]

  matched_profile_summary <- matched_dt[
    ,
    .(
      matched_exposed_records = sum(get(output_column_names$col_treatment_group) == "EXPOSED", na.rm = TRUE),
      unmatched_exposed_records = sum(get(output_column_names$col_treatment_group) == "UNMATCHED", na.rm = TRUE),
      matched_control_records = sum(get(output_column_names$col_treatment_group) == "CONTROL", na.rm = TRUE)
    ),
    by = matching_vars
  ]

  profile_summary <- merge(
    eligible_profile_summary,
    matched_profile_summary,
    by = matching_vars,
    all = TRUE,
    sort = FALSE
  )

  count_columns <- setdiff(colnames(profile_summary), matching_vars)
  for (count_column in count_columns) {
    data.table::set(
      profile_summary,
      which(is.na(profile_summary[[count_column]])),
      count_column,
      0
    )
  }

  profile_summary[
    ,
    `:=`(
      exposed_match_rate_records = .cb_safe_divide(matched_exposed_records, eligible_exposed_records),
      unmatched_rate_records = .cb_safe_divide(unmatched_exposed_records, eligible_exposed_records),
      control_supply_ratio_records = .cb_safe_divide(eligible_control_records, eligible_exposed_records)
    )
  ]

  data.table::setorder(profile_summary, -unmatched_exposed_records, -eligible_exposed_records)
  profile_summary <- head(profile_summary, as.integer(top_n_profiles))

  if (nrow(profile_summary) > 0L) {
    profile_summary[, profile_signature := do.call(paste, c(.SD, sep = " | ")), .SDcols = matching_vars]
    data.table::setcolorder(
      profile_summary,
      c(
        "profile_signature", matching_vars,
        "eligible_exposed_records", "matched_exposed_records", "unmatched_exposed_records",
        "eligible_control_records", "matched_control_records",
        "exposed_match_rate_records", "unmatched_rate_records", "control_supply_ratio_records"
      )
    )
  }

  profile_summary[]
}

.cb_build_variable_bottleneck_summary <- function(eligible_dt,
                                                  matching_vars,
                                                  variable_level_summary,
                                                  input_column_names) {
  eligible_profile_summary <- eligible_dt[
    ,
    .(
      eligible_exposed_records = sum(get(input_column_names$col_eligible_exposed) == TRUE, na.rm = TRUE),
      eligible_control_records = sum(get(input_column_names$col_eligible_control) == TRUE, na.rm = TRUE)
    ),
    by = matching_vars
  ]

  total_exposed_records <- eligible_profile_summary[, sum(eligible_exposed_records)]
  bottleneck_rows <- vector("list", length(matching_vars))

  for (index in seq_along(matching_vars)) {
    var_name <- matching_vars[[index]]
    other_vars <- setdiff(matching_vars, var_name)

    if (length(other_vars) == 0L) {
      relaxed_controls <- eligible_profile_summary[, sum(eligible_control_records)]
      relaxed_summary <- data.table::copy(eligible_profile_summary)
      relaxed_summary[, relaxed_control_records := relaxed_controls]
    } else {
      relaxed_controls <- eligible_profile_summary[
        ,
        .(relaxed_control_records = sum(eligible_control_records, na.rm = TRUE)),
        by = other_vars
      ]

      relaxed_summary <- merge(
        eligible_profile_summary,
        relaxed_controls,
        by = other_vars,
        all.x = TRUE,
        sort = FALSE
      )
    }

    var_level_summary <- variable_level_summary[
      matching_variable == var_name & eligible_exposed_records > 0
    ]

    worst_level_row <- if (nrow(var_level_summary) > 0L) {
      var_level_summary[order(exposed_match_rate_records, -eligible_exposed_records, variable_level)][1L]
    } else {
      NULL
    }

    bottleneck_rows[[index]] <- relaxed_summary[
      ,
      .(
        matching_variable = var_name,
        exposed_records = sum(eligible_exposed_records),
        unsupported_exposed_records = sum(eligible_exposed_records[eligible_control_records == 0], na.rm = TRUE),
        exposed_records_gaining_support_if_dropped = sum(
          eligible_exposed_records[
            eligible_control_records == 0 & relaxed_control_records > 0
          ],
          na.rm = TRUE
        ),
        exposed_records_in_low_supply_profiles = sum(
          eligible_exposed_records[eligible_control_records < eligible_exposed_records],
          na.rm = TRUE
        )
      )
    ][
      ,
      `:=`(
        gain_share_of_all_exposed = .cb_safe_divide(exposed_records_gaining_support_if_dropped, exposed_records),
        gain_share_of_unsupported = .cb_safe_divide(exposed_records_gaining_support_if_dropped, unsupported_exposed_records),
        low_supply_share_of_all_exposed = .cb_safe_divide(exposed_records_in_low_supply_profiles, exposed_records),
        zero_control_levels = nrow(var_level_summary[eligible_control_records == 0]),
        worst_level = if (is.null(worst_level_row)) NA_character_ else worst_level_row$variable_level,
        worst_level_match_rate_records = if (is.null(worst_level_row)) NA_real_ else worst_level_row$exposed_match_rate_records,
        median_level_match_rate_records = if (nrow(var_level_summary) == 0L) {
          NA_real_
        } else {
          stats::weighted.mean(
            x = var_level_summary$exposed_match_rate_records,
            w = pmax(var_level_summary$eligible_exposed_records, 1),
            na.rm = TRUE
          )
        },
        share_of_total_exposed = .cb_safe_divide(exposed_records, total_exposed_records)
      )
    ]
  }

  bottleneck_summary <- data.table::rbindlist(bottleneck_rows, use.names = TRUE, fill = TRUE)
  data.table::setorder(
    bottleneck_summary,
    -exposed_records_gaining_support_if_dropped,
    -unsupported_exposed_records,
    worst_level_match_rate_records
  )
  bottleneck_summary[]
}

.cb_build_matching_plots <- function(variable_level_summary, variable_bottleneck_summary) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(list(
      status_distribution_by_level = NULL,
      level_match_rate_heatmap = NULL,
      support_gain_by_variable = NULL
    ))
  }

  status_distribution_data <- data.table::rbindlist(
    list(
      variable_level_summary[, .(matching_variable, variable_level, status = "Matched exposed", count = matched_exposed_records)],
      variable_level_summary[, .(matching_variable, variable_level, status = "Unmatched exposed", count = unmatched_exposed_records)],
      variable_level_summary[, .(matching_variable, variable_level, status = "Matched control", count = matched_control_records)]
    ),
    use.names = TRUE,
    fill = TRUE
  )

  status_distribution_plot <- ggplot2::ggplot(
    status_distribution_data,
    ggplot2::aes(x = variable_level, y = count, fill = status)
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::facet_wrap(~matching_variable, scales = "free_x") +
    ggplot2::scale_fill_manual(
      values = c(
        "Matched exposed" = "#1b9e77",
        "Unmatched exposed" = "#d95f02",
        "Matched control" = "#4c78a8"
      )
    ) +
    ggplot2::labs(
      title = "Matched and unmatched distributions by matching variable",
      x = NULL,
      y = "Record count",
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )

  heatmap_data <- variable_level_summary[eligible_exposed_records > 0]

  level_match_rate_heatmap <- ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(x = variable_level, y = matching_variable, fill = exposed_match_rate_records)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(
      low = "#d95f02",
      high = "#1b9e77",
      na.value = "#d9d9d9",
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = "Exposed match rate by matching variable level",
      x = NULL,
      y = NULL,
      fill = "Match rate"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  support_gain_by_variable <- ggplot2::ggplot(
    variable_bottleneck_summary,
    ggplot2::aes(
      x = stats::reorder(matching_variable, exposed_records_gaining_support_if_dropped),
      y = exposed_records_gaining_support_if_dropped
    )
  ) +
    ggplot2::geom_col(fill = "#4c78a8") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Exposed records that would gain control support if one variable were dropped",
      x = NULL,
      y = "Exposed records gaining support"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  list(
    status_distribution_by_level = status_distribution_plot,
    level_match_rate_heatmap = level_match_rate_heatmap,
    support_gain_by_variable = support_gain_by_variable
  )
}

.save_matching_diagnostics <- function(diagnostics, output_dir) {
  .ensure_directory(output_dir, "diagnostic output")

  for (table_name in names(diagnostics$tables)) {
    table_value <- diagnostics$tables[[table_name]]
    if (inherits(table_value, "data.table") || inherits(table_value, "data.frame")) {
      data.table::fwrite(table_value, file.path(output_dir, paste0(table_name, ".csv")))
    }
  }

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (plot_name in names(diagnostics$plots)) {
      plot_value <- diagnostics$plots[[plot_name]]
      if (!is.null(plot_value)) {
        ggplot2::ggsave(
          filename = file.path(output_dir, paste0(plot_name, ".png")),
          plot = plot_value,
          width = 10,
          height = 6,
          dpi = 300
        )
      }
    }
  }
}

#' Generate Matching Diagnostics
#'
#' Builds descriptive diagnostics that help identify limiting matching variables.
#' The returned object contains tables showing exposed, control, and unmatched
#' distributions by matching variable level, a summary of high-unmatched matching
#' profiles, and a variable-relaxation summary estimating which matching variable
#' most constrains control support.
#'
#' @param eligible_pop A data frame containing the eligible population before matching.
#' @param matched_cohort A data frame containing the final matched cohort output.
#' @param matching_vars Character vector of matching variables used in the cohort build.
#' @param input_column_names A named list describing the input column names used by
#'   `eligible_pop`.
#' @param output_column_names A named list describing the output column names used by
#'   `matched_cohort`.
#' @param top_n_profiles Integer number of high-unmatched profiles to retain in the
#'   profile bottleneck table. Default is `20`.
#' @param include_plots Logical. If `TRUE`, returns `ggplot2` diagnostics when the
#'   package is installed.
#' @param level_na_label Character label used for missing matching-variable levels.
#'
#' @return An object of class `"matching_diagnostics"` with `tables`, `plots`, and
#'   `metadata` components.
#'
#' @details
#' The diagnostics are designed to answer two related questions: where unmatched
#' exposed records accumulate, and which matching variable is most likely limiting
#' support. The variable-relaxation summary does not rerun the full matcher. Instead,
#' it collapses one matching variable at a time and measures how many exposed records
#' would gain at least some eligible control support under that relaxation.
#'
#' @examples
#' \dontrun{
#' diagnostics <- get_matching_diagnostics(
#'   eligible_pop = D3_ELIGIBILITY,
#'   matched_cohort = D4_MSC,
#'   matching_vars = matching_vars
#' )
#'
#' diagnostics$tables$variable_level_summary
#' plot(diagnostics, type = "support_gain_by_variable")
#' }
#'
#' @export
get_matching_diagnostics <- function(eligible_pop = NULL,
                                     matched_cohort = NULL,
                                     matching_vars = NULL,
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
                                     top_n_profiles = 20,
                                     include_plots = TRUE,
                                     level_na_label = "(Missing)") {
  if (is.null(eligible_pop) || is.null(matched_cohort)) {
    stop("`eligible_pop` and `matched_cohort` must both be provided.")
  }

  if (is.null(matching_vars) || length(matching_vars) == 0L) {
    stop("`matching_vars` must contain at least one matching variable.")
  }

  eligible_dt <- data.table::as.data.table(data.table::copy(eligible_pop))
  matched_dt <- data.table::as.data.table(data.table::copy(matched_cohort))

  required_input_columns <- c(
    input_column_names$col_person_id,
    input_column_names$col_eligible_exposed,
    input_column_names$col_eligible_control,
    matching_vars
  )
  missing_input_columns <- setdiff(required_input_columns, colnames(eligible_dt))
  if (length(missing_input_columns) > 0L) {
    stop(paste0("Missing columns in `eligible_pop`: ", paste(missing_input_columns, collapse = ", "), "."))
  }

  required_output_columns <- c(
    output_column_names$col_person_id,
    output_column_names$col_treatment_group,
    matching_vars
  )
  missing_output_columns <- setdiff(required_output_columns, colnames(matched_dt))
  if (length(missing_output_columns) > 0L) {
    stop(paste0("Missing columns in `matched_cohort`: ", paste(missing_output_columns, collapse = ", "), "."))
  }

  for (var_name in matching_vars) {
    eligible_dt[, (var_name) := .cb_format_level(get(var_name), na_label = level_na_label)]
    matched_dt[, (var_name) := .cb_format_level(get(var_name), na_label = level_na_label)]
  }

  variable_level_summary <- .cb_build_variable_level_summary(
    eligible_dt = eligible_dt,
    matched_dt = matched_dt,
    matching_vars = matching_vars,
    input_column_names = input_column_names,
    output_column_names = output_column_names,
    level_na_label = level_na_label
  )

  variable_bottleneck_summary <- .cb_build_variable_bottleneck_summary(
    eligible_dt = eligible_dt,
    matching_vars = matching_vars,
    variable_level_summary = variable_level_summary,
    input_column_names = input_column_names
  )

  profile_bottleneck_summary <- .cb_build_profile_summary(
    eligible_dt = eligible_dt,
    matched_dt = matched_dt,
    matching_vars = matching_vars,
    input_column_names = input_column_names,
    output_column_names = output_column_names,
    top_n_profiles = top_n_profiles
  )

  plots <- if (isTRUE(include_plots)) {
    .cb_build_matching_plots(variable_level_summary, variable_bottleneck_summary)
  } else {
    list(
      status_distribution_by_level = NULL,
      level_match_rate_heatmap = NULL,
      support_gain_by_variable = NULL
    )
  }

  diagnostics <- list(
    tables = list(
      variable_level_summary = variable_level_summary,
      variable_bottleneck_summary = variable_bottleneck_summary,
      profile_bottleneck_summary = profile_bottleneck_summary
    ),
    plots = plots,
    metadata = list(
      matching_vars = matching_vars,
      generated_at = Sys.time(),
      top_n_profiles = as.integer(top_n_profiles),
      level_na_label = level_na_label
    )
  )

  class(diagnostics) <- "matching_diagnostics"
  diagnostics
}

#' @export
print.matching_diagnostics <- function(x, ...) {
  cat("Matching diagnostics\n")
  cat("Variables:", paste(x$metadata$matching_vars, collapse = ", "), "\n")
  cat("Tables:", paste(names(x$tables), collapse = ", "), "\n")

  top_summary <- x$tables$variable_bottleneck_summary
  if (!is.null(top_summary) && nrow(top_summary) > 0L) {
    cat("Top limiting variables by support gain if dropped:\n")
    print(utils::head(top_summary[, .(
      matching_variable,
      exposed_records_gaining_support_if_dropped,
      unsupported_exposed_records,
      worst_level,
      worst_level_match_rate_records
    )], 5L))
  }

  invisible(x)
}

#' @export
plot.matching_diagnostics <- function(x,
                                      y = NULL,
                                      type = c(
                                        "status_distribution_by_level",
                                        "level_match_rate_heatmap",
                                        "support_gain_by_variable"
                                      ),
                                      ...) {
  type <- match.arg(type)
  plot_value <- x$plots[[type]]

  if (is.null(plot_value)) {
    stop("Requested plot is not available. Install 'ggplot2' and generate diagnostics with `include_plots = TRUE`.")
  }

  print(plot_value)
  invisible(plot_value)
}
