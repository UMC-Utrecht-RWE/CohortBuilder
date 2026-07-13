# S3 methods for matching study cohort output ----------------------------

#' Internal: Log cohort summary via logger
#'
#' Writes structured summary statistics for a matched study cohort directly
#' to the active logger log, using `hide_notes = TRUE` on all intermediate
#' lines so timestamps do not appear between every row.
#'
#' @param object A data.table with class "matching_study_cohort".
#' @noRd
.log_matching_summary <- function(object, col_person_id = "person_id") {
  if (!inherits(object, "data.table")) {
    object <- data.table::as.data.table(object)
  }

  .log_table_lines <- function(x) {
    out <- capture.output(print(x, row.names = FALSE))
    if (length(out) == 0L) {
      return(invisible(NULL))
    }
    for (line in out) {
      logger::log_info(line)
    }
    invisible(NULL)
  }

  required_cols <- c("group", col_person_id)
  if (!all(required_cols %in% names(object))) {
    logger::log_info("[MATCHING] - Skipping cohort summary: 'group' or '", col_person_id,  "' column not found.")
    return(invisible(NULL))
  }

  logger::log_info("=== Matched Study Cohort Summary ===", hide_notes = TRUE)
  logger::log_info("", hide_notes = TRUE)

  # --- Episodes by group ---
  logger::log_info("Episodes by Matching Status:", hide_notes = TRUE)
  logger::log_info("---------------------------------", hide_notes = TRUE)
  episodes_by_group <- object[, .(N = .N), by = .(group)]
  episodes_by_group[, pct := round(100 * N / sum(N), 2L)]
  episodes_by_group <- episodes_by_group[order(-N)]
  .log_table_lines(episodes_by_group)
  logger::log_info("", hide_notes = TRUE)
  logger::log_info(paste0("Total Episodes: ", nrow(object)), hide_notes = TRUE)

  # --- Control usage distribution ---
  logger::log_info("", hide_notes = TRUE)
  logger::log_info("Distribution of Control Usage (matched groups):", hide_notes = TRUE)
  logger::log_info("---------------------------------", hide_notes = TRUE)
  controls <- object[group == "CONTROL"]

  if (nrow(controls) > 0L) {
    control_usage <- controls[, .(N_used = .N), by = get(col_person_id)]
    control_usage_dist <- control_usage[
      ,
      .(
        n_controls = .N,
        pct_controls = round(100 * .N / nrow(control_usage), 2L)
      ),
      by = .(times_used = N_used)
    ][order(times_used)]

    .log_table_lines(control_usage_dist)
    logger::log_info("", hide_notes = TRUE)
    logger::log_info(paste0("Total Unique Control Person IDs: ", nrow(control_usage)), hide_notes = TRUE)
    logger::log_info(paste0("Total Control Episodes: ", nrow(controls)), hide_notes = TRUE)
    logger::log_info(paste0("Average Uses per Control: ", round(mean(control_usage$N_used), 2L)), hide_notes = TRUE)
    logger::log_info(paste0("Max Uses for Single Control: ", max(control_usage$N_used)))
  } else {
    logger::log_info("No matched controls found.")
  }
}

#' Summary Method for Matching Study Cohort
#'
#' Provides summary statistics for a matched study cohort, including:
#' - Count of episodes by matching status (EXPOSED, CONTROL, UNMATCHED)
#' - Distribution of how many times each control person was used
#'
#' @param object A data.table with class "matching_study_cohort" (output from
#'   [build_study_cohort()]).
#' @param ... Additional arguments (unused).
#'
#' @return A list (invisibly) with:
#'   \describe{
#'     \item{`episodes_by_group`}{Data frame showing count and percentage of episodes by group.}
#'     \item{`control_usage_distribution`}{Data frame showing distribution of how many times each control person was used.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- build_study_cohort(...)
#' summary(result)
#' }
#'
#' @export
#' @export summary.matching_study_cohort
summary.matching_study_cohort <- function(object, ...) {
  if (!inherits(object, "data.table")) {
    object <- data.table::as.data.table(object)
  }

  required_cols <- c("group", col_person_id)
  missing_cols <- setdiff(required_cols, names(object))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". Expected 'group' and '",
      col_person_id,
      " columns.",
      call. = FALSE
    )
  }

  cat("\n")
  cat("=== Matched Study Cohort Summary ===\n")
  cat("\n")

  cat("Episodes by Matching Status:\n")
  cat("---------------------------------\n")
  episodes_by_group <- object[, .(N = .N), by = .(group)]
  episodes_by_group[, pct := round(100 * N / sum(N), 2L)]
  episodes_by_group <- episodes_by_group[order(-N)]

  print(episodes_by_group, row.names = FALSE)

  cat("\n")
  cat("Total Episodes: ", nrow(object), "\n", sep = "")

  cat("\n")
  cat("Distribution of Control Usage (matched groups):\n")
  cat("---------------------------------\n")
  controls <- object[group == "CONTROL"]

  control_usage_dist <- NULL
  if (nrow(controls) > 0L) {
    control_usage <- controls[, .(N_used = .N), by = get(col_person_id)]
    control_usage_dist <- control_usage[
      ,
      .(
        n_controls = .N,
        pct_controls = round(100 * .N / nrow(control_usage), 2L)
      ),
      by = .(times_used = N_used)
    ]
    control_usage_dist <- control_usage_dist[order(times_used)]

    print(control_usage_dist, row.names = FALSE)

    cat("\n")
    cat("Total Unique Control Person IDs: ", nrow(control_usage), "\n", sep = "")
    cat("Total Control Episodes: ", nrow(controls), "\n", sep = "")
    cat("Average Uses per Control: ", round(mean(control_usage$N_used), 2L), "\n", sep = "")
    cat("Max Uses for Single Control: ", max(control_usage$N_used), "\n", sep = "")
  } else {
    cat("No matched controls found.\n")
  }

  cat("\n")
  invisible(
    list(
      episodes_by_group = episodes_by_group,
      control_usage_distribution = control_usage_dist
    )
  )
}

#' Plot Method for Matching Study Cohort
#'
#' Creates stacked-count plots for matched study cohort data.
#'
#' By default, this produces a stacked histogram of matched episodes by `group`
#' over `T0` (index date), with automatic binning. You can also provide any
#' other variable in `by_var` to generate a stacked bar plot over that
#' variable's levels.
#'
#' @param x A data.table with class "matching_study_cohort" (output from
#'   [build_study_cohort()]).
#' @param y Unused; provided for S3 generic compatibility.
#' @param by_var Character. Variable to plot on the category axis.
#'   Default is `"T0"`.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 plot object (if ggplot2 is installed), or a base R plot.
#'   If ggplot2 is available, returns a ggplot object invisibly.
#'   If ggplot2 is not available, generates a base R stacked bar/column plot.
#'
#' @examples
#' \dontrun{
#' result <- build_study_cohort(...)
#' # Default: stacked histogram over T0
#' plot(result)
#'
#' # Stacked bars over a user-specified variable
#' plot(result, by_var = "group")
#' }
#'
#' @export
#' @export plot.matching_study_cohort
plot.matching_study_cohort <- function(x, y, by_var = "T0", ...) {
  if (missing(y)) {
    y <- NULL
  }

  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
  }

  required_cols <- c("group", by_var)
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data_plot <- data.table::copy(x)
  is_time_axis <- identical(by_var, "T0")

  if (is_time_axis && !inherits(data_plot[[by_var]], "Date")) {
    converted <- try(as.Date(data_plot[[by_var]]), silent = TRUE)
    if (inherits(converted, "try-error") || all(is.na(converted) & !is.na(data_plot[[by_var]]))) {
      stop(
        "Could not convert '", by_var, "' to Date for stacked time plot.",
        call. = FALSE
      )
    }
    data_plot[[by_var]] <- converted
  }

  if (!is_time_axis) {
    data_plot[[by_var]] <- as.character(data_plot[[by_var]])
    data_plot[[by_var]][is.na(data_plot[[by_var]])] <- "(Missing)"
  }

  if (is_time_axis) {
    non_missing_time <- data_plot[[by_var]][!is.na(data_plot[[by_var]])]
    n_bins <- .cb_sensible_hist_bins(non_missing_time)
  } else {
    plot_data <- data_plot[, .(n_matches = .N), by = .(plot_level = get(by_var), group)]
    level_totals <- stats::aggregate(n_matches ~ plot_level, data = as.data.frame(plot_data), FUN = sum)
    level_order <- level_totals$plot_level[order(-level_totals$n_matches)]
    plot_data[["plot_level"]] <- factor(plot_data[["plot_level"]], levels = level_order)
  }

  has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

  if (has_ggplot2) {
    if (is_time_axis) {
      plot_obj <- .plot_msc_hist_ggplot(data_plot, by_var = by_var, n_bins = n_bins)
    } else {
      plot_obj <- .plot_msc_stacked_ggplot(plot_data, by_var = by_var)
    }
    print(plot_obj)
    return(invisible(plot_obj))
  }

  if (is_time_axis) {
    .plot_msc_hist_base(data_plot, by_var = by_var, n_bins = n_bins)
  } else {
    .plot_msc_stacked_base(plot_data, by_var = by_var)
  }
  invisible(NULL)
}

#' Internal: Choose sensible histogram bin count
#'
#' @param values A vector of Date values.
#'
#' @return Integer number of histogram bins.
#' @noRd
.cb_sensible_hist_bins <- function(values) {
  values_num <- as.numeric(values)
  values_num <- values_num[is.finite(values_num)]

  if (length(values_num) <= 1L) {
    return(1L)
  }

  fd_bins <- try(grDevices::nclass.FD(values_num), silent = TRUE)
  if (inherits(fd_bins, "try-error") || !is.finite(fd_bins) || fd_bins < 1L) {
    fd_bins <- 30L
  }

  as.integer(max(8L, min(60L, round(fd_bins))))
}

#' Internal: Create ggplot2 stacked histogram for matching study cohort
#'
#' @param data A data.table with matching cohort rows.
#' @param by_var Character. Name of the time variable.
#' @param n_bins Integer number of bins.
#'
#' @return A ggplot2 plot object.
#' @noRd
.plot_msc_hist_ggplot <- function(data, by_var = "T0", n_bins = 30L) {
  group_palette <- c(EXPOSED = "#1b9e77", CONTROL = "#d95f02", UNMATCHED = "#7570b3")

  ggplot2::ggplot(
    data,
    ggplot2::aes_string(x = by_var, fill = "group")
  ) +
    ggplot2::geom_histogram(position = "stack", bins = n_bins, color = NA, na.rm = TRUE) +
    ggplot2::scale_fill_manual(
      name = "Matching Status",
      values = group_palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      title = paste0("Matched Episodes by Group Over ", by_var, " (Binned)"),
      x = by_var,
      y = "Number of Matches"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      legend.position = "right"
    )
}

#' Internal: Create ggplot2 stacked plot for matching study cohort
#'
#' @param data A data.table with aggregated matching cohort counts.
#' @param by_var Character. Name of the plotted variable.
#'
#' @return A ggplot2 plot object.
#' @noRd
.plot_msc_stacked_ggplot <- function(data, by_var = "T0") {
  group_palette <- c(EXPOSED = "#1b9e77", CONTROL = "#d95f02", UNMATCHED = "#7570b3")

  ggplot2::ggplot(
    data,
    ggplot2::aes_string(x = "plot_level", y = "n_matches", fill = "group")
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      name = "Matching Status",
      values = group_palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      title = paste0("Matched Episodes by Group Over ", by_var),
      x = by_var,
      y = "Number of Matches"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      legend.position = "right"
    ) +
    ggplot2::coord_flip()
}

#' Internal: Create base R stacked histogram for matching study cohort
#'
#' @param data A data.table with matching cohort rows.
#' @param by_var Character. Name of the time variable.
#' @param n_bins Integer number of bins.
#'
#' @noRd
.plot_msc_hist_base <- function(data, by_var = "T0", n_bins = 30L) {
  data_hist <- data.table::copy(data)
  values_num <- as.numeric(data_hist[[by_var]])

  finite_values <- values_num[is.finite(values_num)]
  if (length(finite_values) == 0L) {
    stop("No non-missing values available for histogram plotting.", call. = FALSE)
  }

  x_min <- min(finite_values)
  x_max <- max(finite_values)

  if (x_min == x_max) {
    breaks <- c(x_min - 0.5, x_max + 0.5)
  } else {
    breaks <- seq(from = x_min, to = x_max, length.out = as.integer(n_bins) + 1L)
  }

  data_hist[["hist_bin"]] <- cut(values_num, breaks = breaks, include.lowest = TRUE, right = FALSE)
  data_hist <- data_hist[!is.na(data_hist[["hist_bin"]])]

  mat <- stats::xtabs(~ group + hist_bin, data = data_hist)
  group_palette <- c(EXPOSED = "#1b9e77", CONTROL = "#d95f02", UNMATCHED = "#7570b3")
  cols <- group_palette[rownames(mat)]

  graphics::barplot(
    mat,
    beside = FALSE,
    col = cols,
    xlab = paste0(by_var, " (binned)"),
    ylab = "Number of Matches",
    main = paste0("Matched Episodes by Group Over ", by_var, " (Binned)"),
    las = 2
  )

  graphics::legend(
    "topright",
    legend = rownames(mat),
    fill = cols,
    cex = 0.9
  )

  invisible(NULL)
}

#' Internal: Create base R stacked plot for matching study cohort
#'
#' @param data A data.table with aggregated matching cohort counts.
#' @param by_var Character. Name of the plotted variable.
#'
#' @noRd
.plot_msc_stacked_base <- function(data, by_var = "T0") {
  data_for_xtabs <- data.table::copy(data)
  data_for_xtabs[["plot_level"]] <- as.character(data_for_xtabs[["plot_level"]])
  mat <- stats::xtabs(n_matches ~ group + plot_level, data = data_for_xtabs)

  group_palette <- c(EXPOSED = "#1b9e77", CONTROL = "#d95f02", UNMATCHED = "#7570b3")
  cols <- group_palette[rownames(mat)]

  graphics::barplot(
    mat,
    beside = FALSE,
    horiz = TRUE,
    col = cols,
    xlab = "Number of Matches",
    ylab = by_var,
    main = paste0("Matched Episodes by Group Over ", by_var),
    las = 1
  )

  graphics::legend(
    "topright",
    legend = rownames(mat),
    fill = cols,
    cex = 0.9
  )

  invisible(NULL)
}
