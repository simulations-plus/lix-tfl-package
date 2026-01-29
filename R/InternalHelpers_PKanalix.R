################################################################################
#-------------------Quarto Report: Non Compartmental analysis------------------#
#----------------------------Internal Helpers----------------------------------#
################################################################################

# Internal Helper Functions
# These functions are prefixed with a dot (.) and are used internally by
# the public PKanalix helper functions.

##################################################################################
#' ConvertHtmlAliasesForFlextable
#'
#' @description
#' Transforms HTML-formatted parameter aliases (containing <sub>, <sup> tags) into
#' flextable-compatible placeholder format. This function is part of the table
#' formatting pipeline and maintains separation between data helpers and presentation
#' logic.
#'
#' @details
#' This function converts HTML markup to neutral placeholders that can be decoded
#' later by flextable rendering functions. The conversion happens in two stages:
#'
#' 1. HTML tags → Placeholders: <sub>text</sub> becomes [[sub:text]]
#' 2. Cleanup: Any remaining HTML tags are stripped
#'
#' **Design Philosophy:**
#' - Table helpers should be presentation-agnostic (don't know about flextable)
#' - Placeholders serve as an intermediate format between HTML and flextable
#' - Actual rendering (as_sub(), as_sup()) happens in print_table_flex()
#'
#' @param df A data frame containing an "aliases" column with HTML-formatted text.
#'   Typically obtained from getNCAParameterStatistics()$information
#'
#' @return A data frame identical to input but with the "aliases" column transformed:
#'   - HTML subscript tags converted to [[sub:...]] placeholders
#'   - All other HTML tags removed
#'   - Plain text preserved as-is
#'
#' applied i helper function .addUnitsAliases()
#'
.convertHtmlAliasesForFlextable <- function(df) {
    convert_sub_and_sup <- function(alias) {
        # Convert <sub>text</sub> to [[sub:text]] placeholder
        alias_clean <- stringr::str_replace_all(alias, "<sub>(.*?)</sub>", "[[sub:\\1]]")
        # Remove any remaining HTML tags
        stringr::str_remove_all(alias_clean, "<[^>]+>")
    }

    df %>%
        dplyr::mutate(!!"aliases" := vapply(.data[["aliases"]], convert_sub_and_sup, character(1)))
}


##################################################################################
# .addUnitsAliases
#
# Add unit and alias columns to a parameter dataframe
#
# @param param_df Data frame containing NCA parameter names in a column named "parameter"
# @param units Logical; if TRUE, adds units information
# @param aliases Logical; if TRUE, adds alias information
#
# @return The input data frame with additional columns:
#   - "unit" if units=TRUE (else column is omitted)
#   - "aliases" if aliases=TRUE (else column is omitted)
#
# @keywords internal
# @noRd
.addUnitsAliases <- function(param_df, units = FALSE, aliases = FALSE) {
    if (!units && !aliases) {
        return(param_df)
    }

    # Fetch PKanalix parameter information (units, aliases, etc.)
    param_info <- lixoftConnectors::getNCAParameterStatistics()$information
    param_info_clean <- as.data.frame(param_info, stringsAsFactors = FALSE)

    # Convert HTML aliases to flextable-compatible format (if aliases requested)
    if (aliases) {
        param_info_clean <- .convertHtmlAliasesForFlextable(param_info_clean)
    }

    # Prepare columns to join
    cols_to_join <- "parameter"
    if (units) cols_to_join <- c(cols_to_join, "unit")
    if (aliases) cols_to_join <- c(cols_to_join, "aliases")

    param_info_subset <- param_info_clean %>%
        dplyr::select(dplyr::all_of(cols_to_join)) %>%
        dplyr::distinct()

    # Left join to preserve all rows in param_df
    result <- param_df %>%
        dplyr::left_join(param_info_subset, by = "parameter")

    return(result)
}


################################################################################
# Helpers for rounding
#
# .round_num
#
# Round numeric columns in a dataframe using the specified method.
#
# @param to_round Data frame with numeric columns to round
# @param digits Integer number of digits for rounding (default 3)
# @param rounding_method Character; one of "monolix", "signif", or "none"
# @param exclude Character vector of column names to exclude from rounding
#
# @return Data frame with numeric columns rounded according to the method
#
# @keywords internal
# @noRd
.round_num <- function(to_round, digits = 3, rounding_method = "monolix", exclude = NULL) {
    if (rounding_method == "none") {
        return(to_round)
    }

    numeric_cols <- names(to_round)[sapply(to_round, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, exclude)

    if (length(numeric_cols) == 0) {
        return(to_round)
    }

    if (rounding_method == "monolix") {
        # Monolix-style rounding: add extra digits for values < 1
        to_round <- to_round %>%
            dplyr::mutate(dplyr::across(
                dplyr::all_of(numeric_cols),
                ~ ifelse(abs(.x) < 1 & !is.na(.x),
                    round(.x, digits + 2),
                    round(.x, digits)
                )
            ))
    } else if (rounding_method == "signif") {
        # Significant figures rounding
        to_round <- to_round %>%
            dplyr::mutate(dplyr::across(
                dplyr::all_of(numeric_cols),
                ~ signif(.x, digits)
            ))
    }

    return(to_round)
}


################################################################################
# Helpers for summarizing statistics
#
# .summarizeStats
#
# Compute summary statistics (N, mean, SD, CV, geometric mean, geometric CV,
# median, min, max, Q1, Q3, P05, P95) for a numeric variable grouped by
# specified columns.
#
# @param df Data frame containing the data
# @param group_vars Character vector of column names to group by
# @param value_col Character; name of the numeric column to summarize
# @param id_col Character; name of the ID column for counting unique subjects
#
# @return Data frame with one row per group and columns for each statistic
#
# @keywords internal
# @noRd
.summarizeStats <- function(df, group_vars, value_col, id_col = "id") {
    df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::summarise(
            N = dplyr::n_distinct(.data[[id_col]]),
            mean = mean(.data[[value_col]], na.rm = TRUE),
            SD = sd(.data[[value_col]], na.rm = TRUE),
            CV = sd(.data[[value_col]], na.rm = TRUE) / mean(.data[[value_col]], na.rm = TRUE) * 100,
            geoMean = exp(mean(log(.data[[value_col]]), na.rm = TRUE)),
            geoCV = sqrt(exp(sd(log(.data[[value_col]]), na.rm = TRUE)^2) - 1) * 100,
            geoSD = exp(sd(log(.data[[value_col]]), na.rm = TRUE)),
            median = median(.data[[value_col]], na.rm = TRUE),
            min = min(.data[[value_col]], na.rm = TRUE),
            max = max(.data[[value_col]], na.rm = TRUE),
            Q1 = quantile(.data[[value_col]], 0.25, na.rm = TRUE),
            Q3 = quantile(.data[[value_col]], 0.75, na.rm = TRUE),
            P05 = quantile(.data[[value_col]], 0.05, na.rm = TRUE),
            P95 = quantile(.data[[value_col]], 0.95, na.rm = TRUE),
            .groups = "drop"
        )
}


################################################################################
# .combinedStats
#
# Add combined statistic columns (MeanSD, GeomCV, GeomSD) to a summary table.
# These are formatted strings combining related metrics for compact reporting.
#
# @param summary_tbl Data frame containing columns: mean, SD, geoMean, geoCV, geoSD
#
# @return Data frame with three additional columns: MeanSD, GeomCV, GeomSD
#
# @keywords internal
# @noRd
.combinedStats <- function(summary_tbl) {
    summary_tbl %>%
        dplyr::mutate(
            MeanSD = paste0(mean, " (", SD, ")"),
            GeomCV = paste0(geoMean, " (", geoCV, ")"),
            GeomSD = paste0(geoMean, " (", geoSD, ")")
        )
}


################################################################################
# .validateAndSelectMetrics
#
# Validate user-specified metrics against available columns and return
# the final list of metrics to include in the output.
#
# @param available_cols Character vector of all column names in the data
# @param user_metrics Character vector of metrics requested by user (NULL means all)
# @param descriptors Character vector of non-metric columns (e.g., "parameter", "Time")
#
# @return Character vector of validated metric column names
#
# @keywords internal
# @noRd
.validateAndSelectMetrics <- function(available_cols, user_metrics = NULL, descriptors = NULL) {
    # Identify available metric columns (exclude descriptors)
    available_metrics <- setdiff(available_cols, descriptors)

    if (is.null(user_metrics)) {
        # If no metrics specified, return all available
        return(available_metrics)
    }

    # Check which requested metrics are available
    valid_metrics <- intersect(user_metrics, available_metrics)
    invalid_metrics <- setdiff(user_metrics, available_metrics)

    if (length(invalid_metrics) > 0) {
        stop(
            "Invalid metric(s) requested: ", paste(invalid_metrics, collapse = ", "), "\n",
            "Available metrics: ", paste(available_metrics, collapse = ", ")
        )
    }

    if (length(valid_metrics) == 0) {
        stop("No valid metrics found in the requested list.")
    }

    return(valid_metrics)
}


################################################################################
# .validateStratificationArgs
#
# Validate stratification logic:
# 1. If stratify_columns=TRUE but covariates=NULL, reset to FALSE and warn
# 2. If covariates provided, validate they exist in the current PKanalix project
#
# @param covariates Character vector of covariate names (or NULL)
# @param stratify_columns Logical; whether to stratify columns
#
# @return Logical value for stratify_columns after validation
#
# @keywords internal
# @noRd
.validateStratificationArgs <- function(covariates, stratify_columns) {
    # 1. Argument consistency check
    if (isTRUE(stratify_columns) && is.null(covariates)) {
        warning("stratify_columns=TRUE requires covariates. Resetting stratify_columns to FALSE.")
        return(FALSE)
    }

    # 2. Covariate existence check (if covariates provided)
    if (!is.null(covariates)) {
        # Get available covariates from current PKanalix project
        cov_info <- lixoftConnectors::getCovariateInformation()$covariate
        available_covariates <- setdiff(names(cov_info), "id")

        # Validate requested covariates exist
        bad_covariates <- setdiff(covariates, available_covariates)
        if (length(bad_covariates) > 0) {
            stop(
                "Invalid covariate(s): ", paste(bad_covariates, collapse = ", "), "\n",
                "Available covariates: ", paste(available_covariates, collapse = ", ")
            )
        }
    }

    return(stratify_columns)
}


################################################################################
# .validateCovariateLabels
#
# Validate that covariate labels match the number of covariates.
#
# @param covariates Character vector of covariate names (or NULL)
# @param covariate_labels Character vector of labels (or NULL)
#
# @return Validated covariate_labels (or NULL)
#
# @keywords internal
# @noRd
.validateCovariateLabels <- function(covariates, covariate_labels) {
    if (!is.null(covariate_labels)) {
        if (is.null(covariates)) {
            warning("covariate_labels provided but no covariates specified. Ignoring labels.")
            return(NULL)
        }
        if (length(covariate_labels) != length(covariates)) {
            stop(
                "Length of covariate_labels (", length(covariate_labels), ") ",
                "must match length of covariates (", length(covariates), ")"
            )
        }
    }
    return(covariate_labels)
}


################################################################################
# .checkProjectFile
#
# Check if projectFile is provided and if it's a valid file path with correct extension.
#
# @param projectFile Path to project file
# @param expected_ext Expected file extension (e.g., "pkx", "mlxtran")
#
# @keywords internal
# @noRd
.checkProjectFile <- function(projectFile, expected_ext) {
    if (is.null(projectFile)) {
        return(invisible(NULL))
    }

    if (!file.exists(projectFile)) {
        stop("Project file not found: ", projectFile)
    }

    ext <- tools::file_ext(projectFile)
    if (tolower(ext) != tolower(expected_ext)) {
        warning(
            "Expected .", expected_ext, " file but got .", ext, " file: ", projectFile
        )
    }

    return(invisible(NULL))
}


################################################################################
# .ensureProjectLoaded
#
# Load a project file if it's different from the currently loaded one.
#
# @param projectFile Path to project file (or NULL to skip)
#
# @keywords internal
# @noRd
.ensureProjectLoaded <- function(projectFile) {
    if (is.null(projectFile)) {
        return(invisible(NULL))
    }

    current_project <- tryCatch(
        lixoftConnectors::getProjectSettings()$filepath,
        error = function(e) NULL
    )

    if (is.null(current_project) || normalizePath(current_project, mustWork = FALSE) != normalizePath(projectFile, mustWork = FALSE)) {
        message("Loading project: ", projectFile)
        lixoftConnectors::loadProject(projectFile)
    }

    return(invisible(NULL))
}


################################################################################
# .prepareNCASummaryStats
#
# Fetch and prepare NCA summary statistics from PKanalix with optional
# stratification, rounding, units, and aliases.
#
# @param project Path to PKanalix project (or NULL for current project)
# @param NCAparams Character vector of NCA parameters to include (NULL = all)
# @param covariates Character vector of covariates for stratification (NULL = overall)
# @param roundDigits Number of digits for rounding
# @param roundMethod Rounding method: "monolix", "signif", or "none"
# @param units Logical; add units column
# @param aliases Logical; add aliases column
# @param by_time Logical; if TRUE, includes time-based results (for concentration tables)
# @param useNominalTime Logical; use nominal time if available
#
# @return Data frame with NCA summary statistics
#
# @keywords internal
# @noRd
.prepareNCASummaryStats <- function(project = NULL, NCAparams = NULL, covariates = NULL,
                                    roundDigits = 3, roundMethod = "monolix",
                                    units = TRUE, aliases = TRUE,
                                    by_time = FALSE, useNominalTime = FALSE) {
    # Ensure correct project is loaded
    .checkProjectFile(project, "pkanalix")
    .ensureProjectLoaded(project)

    # Set stratification (or reset to overall)
    if (!is.null(covariates)) {
        lixoftConnectors::setNCAResultsStratification(covariates)
    } else {
        lixoftConnectors::setNCAResultsStratification()
    }

    # Fetch summary statistics
    if (by_time) {
        # For concentration tables (by time)
        if (useNominalTime) {
            # Check if nominal time is available
            data_info <- lixoftConnectors::getData()
            has_nominal <- any(data_info$headerTypes == "nominaltime")

            if (has_nominal) {
                summary_stats <- lixoftConnectors::getNCAObservedConcentrationsStatistics(useNominalTime = TRUE)
            } else {
                warning("Nominal time requested but not available. Using actual time instead.")
                summary_stats <- lixoftConnectors::getNCAObservedConcentrationsStatistics(useNominalTime = FALSE)
            }
        } else {
            summary_stats <- lixoftConnectors::getNCAObservedConcentrationsStatistics(useNominalTime = FALSE)
        }
    } else {
        # For parameter tables
        summary_stats <- lixoftConnectors::getNCAParameterStatistics()$statistics
    }

    if (is.null(summary_stats) || nrow(summary_stats) == 0) {
        return(NULL)
    }

    summary_stats <- as.data.frame(summary_stats, stringsAsFactors = FALSE)

    # Filter to specific parameters if requested
    if (!is.null(NCAparams) && "parameter" %in% names(summary_stats)) {
        summary_stats <- summary_stats %>%
            dplyr::filter(parameter %in% NCAparams)
    }

    # Apply rounding
    exclude_cols <- c("parameter", "Time", covariates)
    summary_stats <- .round_num(summary_stats, roundDigits, roundMethod, exclude_cols)

    # Add units and/or aliases if requested
    if ((units || aliases) && "parameter" %in% names(summary_stats)) {
        summary_stats <- .addUnitsAliases(summary_stats, units, aliases)
    }

    return(summary_stats)
}


################################################################################
# .prettyLabels
#
# Apply pretty formatting to column names in a data frame.
# Transforms technical column names into more readable display names.
#
# @param df Data frame with column names to prettify
#
# @return Data frame with renamed columns
#
# @keywords internal
# @noRd
.prettyLabels <- function(df) {
    # Define mapping of technical names to pretty names
    name_map <- c(
        "parameter" = "Parameter",
        "Time" = "Time",
        "Nobs" = "N",
        "N" = "N",
        "mean" = "Mean",
        "SD" = "SD",
        "CV" = "CV%",
        "geoMean" = "Geometric Mean",
        "geoCV" = "Geometric CV%",
        "geoSD" = "Geometric SD",
        "median" = "Median",
        "min" = "Min",
        "max" = "Max",
        "Q1" = "Q1",
        "Q3" = "Q3",
        "P05" = "5th Percentile",
        "P95" = "95th Percentile",
        "MeanSD" = "Mean (SD)",
        "GeomCV" = "Geometric Mean (CV%)",
        "GeomSD" = "Geometric Mean (SD)"
    )

    current_names <- names(df)
    new_names <- ifelse(current_names %in% names(name_map),
        name_map[current_names],
        current_names
    )

    names(df) <- new_names
    return(df)
}


################################################################################
# .mockTableForFlextable
#
# Create a mocked version of a table for flextable preview or testing.
# Limits the number of rows and optionally excludes specific columns.
#
# @param df Data frame to mock
# @param mock_settings List with elements:
#   - enable: Logical; if FALSE, return df unchanged
#   - max_rows: Integer or "all"; maximum number of rows to include
#   - exclude_cols: Character vector of column names to exclude
#
# @return Mocked data frame (or original if mock disabled)
#
# @keywords internal
# @noRd
.mockTableForFlextable <- function(df, mock_settings = list(enable = FALSE, max_rows = 5, exclude_cols = NULL)) {
    if (!isTRUE(mock_settings$enable)) {
        return(df)
    }

    # Exclude specified columns
    if (!is.null(mock_settings$exclude_cols)) {
        cols_to_keep <- setdiff(names(df), mock_settings$exclude_cols)
        df_mocked <- df[, cols_to_keep, drop = FALSE]
    } else {
        df_mocked <- df
    }

    # Limit rows
    max_rows_setting <- mock_settings$max_rows
    if (is.numeric(max_rows_setting)) {
        n_limit <- min(nrow(df), max_rows_setting)
    } else {
        # Default behavior (NULL, "all", or invalid input) -> Show ALL rows
        n_limit <- nrow(df)
    }

    df_mocked <- df_mocked[1:n_limit, , drop = FALSE]

    return(df_mocked)
}


################################################################################
# Pivot and Stratification Helpers
################################################################################

#' Helper: Pivot metrics with optional overall column
#'
#' @param df_strat Stratified data with header column
#' @param overall_df Overall data (optional)
#' @param selected_metrics Metrics to pivot
#' @param param_col Parameter column name
#' @param n_per_stratum Data frame with header and header_N mapping
#' @keywords internal
#' @noRd
.pivotMetricsWithOverall <- function(df_strat, overall_df = NULL, selected_metrics,
                                     param_col, n_per_stratum = NULL) {
    if (length(selected_metrics) > 1) {
        # === MULTIPLE METRICS: Pivot to 'Statistic' column ===

        # Pivot Stratified
        result <- df_strat %>%
            dplyr::select(!!rlang::sym(param_col), header, dplyr::all_of(selected_metrics)) %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
            tidyr::pivot_wider(names_from = header, values_from = Value)

        # Add N to column headers if available
        if (!is.null(n_per_stratum)) {
            result <- result %>%
                dplyr::rename_with(~ ifelse(
                    .x %in% n_per_stratum$header,
                    n_per_stratum$header_N[match(.x, n_per_stratum$header)],
                    .x
                ))
        }

        # Join with Overall if provided
        if (!is.null(overall_df)) {
            overall_pivoted <- overall_df %>%
                dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
                tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Overall") %>%
                dplyr::select(!!rlang::sym(param_col), Statistic, Overall, header_N)

            overall_label <- unique(overall_pivoted$header_N)
            result <- result %>%
                dplyr::left_join(overall_pivoted %>% dplyr::select(-header_N), by = c(param_col, "Statistic")) %>%
                dplyr::rename(!!overall_label := Overall)
        }
    } else {
        # === SINGLE METRIC ===
        metric_name <- selected_metrics[1]

        result <- df_strat %>%
            dplyr::select(!!rlang::sym(param_col), header, dplyr::all_of(metric_name)) %>%
            tidyr::pivot_wider(names_from = header, values_from = dplyr::all_of(metric_name))

        # Add N to column headers if available
        if (!is.null(n_per_stratum)) {
            result <- result %>%
                dplyr::rename_with(~ ifelse(
                    .x %in% n_per_stratum$header,
                    n_per_stratum$header_N[match(.x, n_per_stratum$header)],
                    .x
                ))
        }

        # Join with Overall if provided
        if (!is.null(overall_df)) {
            overall_renamed <- overall_df %>%
                dplyr::rename(Overall = dplyr::all_of(metric_name)) %>%
                dplyr::select(!!rlang::sym(param_col), Overall, header_N)

            result <- result %>%
                dplyr::left_join(overall_renamed, by = param_col) %>%
                dplyr::rename(!!unique(overall_renamed$header_N) := Overall) %>%
                dplyr::select(-header_N)
        }
    }

    return(result)
}


#' Helper: Build stratified table with covariate headers
#'
#' @param summary_stats Stratified summary statistics
#' @param overall_stats Overall summary statistics (optional)
#' @param covariates Covariate names
#' @param selected_metrics Selected metrics to include
#' @param param_col Parameter column name
#' @param covariate_labels Custom labels for covariates (optional)
#' @keywords internal
#' @noRd
.buildStratifiedTable <- function(summary_stats, overall_stats = NULL, covariates,
                                  selected_metrics, param_col, covariate_labels = NULL) {
    if (!is.null(covariates)) {
        # Prepare covariate labels
        if (is.null(covariate_labels)) labels <- covariates else labels <- covariate_labels

        # Create header column combining covariates
        df_prepared <- summary_stats %>%
            dplyr::rowwise() %>%
            dplyr::mutate(header = {
                values <- dplyr::c_across(dplyr::all_of(covariates))
                pieces <- paste0(labels, "=", as.character(values))
                paste(pieces, collapse = "\n")
            }) %>%
            dplyr::ungroup()

        # Map headers to N counts
        n_per_stratum <- df_prepared %>%
            distinct(header, Nobs) %>%
            dplyr::mutate(header_N = paste0(header, "\n(N=", Nobs, ")"))

        # Prepare overall data if provided
        overall_prepared <- NULL
        if (!is.null(overall_stats)) {
            overall_prepared <- overall_stats %>%
                dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                dplyr::mutate(header_N = paste0("Overall\n(N=", Nobs, ")"))
        }

        # Use pivot helper
        result <- .pivotMetricsWithOverall(
            df_strat         = df_prepared,
            overall_df       = overall_prepared,
            selected_metrics = selected_metrics,
            param_col        = param_col,
            n_per_stratum    = n_per_stratum
        )
    } else {
        # No covariates - just Overall with N
        max_n <- max(summary_stats$Nobs, na.rm = TRUE)

        if (length(selected_metrics) > 1) {
            result <- summary_stats %>%
                dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                dplyr::mutate(header_N = paste0("Overall\n(N=", max_n, ")")) %>%
                dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
                tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
                tidyr::pivot_wider(names_from = header_N, values_from = Value) %>%
                dplyr::select(-dplyr::any_of("Nobs"))
        } else {
            result <- summary_stats %>%
                dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                dplyr::rename(!!paste0("Overall\n(N=", max_n, ")") := dplyr::all_of(selected_metrics)) %>%
                dplyr::select(-dplyr::any_of("Nobs"))
        }
    }

    return(result)
}


#' Helper: Pivot table to vertical format (Statistic as first column)
#'
#' @param summary_stats Summary statistics data frame
#' @param param_col Parameter column name
#' @param covariates Covariate names to preserve
#' @keywords internal
#' @noRd
.pivotToVertical <- function(summary_stats, param_col, covariates = NULL) {
    stat_cols <- setdiff(names(summary_stats), c(param_col, covariates))
    param_order <- unique(summary_stats[[param_col]])
    stat_order <- stat_cols

    result <- summary_stats %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), as.character)) %>%
        tidyr::pivot_longer(
            cols      = dplyr::all_of(stat_cols),
            names_to  = "Statistic",
            values_to = "Value"
        ) %>%
        dplyr::mutate(Statistic = factor(Statistic, levels = stat_order)) %>%
        tidyr::pivot_wider(
            id_cols     = c(dplyr::any_of(covariates), Statistic),
            names_from  = dplyr::all_of(param_col),
            values_from = Value
        ) %>%
        dplyr::arrange(dplyr::across(dplyr::any_of(covariates)), Statistic) %>%
        dplyr::select(dplyr::any_of(covariates), Statistic, dplyr::all_of(as.character(param_order)))

    return(result)
}
