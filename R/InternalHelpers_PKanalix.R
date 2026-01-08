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
#' PrettyLabels
#'
#' Converts statistical parameter column names to human-readable, formatted labels
#' for presentation in tables and reports. Maps common statistical abbreviations
#' to their full, properly formatted names.
#'
#' @param df A data frame containing statistical columns to be relabeled.
#'
#' @return A data frame with column names converted to pretty, human-readable labels.
#'
#' @examples
#' # Example usage:
#' # df <- data.frame(parameter = "AUC", mean = 100, sd = 20, CV = 15)
#' # PrettyLabels(df)
#'
.prettyLabels <- function(df) {
    # Mapping of common stat names to pretty labels
    stat_labels <- c(
        "parameter"           = "Parameter",
        "mean"                = "Mean",
        "sd"                  = "SD",
        "se"                  = "SE",
        "CV"                  = "CV (%)",
        "min"                 = "Minimum",
        "max"                 = "Maximum",
        "median"              = "Median",
        "geoMean"             = "Geometric\nMean",
        "GeoMean"             = "Geometric\nMean",
        "geoSD"               = "Geometric\nSD",
        "geoCV"               = "Geometric\nCV (%)",
        "GeomCV"              = "Geometric\nMean (CV %)",
        "GeomSD"              = "Geometric\nMean (SD)",
        "MeanSD"              = "Arithmetric\nMean (SD)",
        "harmMean"            = "Harmonic\nMean",
        "P05"                 = "5th Percentile",
        "P95"                 = "95th Percentile",
        "N"                   = "N",
        "Ntot"                = "N Total",
        "Nobs"                = "N", # "N Observed"
        "Nmiss"               = "N Missing",
        "p.value"             = "p-value",
        "%RSE"                = "%RSE",
        "IIV %RSE"            = "IIV %RSE",
        "Dosing.range"        = "Dosing Range",
        "Point.estimate"      = "Point Estimate",
        "Criteria.interval"   = "Criteria Interval",
        "Confidence.interval" = "Confidence Interval"
    )

    # Replace column names where matches are found
    new_names <- names(df)
    new_names_pretty <- ifelse(
        new_names %in% names(stat_labels),
        stat_labels[new_names],
        new_names
    )
    names(df) <- new_names_pretty
    return(df)
}


##################################################################################
#' .addUnitsAliases
#'
#' Enhances parameter data frames by adding units and aliases to create formatted
#' display names. Combines parameter names with their corresponding units and aliases
#' to generate publication-ready parameter labels with proper fallback handling.
#'
#' @param df_param A data frame containing parameter information with a 'parameter' column.
#' @param df_units A data frame containing parameter units with 'parameter' and 'units' columns.
#'                 If NULL, no units are added.
#' @param df_aliases A data frame containing parameter aliases with 'parameter' and 'aliases' columns.
#'                   If NULL, no aliases are added.
#'
#' @return A data frame with formatted display names that combine aliases and units,
#'         maintaining the original parameter order.
#'
#' @examples
#' # Example usage:
#' # df_param <- data.frame(parameter = c("AUC", "Cmax"))
#' # df_units <- data.frame(parameter = c("AUC", "Cmax"), units = c("ng*h/mL", "ng/mL"))
#' # df_aliases <- data.frame(parameter = c("AUC", "Cmax"), aliases = c("AUC₀₋ₜ", "Cₘₐₓ"))
#' # AddUnitsAliases(df_param, df_units, df_aliases)
#'
#' #
.addUnitsAliases <- function(df, add_units = TRUE, add_aliases = TRUE) {
    # Retrieve all parameters from project
    allparams <- lixoftConnectors::getNCASettings()$computedncaparameters

    NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = allparams)$information %>%
        tibble::rownames_to_column(var = "parameter")


    # Check and retrieve units information (if requested)
    if (isTRUE(add_units)) {
        # extract units if active in project
        if (isTRUE(lixoftConnectors::getDataSettings()$enableunits)) {
            units_df <- NCAParamSummary_info %>%
                dplyr::select(parameter, units)
        } else {
            # warning if units are requested but not enabled
            warning("Units were requested (units=TRUE) but are not enabled in the current project. Skipping unit retrieval.")
            units_df <- NULL
        }
    } else {
        units_df <- NULL
    }

    # Check and retrieve aliases information (if requested)
    # ----------------------------------------------------------
    if (isTRUE(add_aliases)) {
        if ("aliases" %in% names(NCAParamSummary_info)) {
            aliases_df <- NCAParamSummary_info %>%
                dplyr::select(parameter, aliases) %>%
                .convertHtmlAliasesForFlextable()
        } else {
            warning("Aliases were requested (aliases=TRUE) but are not available in the current project. Skipping alias retrieval.")
            aliases_df <- NULL
        }
    } else {
        aliases_df <- NULL
    }

    # Save original order for later restoration
    original_order <- unique(df$parameter)

    # Merge units if provided
    if (!is.null(units_df)) {
        units_df <- units_df %>% dplyr::mutate(
            parameter = as.character(parameter),
            units = as.character(units)
        )
        df <- dplyr::left_join(df, units_df, by = "parameter")
    } else {
        df$units <- NA_character_
    }

    # Merge aliases if provided
    if (!is.null(aliases_df)) {
        aliases_df <- aliases_df %>% dplyr::mutate(
            parameter = as.character(parameter),
            aliases = as.character(aliases)
        )
        df <- dplyr::left_join(df, aliases_df, by = "parameter")
    } else {
        df$aliases <- NA_character_
    }

    # Coerce all relevant columns to character to prevent paste0() errors
    df <- df %>%
        dplyr::mutate(
            parameter = as.character(parameter),
            aliases = as.character(aliases),
            units = as.character(units)
        )

    # Build display name with proper fallback handling
    df <- df %>%
        dplyr::mutate(display_name = dplyr::case_when(
            !is.na(aliases) & !is.na(units) & units != "" ~ paste0(aliases, " (", units, ")"),
            !is.na(aliases) ~ aliases,
            is.na(aliases) & !is.na(units) & units != "" ~ paste0(parameter, " (", units, ")"),
            TRUE ~ parameter
        ))

    # Restore original order and finalize structure
    df <- df %>%
        dplyr::mutate(parameter = factor(parameter, levels = original_order)) %>%
        dplyr::arrange(parameter) %>%
        dplyr::relocate(display_name, .before = 1) %>%
        dplyr::select(-parameter, -units, -aliases) %>%
        dplyr::rename(parameter = display_name)

    return(df)
}


##################################################################################
#' RoundNumericCols
#'
#' Rounds all numeric columns in a data frame to a specified number of decimal places.
#' Also attempts to convert character columns that contain numeric values to numeric
#' before rounding, providing robust handling of mixed data types.
#'
#' @param df A data frame containing columns to be rounded.
#' @param digits An integer specifying the number of decimal places for rounding. Default is 3.
#'
#' @return A data frame with all numeric columns rounded to the specified number of decimal places.
#'
#' @examples
#' # Example usage:
#' # df <- data.frame(param = c("AUC", "Cmax"), value1 = c(123.456, 67.891), value2 = c("234.567", "89.123"))
#' # RoundNumericCols(df, digits = 2)
.roundNumericCols <- function(x,
                              digits = 3,
                              exclude = NULL,
                              rounding_method = c("monolix", "signif")) {
    rounding_method <- match.arg(rounding_method)

    # Inner function: rounds a single numeric vector
    .round_num <- function(number, digits, rounding_method) {
        # handle NA/NaN
        if (!is.numeric(number)) {
            number <- suppressWarnings(as.numeric(as.character(number)))
        }

        if (!is.numeric(number)) {
            return(number)
        }

        if (rounding_method == "monolix") {
            adj_digits <- digits + ifelse(
                abs(number) < 1 & number != 0 & !is.na(number),
                abs(floor(log10(abs(number))) + 1),
                0
            )
            rounded <- round(number, digits = adj_digits)
        } else {
            rounded <- signif(number, digits = digits)
        }

        # keep NaN/NA as-is
        rounded[is.nan(number)] <- NaN
        rounded[is.na(number)] <- NA
        return(rounded)
    }

    # case 1: data frame apply columnwise
    if (is.data.frame(x)) {
        for (nm in names(x)) {
            if (!is.null(exclude) && nm %in% exclude) next
            x[[nm]] <- .round_num(x[[nm]], digits, rounding_method)
        }
        return(as.data.frame(x))
    }

    # case 2: atomic vector
    return(.round_num(x, digits, rounding_method))
}


##################################################################################
#' .getDelim
#'
#' This helper function attempts to detect the delimiter used in a text file by reading
#' a small sample of rows and determining which of a specified set of delimiters yields the
#' highest number of columns. This function is useful for automatically identifying the correct
#' delimiter when reading data files with various formats.
#'
#' @param filename A string specifying the path to the file whose delimiter is to be detected.
#' @param seps A character vector of possible delimiters to test. The default is `c("\t", " ", ";", ",")`.
#' @param nrows An integer specifying the number of rows to read for testing the delimiter. Default is 50.
#'
#' @details
#' The function works by iteratively attempting to read a portion of the file (`nrows` rows) with
#' each delimiter specified in `seps`. It counts the number of columns produced by each delimiter
#' and selects the delimiter that yields the highest number of columns. This method assumes that
#' the correct delimiter will create more columns than incorrect ones.
#'
#' - **Max Columns Logic**: The function assumes that the delimiter yielding the maximum number
#'   of columns is the correct one. If the resulting column count is only 1 (indicating that the
#'   file could not be split into columns), the function outputs an error message and returns `NULL`.
#'
#' @return Returns the detected delimiter as a character if successful. If the file cannot be
#'         split into columns with any of the tested delimiters, returns `NULL` invisibly.
#'
#' @examples
#' # Example usage:
#' # sep <- .getDelim("datafile.txt")
#'
.getDelim <- function(filename, seps = c(",", "\t", ";"), nrows = 50) {
    nbCol <- NULL
    for (i in 1:length(seps)) {
        sep <- seps[i]
        df <- utils::read.csv(filename, sep = sep, nrows = nrows)
        nbCol <- c(nbCol, length(df))
    }
    iSep <- which(nbCol == max(nbCol))[1]
    sep <- seps[iSep]
    if (nbCol[iSep] == 1) {
        message(paste0("ERROR: Cannot read file ", filename, "."))
        return(invisible(NULL))
    }
    return(invisible(sep))
}


##################################################################################
#' summarizeStats
#'
#' Calculates comprehensive descriptive statistics for a numeric variable,
#' including arithmetic, geometric, and harmonic means, standard deviation,
#' coefficient of variation, quantiles, and percentiles. Supports grouped
#' summaries by one or more categorical variables.
#'
#' @param df A data frame containing the data to summarize.
#' @param group_vars Character vector of column names to group by. If `NULL`,
#'   statistics are calculated for the entire dataset without grouping.
#' @param value_col Character string specifying the name of the numeric column
#'   to summarize. Default is `"value"`.
#' @param id_col Character string specifying the name of the ID column for
#'   counting distinct subjects/individuals. If `NULL`, row count is used instead.
#'   Default is `NULL`.
#'
#' @details
#' This function computes a comprehensive set of summary statistics:
#' \itemize{
#'   \item **N**: Number of distinct IDs (if `id_col` provided) or row count
#'   \item **Arithmetic statistics**: mean, SD, CV (coefficient of variation as percentage)
#'   \item **Quantiles**: min, P05 (5th percentile), Q1, median, Q3, P95 (95th percentile), max
#'   \item **Geometric statistics**: geoMean, geoSD, geoCV (calculated on log scale)
#'   \item **Harmonic mean**: harmMean
#' }
#'
#' **Geometric Statistics**: Only positive, finite values are used. The geometric mean
#' is calculated as `exp(mean(log(x)))`, geometric SD as `exp(sd(log(x)))`, and
#' geometric CV as `100 * sqrt(exp(sd(log(x))^2) - 1)`.
#'
#' **Harmonic Mean**: Only positive, finite values are used. Calculated as
#' `n / sum(1/x)` where n is the count of valid values.
#'
#' @return A data frame with one row per group (or one row total if ungrouped)
#'   containing all calculated statistics as columns.
#'
#' @keywords internal
#'
#' @examples
#' # df_long <- data.frame(parameter = rep(c("AUC", "Cmax"), each = 10),
#' #                       value = c(rnorm(10, 100, 20), rnorm(10, 50, 10)),
#' #                       id = rep(1:10, 2))
#' # .summarizeStats(df_long, group_vars = "parameter", value_col = "value", id_col = "id")
#'
.summarizeStats <- function(df,
                            group_vars = NULL,
                            value_col = "value",
                            id_col = NULL) {
    # Ensure the value column exists
    if (!(value_col %in% names(df))) {
        stop("Value column '", value_col, "' not found in input data.")
    }

    df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::summarise(
            N = if (!is.null(id_col)) dplyr::n_distinct(.data[[id_col]]) else dplyr::n(),
            min = min(.data[[value_col]], na.rm = TRUE),
            P05 = stats::quantile(.data[[value_col]], 0.05, na.rm = TRUE),
            Q1 = stats::quantile(.data[[value_col]], 0.25, na.rm = TRUE),
            mean = mean(.data[[value_col]], na.rm = TRUE),
            SD = stats::sd(.data[[value_col]], na.rm = TRUE),
            CV = if (is.na(mean) || mean == 0) NA_real_ else 100 * SD / mean,
            median = stats::median(.data[[value_col]], na.rm = TRUE),
            Q3 = stats::quantile(.data[[value_col]], 0.75, na.rm = TRUE),
            P95 = stats::quantile(.data[[value_col]], 0.95, na.rm = TRUE),
            max = max(.data[[value_col]], na.rm = TRUE),

            # geometric mean / sd / cv
            geoMean = {
                mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
                if (sum(mask) > 0) exp(mean(log(.data[[value_col]][mask]), na.rm = TRUE)) else NA_real_
            },
            geoSD = {
                mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
                sdlog <- if (sum(mask) > 1) stats::sd(log(.data[[value_col]][mask]), na.rm = TRUE) else NA_real_
                if (is.na(sdlog)) NA_real_ else exp(sdlog)
            },
            geoCV = {
                mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
                sdlog <- if (sum(mask) > 1) stats::sd(log(.data[[value_col]][mask]), na.rm = TRUE) else NA_real_
                if (is.na(sdlog)) NA_real_ else 100 * sqrt(exp(sdlog^2) - 1)
            },

            # harmonic mean
            harmMean = {
                mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
                if (any(mask, na.rm = TRUE)) length(.data[[value_col]][mask]) / sum(1 / .data[[value_col]][mask]) else NA_real_
            },
            .groups = "drop"
        )
}

##################################################################################
#' combinedStats
#'
#' Creates combined display metrics by concatenating mean with SD and geometric
#' mean with geometric CV or SD. This formatting is commonly used in pharmacokinetic
#' summary tables to present central tendency and variability together.
#'
#' @param df A data frame containing columns named `mean`, `SD`, `geoMean`,
#'   `geoCV`, and `geoSD`. Typically the output from `.summarizeStats()`.
#'
#' @details
#' The function creates three new combined columns:
#' \itemize{
#'   \item **MeanSD**: Formatted as "mean (SD)"
#'   \item **GeomCV**: Formatted as "geoMean (geoCV)"
#'   \item **GeomSD**: Formatted as "geoMean (geoSD)"
#' }
#'
#' These combined metrics are useful for creating compact summary tables where
#' both central tendency and variability need to be displayed together.
#'
#' @return The input data frame with three additional columns: `MeanSD`, `GeomCV`,
#'   and `GeomSD`.
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(mean = 100, SD = 20, geoMean = 95, geoCV = 18, geoSD = 1.2)
#' # .combinedStats(df)
#' # Result: adds columns MeanSD = "100 (20)", GeomCV = "95 (18)", GeomSD = "95 (1.2)"
#'
.combinedStats <- function(df) {
    df %>%
        dplyr::mutate(
            MeanSD = paste0(.data$mean, " (", .data$SD, ")"),
            GeomCV = paste0(.data$geoMean, " (", .data$geoCV, ")"),
            GeomSD = paste0(.data$geoMean, " (", .data$geoSD, ")")
        )
}

##################################################################################
#' fixNCAConcentrationsColumnSwap
#'
#' Corrects the column order issue in \code{getNCAConcentrations()} output where
#' covariate columns appear with swapped/reversed content. The function detects
#' when covariate column headers are in the correct order but their data content
#' is swapped, and restores the proper alignment between column
#' names and their corresponding data.
#'
#' @param df Data frame returned by \code{getNCAConcentrations(displayIndividualProfiles = TRUE)}
#'   that exhibits the column swapping issue.
#' @param original_covariates Character vector of covariate names in their intended
#'   order, as originally passed to \code{setNCAResultsStratification(split = covariates)}.
#'   Must match the order used when setting up the stratification.
#'
#' @details This function corrects a known bug where \code{getNCAConcentrations()}
#'   with \code{displayIndividualProfiles = TRUE} returns covariate columns with
#'   correct headers but swapped content. When the number of covariates is even,
#'   all covariates are affected in reverse order. When odd, the middle covariate
#'   remains correct while others are mirrored around it. The function identifies
#'   covariate columns by name, reverses their order, and applies the original
#'   intended column names to restore proper data alignment.
#'
#' @return A data frame with the same structure as the input but with covariate
#'   columns correctly ordered and named according to \code{original_covariates}.
#'   Non-covariate columns (subject, time points, concentrations) remain unchanged.
#'
#' @examples
#' # covariates <- c("DOSE", "FOOD", "VISIT")
#' # setNCAResultsStratification(split = covariates)
#' # df <- getNCAConcentrations(displayIndividualProfiles = TRUE)
#' # df_fixed <- fixNCAConcentrationsColumnSwap(df, covariates)
.fixNCAConcentrationsColumnSwap <- function(df, original_covariates) {
    # Get the number of covariates
    n_cov <- length(original_covariates)

    # If no covariates or only one, no swapping occurs
    if (n_cov <= 1) {
        return(df)
    }

    # Find covariate columns in the dataframe
    cov_cols <- names(df)[names(df) %in% original_covariates]

    # Find non-covariate columns
    non_cov_cols <- names(df)[!names(df) %in% original_covariates]

    # Create the corrected column order
    # The covariates appear to be reversed in the output
    corrected_cov_order <- rev(cov_cols)

    # Reorder the dataframe with corrected covariate order
    df_fixed <- df[, c(corrected_cov_order, non_cov_cols)]

    # Rename columns to match the original intended order
    names(df_fixed)[1:n_cov] <- original_covariates

    return(df_fixed)
}

##################################################################################

#' Internal helper to fetch and prepare Covariate summary stats
#'
#' @description Fetches covariate data, reshapes to long format,
#' summarizes using .summarizeStats, rounds, and adds combined metrics.
#'
.prepareCovariateSummaryStats <- function(projectFile, covariates, cont_cols, roundDigits, roundMethod) {
    # Base data: variables from dataset tagged as covariates
    # Note: We fetch ID to ensure we can count subjects correctly
    cov_df <- lixoftConnectors::getCovariateInformation()$covariate

    # Ensure we select valid columns (intersection of what is in cov_df and what is expected)
    # This handles edge cases where header info might slightly differ from loaded covariate info
    valid_cont_cols <- intersect(names(cov_df), cont_cols)

    if (length(valid_cont_cols) == 0) {
        return(NULL)
    }

    keep_cols <- c("id", valid_cont_cols, covariates)

    # --- Reshape to Long Format ---
    # variable = covariate name, value = measurement
    df_long <- cov_df %>%
        dplyr::select(dplyr::all_of(keep_cols)) %>%
        tidyr::pivot_longer(
            cols = dplyr::all_of(valid_cont_cols),
            names_to = "variable",
            values_to = "value"
        ) %>%
        dplyr::mutate(value = suppressWarnings(as.numeric(value)))

    # Summarize & determine grouping
    if (is.null(covariates)) {
        group_vars <- "variable"
    } else {
        group_vars <- c(covariates, "variable")
    }

    # Calculate stats
    summary_stats <- .summarizeStats(
        df         = df_long,
        group_vars = group_vars,
        value_col  = "value",
        id_col     = "id"
    )

    # Standardize Count column to "Nobs"
    if ("N" %in% names(summary_stats)) {
        summary_stats <- dplyr::rename(summary_stats, Nobs = N)
    } else if ("N Total" %in% names(summary_stats)) {
        summary_stats <- dplyr::rename(summary_stats, Nobs = `N Total`)
    }

    # Rounding & combining metrics
    summary_stats <- .roundNumericCols(
        summary_stats,
        digits          = roundDigits,
        rounding_method = roundMethod,
        exclude         = c("variable", covariates, "Nobs")
    )

    summary_stats <- .combinedStats(summary_stats)

    return(summary_stats)
}


##################################################################################
#' Internal helper to validate and select metrics
#'
#' @param available_cols Character vector of all column names in the stats dataframe.
#' @param user_metrics Character vector of metrics requested by the user (can be NULL).
#' @param descriptors Character vector of column names that are descriptors (not metrics).
#'
#' @return A character vector of valid selected metrics.
#'
.validateAndSelectMetrics <- function(available_cols, user_metrics, descriptors) {
    # Determine what are actual metrics by excluding descriptors
    available_metrics_list <- setdiff(available_cols, descriptors)

    if (is.null(user_metrics)) {
        # User did not specify metrics -> Return ALL available
        message("No metrics specified. Using all available metrics.")
        return(available_metrics_list)
    } else {
        # User specified metrics
        missing_metrics <- setdiff(user_metrics, available_metrics_list)
        valid_metrics <- intersect(user_metrics, available_metrics_list)

        # Error if NO valid metrics found
        if (length(valid_metrics) == 0) {
            stop(paste0(
                "None of the requested metrics were found.\n",
                "Requested: ", paste(user_metrics, collapse = ", "), "\n",
                "Available metrics: ", paste(available_metrics_list, collapse = ", ")
            ))
        }

        # Warning if SOME metrics missing
        if (length(missing_metrics) > 0) {
            warning(paste0(
                "Ignoring unknown metrics: ", paste(missing_metrics, collapse = ", "),
                ".\nAvailable metrics: ", paste(available_metrics_list, collapse = ", ")
            ))
        }

        return(valid_metrics)
    }
}
##################################################################################
#' Internal helper to validate stratification arguments
#'
#' @param covariates Vector of covariate names (or NULL).
#' @param stratify_columns Boolean flag indicating if stratification is requested.
#'
#' @return Boolean. Returns FALSE if covariates are missing, otherwise returns original stratify_columns.
#'
.validateStratificationArgs <- function(covariates, stratify_columns) {
    if (isTRUE(stratify_columns) && is.null(covariates)) {
        warning("stratify_columns = TRUE but no covariates were provided. The table will be generated without stratification.")
        return(FALSE)
    }
    return(stratify_columns)
}


##################################################################################

#' Internal helper to validate covariate labels against covariates
#'
#' @param covariates Character vector of covariate names.
#' @param covariate_labels Character vector of labels (can be NULL).
#'
#' @return A character vector of validated labels matching the length of covariates.
#'
.validateCovariateLabels <- function(covariates, covariate_labels) {
    if (is.null(covariates)) {
        return(NULL)
    }

    # If no labels provided, use variable names
    if (is.null(covariate_labels)) {
        return(covariates)
    }

    n_cov <- length(covariates)
    n_lab <- length(covariate_labels)

    if (n_lab < n_cov) {
        # Scenario: Fewer labels than covariates -> Fill the rest with names
        # e.g. covs=c(A,B,C), labels=c("LabelA") -> result=c("LabelA", "B", "C")
        final_labels <- c(covariate_labels, covariates[(n_lab + 1):n_cov])
    } else if (n_lab > n_cov) {
        # Scenario: More labels than covariates -> Truncate and Warn
        final_labels <- covariate_labels[1:n_cov]
        warning(paste0("More labels provided (", n_lab, ") than covariates (", n_cov, "). Extra labels ignored."))
    } else {
        # Scenario: Exact match
        final_labels <- covariate_labels
    }

    return(final_labels)
}


##################################################################################
#' Internal helper to fetch and prepare NCA & Conecntration summary stats
#'
#' @description
#' This internal function acts as the backend data pipeline for NCA summary tables and listings.
#' It retrieves data, calculates statistics, applies rounding, handles stratification,
#' and formats labels based on settings inherited from the calling function.
#'
#' It operates in two distinct modes:
#' \itemize{
#'   \item **Parameter Mode** (`by_time = FALSE`): Retrieves and summarizes NCA parameters (e.g., Cmax, AUC).
#'   \item **Concentration Mode** (`by_time = TRUE`): Retrieves and summarizes concentration data grouped by timepoint.
#' }
#'
#' @param project A character string representing the path to the PKanalix project file (.pkx).
#' @param NCAparams A character vector of NCA parameter names to filter by.
#'   If `NULL`, all available parameters are retrieved.
#'   \emph{Applicable only when `by_time = FALSE`.}
#' @param covariates A character vector of covariate names to stratify the results by.
#'   If `NULL`, overall (non-stratified) statistics are calculated.
#' @param roundDigits Integer. The rounding precision (number of digits).
#'   Inherited from the parent table/listing function settings.
#' @param roundMethod Character string. The rounding method (e.g., "monolix" or "signif").
#'   Inherited from the parent table/listing function settings.
#' @param units Logical. If `TRUE`, units are appended to parameter names or values.
#'   \emph{Applicable only when `by_time = FALSE` (NCA Parameter summaries).}
#' @param aliases Logical. If `TRUE`, parameter aliases (friendly names) are used.
#'   \emph{Applicable only when `by_time = FALSE` (NCA Parameter summaries).}
#' @param by_time Logical. Toggles the function mode:
#'   \itemize{
#'     \item `FALSE` (default): Prepares NCA Parameter statistics.
#'     \item `TRUE`: Prepares Concentration summary statistics by timepoint.
#'   }
#' @param useNominalTime Logical. If `TRUE`, attempts to use Nominal Time for grouping concentrations.
#'   If unavailable, falls back to Actual Time with a warning.
#'   \emph{Applicable only when `by_time = TRUE` (Concentration summaries).}
#'
#' @return A data frame containing the summarized statistics (Mean, SD, Median, etc.)
#'   and combined metrics (MeanSD, GeomCV), formatted and rounded according to the inputs.
#'   Returns `NULL` if the required NCA results are not available.
#'
#' @keywords internal
#'
.prepareNCASummaryStats <- function(projectFile,
                                    NCAparams = NULL,
                                    covariates = NULL,
                                    roundDigits = 3,
                                    roundMethod = "monolix",
                                    units = TRUE,
                                    aliases = TRUE,
                                    by_time = FALSE,
                                    useNominalTime = TRUE) {
    if (by_time) {
        # CONCENTRATION MODE: Calculate stats by timepoint
        # Set Stratification
        lixoftConnectors::setNCAResultsStratification() # reset
        if (!is.null(covariates)) {
            lixoftConnectors::setNCAResultsStratification(split = covariates)
        }

        # Check if nominal time exists
        has_nomtime <- FALSE
        if (useNominalTime) {
            data <- lixoftConnectors::getData()
            has_nomtime <- any(data$headerTypes == "nominaltime")
            if (!has_nomtime) {
                warning("No nominal time available in project; Actual time will be used instead.")
            }
        }

        # Get concentration data
        df <- lixoftConnectors::getNCAConcentrations(useNominalTime = has_nomtime)

        if (is.null(df)) {
            return(NULL)
        }

        # Fix column swap bug
        df_fixed <- .fixNCAConcentrationsColumnSwap(df, covariates)

        # Choose label column
        stat_col <- if ("metric" %in% names(df_fixed)) "metric" else "subject"

        # Keep only summary stats (filter out individual subjects)
        summary_only <- df_fixed %>%
            dplyr::filter(!grepl("^\\d+$", !!rlang::sym(stat_col)))

        # Pivot time columns to rows
        cols_to_pivot <- setdiff(names(df_fixed), c(covariates, stat_col))

        summary_long <- summary_only %>%
            tidyr::pivot_longer(
                cols      = dplyr::all_of(cols_to_pivot),
                names_to  = "Time",
                values_to = "Value"
            )

        # Convert to numeric for proper rounding
        summary_long$Value <- suppressWarnings(as.numeric(summary_long$Value))

        # Pivot metrics to columns
        summary_stats <- summary_long %>%
            tidyr::pivot_wider(names_from = !!rlang::sym(stat_col), values_from = "Value") %>%
            dplyr::arrange(dplyr::across(dplyr::any_of(covariates)), as.numeric(Time)) %>%
            dplyr::mutate(dplyr::across(dplyr::any_of(covariates), as.character))

        # Apply Rounding
        summary_stats <- .roundNumericCols(
            summary_stats,
            digits = roundDigits,
            rounding_method = roundMethod,
            exclude = c("Time", covariates)
        )

        # Add Combined Metrics
        summary_stats <- .combinedStats(summary_stats)

        # Note: units and aliases are not applicable for concentration data

        return(summary_stats)
    } else {
        # PARAMETER MODE: Original logic (calculate stats by parameter)
        # Set Stratification
        lixoftConnectors::setNCAResultsStratification() # reset
        if (!is.null(covariates)) {
            lixoftConnectors::setNCAResultsStratification(split = covariates)
        }

        # Get Summary Stats (from getNCAParameterStatistics)
        summary_stats <- lixoftConnectors::getNCAParameterStatistics(parameters = NCAparams)$statistics
        if (is.null(summary_stats)) {
            # Return NULL if no stats, parent function will handle
            return(NULL)
        }
        summary_stats$parameter <- as.character(summary_stats$parameter)

        # Get Individual Parameters & Stats
        indiv <- lixoftConnectors::getNCAIndividualParameters()$parameters

        # Identify grouping and pivoting structure
        grouping_vars <- c("id", covariates)

        # Keep only numeric columns + covariates + id
        indiv_long <- indiv %>%
            dplyr::select(dplyr::all_of(grouping_vars), dplyr::where(is.numeric)) %>%
            tidyr::pivot_longer(
                cols = -dplyr::all_of(grouping_vars),
                names_to = "parameter",
                values_to = "value"
            ) %>%
            dplyr::relocate(dplyr::any_of(covariates), id, parameter, value)

        # Summarize (handles stats automatically)
        if (is.null(covariates)) {
            descr_stats <- .summarizeStats(
                df         = indiv_long,
                group_vars = "parameter",
                value_col  = "value",
                id_col     = "id"
            ) %>%
                dplyr::select("parameter", P05, Q1, median, Q3, P95, max) %>%
                dplyr::arrange(parameter)
        } else {
            descr_stats <- .summarizeStats(
                df         = indiv_long,
                group_vars = c(covariates, "parameter"),
                value_col  = "value",
                id_col     = "id"
            ) %>%
                dplyr::select(dplyr::all_of(c(covariates, "parameter")), P05, Q1, median, Q3, P95, max) %>%
                dplyr::arrange(dplyr::across(dplyr::all_of(c(covariates, "parameter"))))
        }

        # Merge Summaries
        join_cols <- c("parameter", covariates)
        summary_stats <- dplyr::left_join(summary_stats, descr_stats, by = join_cols)

        # Apply Rounding & Combining
        summary_stats <- .roundNumericCols(
            summary_stats,
            digits          = roundDigits,
            rounding_method = roundMethod,
            exclude         = c("parameter", covariates)
        )

        summary_stats <- .combinedStats(summary_stats)

        # Apply Labels (Units/Aliases only)
        if (isTRUE(units) || isTRUE(aliases)) {
            summary_stats <- .addUnitsAliases(summary_stats, add_units = units, add_aliases = aliases)
        }

        return(summary_stats)
    }
}


##################################################################################
#' calculate_raw_freq
#'
#' Calculates raw frequency counts and percentages for categorical variables.
#' For each unique value (modality) within each variable, computes the count,
#' percentage, and total number of distinct subjects.
#'
#' @param df_input A data frame in long format with columns:
#'   \itemize{
#'     \item `id`: Subject/individual identifier
#'     \item `variable`: Name of the categorical variable being summarized
#'     \item `value`: The categorical value/modality for that variable
#'   }
#'
#' @details
#' This function is used internally for creating frequency tables of categorical
#' covariates. It first counts the total number of distinct subjects (IDs) for
#' each variable, then calculates how many subjects have each specific value
#' (modality) of that variable.
#'
#' The percentage is calculated as `(Count / N) * 100` where:
#' \itemize{
#'   \item **Count**: Number of subjects with this specific modality
#'   \item **N**: Total number of distinct subjects for this variable
#' }
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item `variable`: Variable name
#'     \item `Modalities`: The specific value/category
#'     \item `Count`: Number of subjects with this modality
#'     \item `Percentage`: Percentage of subjects with this modality
#'     \item `N`: Total number of distinct subjects for this variable
#'   }
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(id = c(1,2,3,1,2,3),
#' #                  variable = rep(c("SEX", "RACE"), each = 3),
#' #                  value = c("M", "F", "M", "White", "White", "Asian"))
#' # .calculate_raw_freq(df)
#'
.calculate_raw_freq <- function(df_input) {
    # This logic assumes df_input has columns: id, variable, value
    subject_counts <- df_input %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(N = dplyr::n_distinct(id), .groups = "drop")

    df_input %>%
        dplyr::group_by(variable, value) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        dplyr::left_join(subject_counts, by = "variable") %>%
        dplyr::mutate(Percentage = (Count / N) * 100) %>%
        dplyr::select(variable, Modalities = value, Count, Percentage, N)
}


##################################################################################
#' format_frequency_table
#'
#' Formats a frequency table by applying rounding and creating a combined
#' display metric showing count and percentage together as "N (%)".
#'
#' @param summary_tbl A data frame containing frequency statistics, typically
#'   the output from `.calculate_raw_freq()`. Must contain columns `Count` and
#'   `Percentage`.
#' @param digits Integer specifying the number of decimal places for rounding
#'   the percentage values.
#' @param method Character string specifying the rounding method. Either
#'   `"monolix"` (adaptive precision) or `"signif"` (significant figures).
#' @param exclude_cols Character vector of column names to exclude from rounding.
#'
#' @details
#' This function performs two main operations:
#' \enumerate{
#'   \item Applies rounding to numeric columns using `.roundNumericCols()`
#'   \item Creates a combined `N (%)` column formatted as "Count (Percentage)"
#' }
#'
#' The percentage is formatted using `sprintf()` to ensure consistent decimal places.
#' After creating the combined column, the original `Count` and `Percentage` columns
#' are removed.
#'
#' @return A data frame with the same structure as the input, but with:
#'   \itemize{
#'     \item Numeric values rounded according to the specified method
#'     \item A new `N (%)` column replacing the separate `Count` and `Percentage` columns
#'   }
#'
#' @keywords internal
#'
#' @examples
#' # freq_tbl <- data.frame(variable = "SEX", Modalities = c("M", "F"),
#' #                        Count = c(15, 10), Percentage = c(60.0, 40.0), N = c(25, 25))
#' # .format_frequency_table(freq_tbl, digits = 1, method = "monolix", exclude_cols = c("variable", "Modalities", "N"))
#'
.format_frequency_table <- function(summary_tbl, digits, method, exclude_cols) {
    # 1. Apply rounding (respects roundMethod)
    summary_tbl <- .roundNumericCols(
        summary_tbl,
        digits          = digits,
        rounding_method = method,
        exclude         = exclude_cols
    )

    # 2. Create the final combined display metric N (%)
    summary_tbl <- summary_tbl %>%
        dplyr::mutate(
            `N (%)` = paste0(Count, " (", sprintf(paste0("%.", digits, "f"), Percentage), ")")
        ) %>%
        dplyr::select(-Count, -Percentage)

    return(summary_tbl)
}


##################################################################################
#' calc_format_stratum
#'
#' Convenience wrapper that calculates raw frequencies and formats them in one step.
#' Combines `.calculate_raw_freq()` and `.format_frequency_table()` into a single
#' function call.
#'
#' @param df_input A data frame in long format with columns `id`, `variable`, and `value`.
#'   See `.calculate_raw_freq()` for details.
#' @param digits Integer specifying the number of decimal places for rounding percentages.
#' @param method Character string specifying the rounding method ("monolix" or "signif").
#' @param exclude_cols Character vector of column names to exclude from rounding.
#'
#' @details
#' This is a pipeline function that:
#' \enumerate{
#'   \item Calculates raw frequency statistics using `.calculate_raw_freq()`
#'   \item Formats the results using `.format_frequency_table()`
#' }
#'
#' It's used internally to streamline the creation of formatted frequency tables
#' for categorical covariates in summary reports.
#'
#' @return A formatted frequency table with combined `N (%)` display metric.
#'   See `.format_frequency_table()` for details on the output structure.
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(id = c(1,2,3,1,2,3),
#' #                  variable = rep(c("SEX", "RACE"), each = 3),
#' #                  value = c("M", "F", "M", "White", "White", "Asian"))
#' # .calc_format_stratum(df, digits = 1, method = "monolix", exclude_cols = c("variable", "Modalities", "N"))
#'
.calc_format_stratum <- function(df_input, digits, method, exclude_cols) {
    raw_stats <- .calculate_raw_freq(df_input)
    final_stats <- .format_frequency_table(raw_stats, digits, method, exclude_cols)
    return(final_stats)
}


##################################################################################
#' mockOutputData
#'
#' Masks sensitive data in output tables by replacing numeric values and counts
#' with placeholder patterns (e.g., "xx.xxx"). Used for creating example outputs
#' or documentation without revealing actual study data.
#'
#' @param df A data frame containing the table to be mocked.
#' @param mock_settings A list with the following elements:
#'   \itemize{
#'     \item `enable`: Logical. If `FALSE`, the function returns the input unchanged.
#'     \item `exclude_cols`: Character vector of column names to exclude from mocking
#'       (e.g., parameter names, variable labels).
#'     \item `max_rows`: Numeric or `NULL`. Maximum number of rows to include in output.
#'       If `NULL` or `"all"`, all rows are included.
#'   }
#' @param digits Integer specifying the number of decimal places, used to determine
#'   the pattern for mocked values (e.g., digits = 3 produces "xx.xxx").
#' @param method Character string specifying the rounding method. Passed to
#'   `.roundNumericCols()` but primarily used for consistency with other functions.
#'
#' @details
#' This function performs three main operations:
#'
#' **1. Mock Data Cells**: Replaces numeric values with placeholder patterns:
#' \itemize{
#'   \item Simple numbers: "xx.xxx" (based on `digits` parameter)
#'   \item Combined metrics: "xx.xxx (xx.xxx)" for values containing parentheses
#'   \item Preserves NA values as-is
#' }
#'
#' **2. Mock Column Headers**: Masks stratification information in column names:
#' \itemize{
#'   \item Modality values: "DOSE=100" becomes "DOSE=xx"
#'   \item Subject counts: "(N=25)" becomes "(N=xx)"
#' }
#'
#' **3. Limit Rows**: Optionally truncates the output to a maximum number of rows
#' as specified in `mock_settings$max_rows`.
#'
#' The function intelligently detects whether a column contains numeric data or
#' combined metrics (with parentheses) and applies the appropriate masking pattern.
#'
#' @return A data frame with the same structure as the input but with:
#'   \itemize{
#'     \item Numeric values replaced with "xx" patterns
#'     \item Column headers masked if they contain stratification info
#'     \item Rows limited to `max_rows` if specified
#'   }
#'   Returns the original data frame unchanged if `mock_settings$enable` is `FALSE`.
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(Parameter = c("AUC", "Cmax"),
#' #                  `Mean (SD)` = c("100.5 (20.3)", "50.2 (10.1)"),
#' #                  check.names = FALSE)
#' # mock_settings <- list(enable = TRUE, exclude_cols = "Parameter", max_rows = 10)
#' # .mockOutputData(df, mock_settings, digits = 1, method = "monolix")
#' # Result: Parameter column unchanged, Mean (SD) becomes "xx.x (xx.x)"
#'
.mockOutputData <- function(df, mock_settings, digits, method) {
    if (!isTRUE(mock_settings$enable)) {
        return(df)
    }

    df_mocked <- df
    exclude_cols <- mock_settings$exclude_cols %||% character(0)

    # Identify which columns need mocking
    cols_to_mock <- setdiff(names(df), exclude_cols)

    # --------------------------------------------------------------------------
    # PREPARE MASKS
    x_integer_part <- "xx"
    if (digits > 0) {
        x_decimal_part <- paste(rep("x", digits), collapse = "")
        x_base_number <- paste0(x_integer_part, ".", x_decimal_part)
    } else {
        x_base_number <- x_integer_part
    }
    x_combined_format <- paste0(x_base_number, " (", x_base_number, ")")

    n_rows <- nrow(df)

    # --------------------------------------------------------------------------
    # MOCK DATA CELLS

    for (col in cols_to_mock) {
        original_data <- df[[col]]

        # Check 1: Is it a combined string? e.g. "10 (2)"
        contains_combined <- any(grepl("\\(", original_data))

        # Check 2: Is it numeric OR does it look like a number?
        as_num <- suppressWarnings(as.numeric(original_data))
        looks_numeric <- is.numeric(original_data) || (!all(is.na(as_num) & !is.na(original_data)))

        if (contains_combined || looks_numeric) {
            # Create a baseline vector filled with the simple format (xx.xxx)
            new_vals <- rep(x_base_number, n_rows)

            # Identify specific rows that have the combined format (parentheses)
            # and overwrite those specific slots with the combined mask
            has_paren <- grepl("\\(", original_data)
            new_vals[has_paren] <- x_combined_format

            # Preserve NAs
            new_vals[is.na(original_data)] <- NA_character_

            # Assign back to df_mocked
            df_mocked[[col]] <- new_vals
        }
    }

    # --------------------------------------------------------------------------
    # MOCK HEADERS
    header_pattern <- "\\n\\(N=(\\d+)\\)$"
    new_names <- names(df_mocked)

    for (i in seq_along(new_names)) {
        col_name <- new_names[i]

        if (col_name %in% exclude_cols) next

        if (grepl(header_pattern, col_name)) {
            # Mask Modality
            if (grepl("=.*\\n", col_name)) {
                col_name <- sub("=(.*?)\\n", "=xx\n", col_name)
            }
            # Mask N Count
            col_name <- sub("N=(\\d+)", "N=xx", col_name)

            new_names[i] <- col_name
        }
    }
    names(df_mocked) <- new_names

    # --------------------------------------------------------------------------
    # LIMIT ROWS
    max_rows_setting <- mock_settings$max_rows

    # Logic: If NULL (not provided) or "all", show everything. Only limit if numeric.
    if (is.numeric(max_rows_setting)) {
        n_limit <- min(nrow(df), max_rows_setting)
    } else {
        # Default behavior (NULL, "all", or invalid input) -> Show ALL rows
        n_limit <- nrow(df)
    }

    df_mocked <- df_mocked[1:n_limit, , drop = FALSE]

    return(df_mocked)
}
