#' Creates a data frame summarizing the continuous covariates in the dataset with descriptive statistics.
#'
#' @param projectFile path to project file (Monolix: .mlxtran or PKanalix: .pkx) or NULL to use the currently loaded project
#' @param covDisplayLabels (optional) data.frame with columns covariate and label for the dataset covariate name
#' @param splitCovariates vector of categorical covariates to stratify by, or NULL
#' @param metrics vector of metrics to display, or NULL to display all available metrics
#' @param metricsAsRows TRUE to display metrics as rows, or FALSE (default) to display them as columns
#' @param digits number of digits to display
#' @param roundingMethod "monolix" to use MonolixSuite rounding where digits specifies the number of places after the decimal, or "signif"
#' for digits to specify the number of significant figures
#'
#' @returns a data.frame
#' @seealso [getContinuousCovariateMetrics()]
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # TODO
tableContinuousCovariateSummary <- function(projectFile      = NULL,
                                            covDisplayLabels = NULL,
																						splitCovariates  = NULL,
																						metrics          = NULL,
																						metricsAsRows    = FALSE,
                                            roundDigits      = 2,
                                            roundingMethod   = c("monolix", "signif", "none"),
																						prettyLabels     = TRUE)
{


	projectFile      = prj9
	covDisplayLabels = NULL
	splitCovariates  = "APGAR"
	metrics          = NULL
	metricsAsRows    = FALSE
	roundDigits      = 2
	roundMethod   = "monolix"
	prettyLabels     = TRUE

    .checkProjectFile(projectFile, c("monolix", "pkanalix"))
    .ensureProjectLoaded(projectFile)

    if (!is.null(covDisplayLabels) && !is.data.frame(covDisplayLabels)) {
        stop("The argument 'covDisplayLabels' must be a dataframe.", call. = FALSE)
    }
    if (!is.null(covDisplayLabels) && !identical(names(covDisplayLabels), c("covariate", "label"))) {
    	stop(paste0("The argument 'covDisplayLabels' must contain the headers 'covariate' and 'label' Found headers are: ",
    							paste(names(covDisplayLabels), collapse = ", ")
    	), call. = FALSE )
    }
    if (metricsAsRows && !is.null(splitCovariates)) {
    	stop("Splitting by covariates not supported for displaying metrics as rows.", call. = FALSE)
    }

    cov_info <- lixoftConnectors::getCovariateInformation()

    if (!is.null(splitCovariates)) {
    	if (!all(splitCovariates %in% cov_info$name[cov_info$type == "categorical"])) {
    		stop("Covariates to use for splitting must be categorical covariates (headerTypes == 'catcov').")
    	}
    }

    # Prepare data: pivot continuous covariates to long format
    cont_covs <- cov_info$name[cov_info$type == "continuous"]
    has_id    <- "id" %in% names(cov_info$covariate)

    cols_to_select <- c(cont_covs, splitCovariates)
    if (has_id) cols_to_select <- c("id", cols_to_select)

    cov_data <- cov_info$covariate %>%
        dplyr::select(dplyr::all_of(cols_to_select)) %>%
        tidyr::pivot_longer(
            cols = dplyr::all_of(cont_covs),
            names_to = "Covariate",
            values_to = "Value")

    # Calculate descriptive statistics using internal helper
    group_by_vars <- c("Covariate", splitCovariates)

    cov_summary <- .calculateDescriptiveStats(
        data      = cov_data,
        value_col = "Value",
        id_col    = if (has_id) "id" else NULL,
        group_by  = group_by_vars)

    # Apply rounding
    cov_summary <- .round_num(
        to_round        = cov_summary,
        digits          = roundDigits,
        rounding_method = roundingMethod)

    # Add combined metrics
    cov_summary <- .combinedStats(cov_summary)

    # Select metrics
    descriptor_cols <- c("Covariate", splitCovariates)

    selected_metrics <- .validateAndSelectMetrics(
        available_cols = names(cov_summary),
        user_metrics   = metrics,
        descriptors    = descriptor_cols)

    cov_summary <- cov_summary[, c(descriptor_cols, selected_metrics), drop = FALSE]

    # rename covariates
    if (!is.null(covDisplayLabels)) {
    	labelIdx <- match(cov_summary$Covariate, covDisplayLabels$covariate)
    	cov_summary$Covariate[!is.na(labelIdx)] <- covDisplayLabels$label[stats::na.omit(labelIdx)]
    }

    if (prettyLabels) {
    	cov_summary <- .prettyLabels(cov_summary)
    }

    # transpose if requested
    if (metricsAsRows) {
    	col_names = cov_summary$Covariate
    	cov_summary <- cov_summary %>%
    		dplyr::select(-"Covariate") %>%
    		t() %>%
    		as.data.frame() %>%
    		setNames(col_names) %>%
    		tibble::rownames_to_column("Statistic")
    }
    attr(cov_summary, "project_source") <- lixoftConnectors::getProjectSettings()$directory
    return(cov_summary)
}

#' Available metrics for continuous covariate summary
#'
#' Metrics that can be passed to the 'metrics' argument of [tableContinuousCovariateSummary()]
#'
#' @returns vector of available metrics
#' @seealso [tableContinuousCovariateSummary()]
#' @export
#'
#' @examples
#' getContinuousCovariateMetrics()
getContinuousCovariateMetrics <- function() {
	return(c("N", "Nobs", "min", "P05", "Q1", "mean", "SD", "SE", "CV",
	         "median", "Q3", "P95", "max", "geoMean", "geoSD", "geoCV",
	         "harmMean", "MeanSD", "MeanCV", "GeomCV", "GeomSD"))
}

#' Creates a table summarizing the frequency of categorical covariates in the dataset.
#'
#' @param projectFile path to project file (Monolix: .mlxtran or PKanalix: .pkx) or NULL to use the currently loaded project
#' @param covDisplayLabels (optional) data.frame with columns covariate and label for the dataset covariate name
#' @param categoryDisplayLabels (optional) data.frame with columns covariate, category, and label for the dataset categorical covariate categories
#' @param digits number of digits to display
#' @param roundingMethod "monolix" to use MonolixSuite rounding where digits specifies the number of places after the decimal, or "signif"
#' for digits to specify the number of significant figures
#'
#' @returns a data.frame
#' @export
#'
#' @examples
#' # TODO
tableCategoricalCovariateSummary <- function(projectFile,
                                             covDisplayLabels = NULL,
                                             categoryDisplayLabels = NULL,
                                             digits = 2,
																						 roundingMethod = c("monolix", "signif", "none")) {
    .checkProjectFile(projectFile, c("monolix", "pkanalix"))
    .ensureProjectLoaded(projectFile)

    if (!is.null(covDisplayLabels) && !is.data.frame(covDisplayLabels)) {
        stop("The argument 'covDisplayLabels' must be a dataframe.", call. = FALSE)
    }

    if (!is.null(covDisplayLabels) &&
        !identical(names(covDisplayLabels), c("covariate", "label"))) {
        stop(
            paste0(
                "The argument 'covDisplayLabels' must contain the headers 'covariate' and 'label.' Headers found are: ",
                paste(names(covDisplayLabels), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    if (!is.null(categoryDisplayLabels) && !is.data.frame(categoryDisplayLabels)) {
        stop("The argument 'categoryDisplayLabels' must be a dataframe.", call. = FALSE)
    }

    if (!is.null(categoryDisplayLabels) &&
        !identical(
            names(categoryDisplayLabels),
            c("covariate", "category", "label")
        )) {
        stop(
            paste0(
                "The argument 'categoryDisplayLabels' must contain the headers 'covariate,' 'category,' and 'label.' Headers found are: ",
                paste(names(categoryDisplayLabels), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    cov_info <- lixoftConnectors::getCovariateInformation()

    cov_data <- cov_info$covariate %>%
        dplyr::select(cov_info$name[cov_info$type == "categorical"]) # verify then make summary table
    cov_summary <- NULL

    for (c in names(cov_data)) {
        cov <- cov_data %>%
            dplyr::summarise(.by = as.name(c), Frequency = dplyr::n()) %>%
            dplyr::mutate(
                Covariate = "",
                Category = !!as.name(c),
                Percentage = .round_num(.data$Frequency / sum(.data$Frequency), digits, rounding_method = roundingMethod)
            ) %>%
            dplyr::select(!as.name(c)) %>%
            dplyr::relocate("Frequency", .before = "Percentage")

        # rename categories
        if (!is.null(categoryDisplayLabels)) {
            cov <- cov %>%
                dplyr::mutate(Category = dplyr::filter(
                    categoryDisplayLabels,
                    .data$covariate == c
                )$label[match(.data$Category, dplyr::filter(
                    categoryDisplayLabels,
                    .data$covariate == c
                )$category)])
        }

        # rename covariates
        cov$Covariate[1] <- if (is.null(covDisplayLabels)) {
            c
        } else {
            covDisplayLabels$label[covDisplayLabels$covariate == c][1]
        }
        cov_summary <- rbind(cov_summary, cov)
    }
    attr(cov_summary, "project_source") <- lixoftConnectors::getProjectSettings()$directory
    return(cov_summary)
}

#-------------------------------------------------------------------------------
#' CovariateSummary_Table
#'
#' Generates a summary table of continuous covariates with descriptive statistics.
#' Provides overall summary and optional stratification by one or more categorical covariates.
#' Supports different output layouts:
#'   - **stratify_columns = TRUE**: Always produces one column per stratum + Overall column.
#'   - **stratify_columns = FALSE, horizontal = TRUE**: Wide layout with one row per covariate and columns for statistics.
#'   - **stratify_columns = FALSE, horizontal = FALSE**: Long layout with one row per statistic and columns for covariates.
#'
#' Statistics include: N, min, max, mean, SD, CV, median, Q1, Q3, 5th/95th percentiles,
#' as well as a combined Mean (SD) column.
#'
#' @param splitCovariates A character vector of categorical covariate(s) for stratification
#'   (must come from `headerTypes == 'catcov'`). If `NULL`, only overall statistics are reported.
#'   Multiple covariates will be combined into a single strata label.
#' @param splitCovariate_labels A character vector of labels for the `splitCovariates` covariates.
#'   Must be the same length as `splitCovariates`. If `NULL`, covariate names are used.
#' @param digits Integer. Number of decimal places to round statistics. Default is 2.
#' @param horizontal Logical. If `TRUE` (default), results are returned in wide layout
#'   (variables in rows, statistics in columns). Ignored when `stratify_columns = TRUE`.
#' @param stratify_columns Logical. If `TRUE`, forces output by strata (columns for each stratum + Overall).
#'   Default is `FALSE`. When `TRUE`, the `horizontal` argument is ignored.
#'
#' @return A `data.frame` with descriptive statistics for continuous covariates:
#'   - If `stratify_columns = TRUE`: Columns for each stratum + Overall.
#'   - If `horizontal = TRUE`: Rows = covariates, columns = statistics.
#'   - If `horizontal = FALSE`: Rows = statistics, columns = covariates (and optionally strata).
#'
#' @examples
#' # Overall summary only
#' CovariateSummary_Table()
#'
#' # Stratify by one covariate (SEX)
#' CovariateSummary_Table(splitCovariates = "SEX")
#'
#' # Stratify by multiple covariates with custom labels
#' CovariateSummary_Table(
#'     splitCovariates = c("SEX", "TINFCAT"),
#'     splitCovariate_labels = c("Sex", "Inf. Dur.")
#' )
#'
#' # Vertical layout
#' CovariateSummary_Table(horizontal = FALSE)
#'
#' # Always by strata (with Overall column)
#' CovariateSummary_Table(splitCovariates = "SEX", stratify_columns = TRUE)
#'
CovariateSummary_Table <- function(projectFile = NULL,
                                   splitCovariates = NULL,
                                   metrics = NULL,
                                   horizontal = TRUE,
                                   stratify_columns = FALSE,
                                   splitCovariate_labels = NULL,
                                   roundDigits = 3,
                                   roundMethod = c("monolix", "signif"),
                                   prettyLabels = TRUE,
                                   mock = list(enable = FALSE, max_rows = 10)) {
    roundMethod <- match.arg(roundMethod)

    # ==========================================================================
    # 1. VALIDATION & SETUP
    .checkProjectFile(projectFile, c("pkanalix", "monolix"))
    .ensureProjectLoaded(projectFile)

    # Validate Stratification Logic: if stratify=TRUE but splitCovariates=NULL -> resets stratify to FALSE and warns.
    stratify_columns <- .validateStratificationArgs(splitCovariates, stratify_columns)

    header_info <- lixoftConnectors::getData()
    cont_cols <- header_info$header[header_info$headerTypes == "contcov"]
    cat_cols <- header_info$header[header_info$headerTypes == "catcov"]

    if (length(cont_cols) == 0) {
        stop("No continuous covariates found in the dataset (headerTypes == 'contcov').")
    }

    # Prepare final_labels (will remain NULL if splitCovariates is NULL)
    final_labels <- NULL

    if (!is.null(splitCovariates)) {
        if (!all(splitCovariates %in% cat_cols)) {
            stop("`splitCovariates` must be categorical covariates (headerTypes == 'catcov').")
        }

        # --- Validate Labels using Helper ---
        final_labels <- .validateSplitCovariateLabels(splitCovariates, splitCovariate_labels)
    }

    # ==========================================================================
    # 2. TABLE FORMATTING
    # ==========================================================================

    if (stratify_columns) {
        # --- MODE 1: STRATIFY COLUMNS ---

        if (!horizontal) {
            message("Note: stratify_columns=TRUE uses a different table format. The 'horizontal' argument is ignored.")
        }

        # Get Stratified Data
        summary_stats <- .prepareCovariateSummaryStats(projectFile, splitCovariates, cont_cols, roundDigits, roundMethod)

        if (is.null(summary_stats)) stop("Failed to calculate statistics for continuous covariates.")

        var_col <- "variable"

        # Determine Metrics
        descriptor_cols <- c(var_col, splitCovariates, "Nobs")

        selected_metrics <- .validateAndSelectMetrics(
            available_cols = names(summary_stats),
            user_metrics   = metrics,
            descriptors    = descriptor_cols
        )

        # Stratified summary (requires Overall)
        if (!is.null(splitCovariates)) {
            overall_stats <- .prepareCovariateSummaryStats(projectFile, NULL, cont_cols, roundDigits, roundMethod)

            # Safety check
            if (!all(selected_metrics %in% names(overall_stats))) {
                stop("Mismatch in metrics between Stratified and Overall results.")
            }

            # Prepare Overall: (Pivot logic for Overall happens inside the selected_metrics check below)
            # Header Generation
            # Use the validated final_labels
            df_prepared <- summary_stats %>%
                dplyr::rowwise() %>%
                dplyr::mutate(header = {
                    values <- dplyr::c_across(dplyr::all_of(splitCovariates))
                    pieces <- paste0(final_labels, "=", as.character(values))
                    paste(pieces, collapse = "\n")
                }) %>%
                dplyr::ungroup()

            n_per_stratum <- df_prepared %>%
                distinct(header, Nobs) %>%
                dplyr::mutate(header_N = paste0(header, "\n(N=", Nobs, ")"))

            if (length(selected_metrics) > 1) {
                # Multiple metrics: Pivot to 'Statistic' column

                overall <- overall_stats %>%
                    dplyr::select(!!rlang::sym(var_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                    dplyr::mutate(header_N = paste0("Overall\n(N=", Nobs, ")")) %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
                    tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Overall") %>%
                    dplyr::select(!!rlang::sym(var_col), Statistic, Overall, header_N)

                df_strat <- df_prepared %>%
                    dplyr::select(!!rlang::sym(var_col), header, dplyr::all_of(selected_metrics)) %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
                    tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
                    tidyr::pivot_wider(names_from = header, values_from = Value)

                df_strat <- df_strat %>%
                    dplyr::rename_with(~ ifelse(.x %in% n_per_stratum$header, n_per_stratum$header_N[match(.x, n_per_stratum$header)], .x))

                overall_label <- unique(overall$header_N)
                summary_stats <- df_strat %>%
                    dplyr::left_join(overall %>% dplyr::select(-header_N), by = c(var_col, "Statistic")) %>%
                    dplyr::rename(!!overall_label := Overall)
            } else {
                # Single metric: Standard Compact format

                overall <- overall_stats %>%
                    dplyr::select(!!rlang::sym(var_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                    dplyr::mutate(header_N = paste0("Overall\n(N=", Nobs, ")")) %>%
                    dplyr::rename(Overall = dplyr::all_of(selected_metrics)) %>%
                    dplyr::select(!!rlang::sym(var_col), Overall, header_N)

                df_strat <- df_prepared %>%
                    dplyr::select(!!rlang::sym(var_col), header, dplyr::all_of(selected_metrics)) %>%
                    tidyr::pivot_wider(names_from = header, values_from = dplyr::all_of(selected_metrics))

                df_strat <- df_strat %>%
                    dplyr::rename_with(~ ifelse(.x %in% n_per_stratum$header, n_per_stratum$header_N[match(.x, n_per_stratum$header)], .x))

                summary_stats <- df_strat %>%
                    dplyr::left_join(overall, by = var_col) %>%
                    dplyr::rename(!!unique(overall$header_N) := Overall) %>%
                    dplyr::select(-header_N)
            }

            names(summary_stats)[names(summary_stats) == var_col] <- "Subject characteristics"
        } else {
            # No strata provided (just Overall)

            if (length(selected_metrics) > 1) {
                summary_stats <- summary_stats %>%
                    dplyr::select(!!rlang::sym(var_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                    dplyr::mutate(header_N = paste0("Overall\n(N=", max(Nobs, na.rm = TRUE), ")")) %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
                    tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
                    tidyr::pivot_wider(names_from = header_N, values_from = Value) %>%
                    dplyr::select(-dplyr::any_of("Nobs"))
            } else {
                summary_stats <- summary_stats %>%
                    dplyr::select(!!rlang::sym(var_col), Nobs, dplyr::all_of(selected_metrics)) %>%
                    dplyr::rename(!!paste0("Overall\n(N=", max(Nobs, na.rm = TRUE), ")") := dplyr::all_of(selected_metrics)) %>%
                    dplyr::select(-dplyr::any_of("Nobs"))
            }

            names(summary_stats)[names(summary_stats) == var_col] <- "Subject characteristics"
        }
    } else {
        # Standard format: HORIZONTAL/VERTICAL
        summary_stats <- .prepareCovariateSummaryStats(projectFile, splitCovariates, cont_cols, roundDigits, roundMethod)

        if (is.null(summary_stats)) stop("Failed to calculate statistics for continuous covariates.")

        var_col <- "variable"

        # Determine Metrics using Helper
        descriptor_cols <- c(var_col, splitCovariates, "Nobs")

        selected_metrics <- .validateAndSelectMetrics(
            available_cols = names(summary_stats),
            user_metrics   = metrics,
            descriptors    = descriptor_cols
        )

        cols_to_keep <- intersect(c(descriptor_cols, selected_metrics), names(summary_stats))
        summary_stats <- summary_stats[, cols_to_keep, drop = FALSE]

        if (horizontal) {
            summary_stats <- summary_stats %>%
                dplyr::mutate(dplyr::across(-dplyr::all_of(c(var_col, splitCovariates)), as.character))

            names(summary_stats)[names(summary_stats) == var_col] <- "Subject characteristics"

            if (!is.null(splitCovariates)) {
                # Apply validated final_labels to column names
                names(summary_stats)[names(summary_stats) %in% splitCovariates] <- final_labels
            }
        } else {
            pivot_id_cols <- if (is.null(splitCovariates)) var_col else c(var_col, splitCovariates)
            stat_cols <- setdiff(names(summary_stats), pivot_id_cols)
            stat_order <- stat_cols

            summary_stats <- summary_stats %>%
                dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), as.character)) %>%
                tidyr::pivot_longer(
                    cols = dplyr::all_of(stat_cols),
                    names_to = "Statistic",
                    values_to = "Value"
                ) %>%
                dplyr::mutate(Statistic = factor(Statistic, levels = stat_order)) %>%
                tidyr::pivot_wider(
                    names_from = dplyr::all_of(var_col),
                    values_from = Value
                )

            if (!is.null(splitCovariates)) {
                # Apply validated final_labels to column names
                names(summary_stats)[names(summary_stats) %in% splitCovariates] <- final_labels
            }
        }
    }

    if (isTRUE(mock$enable)) {
        # Dynamically create the exclusion vector
        if (stratify_columns || (!stratify_columns && horizontal == FALSE)) {
            # Stratified tables (all modes) and Vertical tables have 'Statistic' as a descriptor
            # Stratified: Subject characteristics, Statistic, Stratum A, ...
            # Vertical: Statistic, GroupVar 1, ...
            exclusion_vector <- c("Subject characteristics", "Statistic", final_labels)
        } else {
            # Horizontal (Default) table: Subject characteristics, GroupVar 1, Metric 1, ...
            exclusion_vector <- c("Subject characteristics", final_labels)
        }

        # Clean exclusion vector (remove NULLs/blanks introduced by final_labels if splitCovariates=NULL)
        exclusion_vector <- exclusion_vector[!is.null(exclusion_vector) & exclusion_vector != ""]

        # Execute the mock helper
        summary_stats <- .mockOutputData(
            df = summary_stats,
            mock_settings = list(
                enable = TRUE,
                max_rows = mock$max_rows,
                exclude_cols = exclusion_vector
            ),
            digits = roundDigits,
            method = roundMethod
        )
    }

    if (isTRUE(prettyLabels)) {
        summary_stats <- .prettyLabels(summary_stats)

        # In case metrics are written in Statistic column
        if ("Statistic" %in% names(summary_stats)) {
            curr_metrics <- unique(summary_stats$Statistic)
            dummy_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
            names(dummy_df) <- curr_metrics
            dummy_df <- .prettyLabels(dummy_df)
            metric_map <- setNames(names(dummy_df), curr_metrics)

            summary_stats$Statistic <- metric_map[summary_stats$Statistic]
        }
    }

    return(as.data.frame(summary_stats))
}

#-------------------------------------------------------------------------------
#' CovariateFrequency_Table
#'
#' Generates a frequency table of categorical covariates, showing counts and percentages.
#' Provides overall summaries and optional stratification by one or more categorical covariates.
#' Supports two main output layouts:
#'   - **stratify_columns = TRUE**: Always produces one column per stratum plus an Overall column.
#'   - **stratify_columns = FALSE**: Returns results in long format (rows = covariate × modality × stratum).
#'
#' For each covariate and modality, the function reports the number of subjects
#' and the corresponding percentage within each stratum or overall population.
#'
#' @param splitCovariates A character vector of categorical covariate(s) for stratification
#'   (must come from `headerTypes == 'catcov'`). If `NULL`, only overall frequencies are reported.
#'   Multiple stratifiers will be combined into a single stratum header.
#' @param covariate_labels A character vector of labels for the `splitCovariates` covariates.
#'   Must be the same length as `splitCovariates`. If `NULL`, covariate names are used.
#' @param digits Integer. Number of decimal places to round percentages. Default is 2.
#' @param stratify_columns Logical. If `TRUE`, forces output by strata (columns for each stratum + Overall).
#'   Default is `FALSE`.
#'
#' @details
#'   The function first retrieves categorical covariates from the dataset
#'   (`headerTypes == 'catcov'`). It then:
#'   - Computes subject counts and percentages for each modality.
#'   - If `stratify_columns = FALSE`:
#'       * With no stratifier: returns a simple frequency table.
#'       * With stratifier(s): returns a long-format table including the stratifier values.
#'   - If `stratify_columns = TRUE`:
#'       * Always computes an Overall column (excluding stratifier variables themselves).
#'       * Builds combined stratum headers (e.g. `"Sex=0 | Inf.Dur.=4"`) and creates
#'         one column per stratum.
#'
#' @return A `data.frame` containing counts and percentages for categorical covariates:
#'   - If `stratify_columns = TRUE`: Columns for each stratum + Overall.
#'   - If `stratify_columns = FALSE` and `splitCovariates = NULL`: Simple frequency table.
#'   - If `stratify_columns = FALSE` and `splitCovariates` is set: Long format table with stratifier(s) in rows.
#'
#' @examples
#' # Overall frequencies only
#' CovariateFrequency_Table()
#'
#' # Stratify frequencies by SEX (long format)
#' CovariateFrequency_Table(splitCovariates = "SEX")
#'
#' # Multiple stratifiers with custom labels
#' CovariateFrequency_Table(
#'     splitCovariates = c("SEX", "TINFCAT"),
#'     covariate_labels = c("Sex", "Inf. Dur.")
#' )
#'
#' # By strata layout: one column per stratum + Overall
#' CovariateFrequency_Table(splitCovariates = "SEX", stratify_columns = TRUE)
#'
CovariateFrequency_Table <- function(projectFile = NULL,
                                     splitCovariates = NULL,
                                     stratify_columns = FALSE,
                                     covariate_labels = NULL,
                                     roundDigits = 3,
                                     roundMethod = c("monolix", "signif"),
                                     mock = list(enable = FALSE, max_rows = "all")) {
    roundMethod <- match.arg(roundMethod)

    # ==========================================================================
    # VALIDATION & SETUP

    .checkProjectFile(projectFile, c("pkanalix", "monolix"))
    .ensureProjectLoaded(projectFile)

    # Validate Stratification Logic: if stratify=TRUE but splitCovariates=NULL -> resets stratify to FALSE and warns.
    stratify_columns <- .validateStratificationArgs(splitCovariates, stratify_columns)

    header_info <- lixoftConnectors::getData()
    catcov_col <- header_info$header[header_info$headerTypes == "catcov"]

    if (length(catcov_col) == 0) stop("No categorical covariates found.")

    if (!is.null(splitCovariates)) {
        if (!all(splitCovariates %in% catcov_col)) {
            stop("All variables in `splitCovariates` must be categorical covariates (headerTypes == 'catcov').")
        }
        if (length(setdiff(catcov_col, splitCovariates)) == 0) {
            stop("All categorical covariates were used as stratifiers; none remain to summarize.")
        }
    }

    # Validate Labels
    final_labels <- .validateCovariateLabels(splitCovariates, covariate_labels)
    if (is.null(final_labels) && !is.null(splitCovariates)) final_labels <- splitCovariates

    df_cov <- getCovariateInformation()$covariate

    # ==========================================================================
    # DATA GENERATION

    if (stratify_columns) {
        # STRATIFIED (WIDE)

        vars_to_summarize <- setdiff(catcov_col, splitCovariates)

        df_long_all <- df_cov %>%
            tidyr::pivot_longer(cols = dplyr::all_of(catcov_col), names_to = "variable", values_to = "value")

        # Overall summary
        overall_raw <- df_long_all %>%
            dplyr::filter(!variable %in% splitCovariates) %>%
            .calculate_raw_freq()

        overall <- overall_raw %>%
            .format_frequency_table(roundDigits, roundMethod, exclude_cols = c("variable", "Modalities", "N")) %>%
            dplyr::rename(Overall = `N (%)`)

        if (is.null(splitCovariates)) {
            summary_stats <- overall %>% dplyr::rename(`Subject characteristics` = variable)
        } else {
            # Stratified Data
            df_long <- df_cov %>%
                dplyr::select(id, dplyr::all_of(splitCovariates), dplyr::all_of(catcov_col)) %>%
                tidyr::pivot_longer(cols = dplyr::all_of(vars_to_summarize), names_to = "variable", values_to = "value")

            stratified_df <- df_long %>%
                dplyr::rowwise() %>%
                dplyr::mutate(header = {
                    pieces <- purrr::map2_chr(final_labels, splitCovariates, ~ paste0(.x, "=", as.character(dplyr::pick(dplyr::all_of(.y)))))
                    paste(pieces, collapse = "\n")
                }) %>%
                dplyr::ungroup()

            stratified_result <- stratified_df %>%
                dplyr::group_by(header) %>%
                do(stats = .calc_format_stratum(., roundDigits, roundMethod, exclude_cols = c("variable", "Modalities", "N"))) %>%
                tidyr::unnest(stats) %>%
                tidyr::pivot_wider(names_from = header, values_from = `N (%)`)

            summary_stats <- dplyr::left_join(stratified_result, overall, by = c("variable", "Modalities"))
            names(summary_stats)[1] <- "Subject characteristics"
        }
    } else {
        remaining_catcov <- if (is.null(splitCovariates)) catcov_col else setdiff(catcov_col, splitCovariates)

        df_long <- df_cov %>%
            dplyr::select(id, dplyr::all_of(c(remaining_catcov, splitCovariates))) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(remaining_catcov), names_to = "variable", values_to = "value")

        # Calculate Stats
        result_raw <- df_long %>%
            dplyr::group_by(!!!rlang::syms(splitCovariates), variable, value) %>%
            dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
            dplyr::left_join(
                df_long %>%
                    dplyr::filter(!is.na(value)) %>%
                    dplyr::group_by(!!!rlang::syms(splitCovariates), variable) %>%
                    dplyr::summarise(N = dplyr::n_distinct(id), .groups = "drop"),
                by = c(splitCovariates, "variable")
            ) %>%
            dplyr::mutate(Percentage = (Count / N) * 100) %>%
            dplyr::select(!!!rlang::syms(splitCovariates), variable, Modalities = value, Count, Percentage, N)

        # Format
        summary_stats <- result_raw %>%
            .format_frequency_table(roundDigits, roundMethod, c(splitCovariates, "variable", "Modalities", "Count", "N")) %>%
            dplyr::select(!!!rlang::syms(splitCovariates), `Subject characteristics` = variable, Modalities, Frequency = `N (%)`) %>%
            dplyr::arrange(dplyr::across(dplyr::any_of(splitCovariates)), `Subject characteristics`, Modalities)

        # Apply labels to grouping columns
        if (!is.null(splitCovariates)) {
            names(summary_stats)[names(summary_stats) %in% splitCovariates] <- final_labels
        }
    }

    # ==========================================================================
    # MOCKING (Applied to summary_stats)

    if (isTRUE(mock$enable)) {
        # Create exclusion vector based on table structure
        if (stratify_columns) {
            # Wide format: Exclude descriptors, mock the data columns (Stratums + Overall)
            exclusion_vector <- c("Subject characteristics", "Modalities")
        } else {
            # Long format: Exclude grouping vars + descriptors, mock 'Frequency'
            exclusion_vector <- c("Subject characteristics", "Modalities", final_labels)
        }

        # Clean exclusion vector
        exclusion_vector <- exclusion_vector[!is.null(exclusion_vector) & exclusion_vector != ""]

        summary_stats <- .mockOutputData(
            df = summary_stats,
            mock_settings = list(enable = TRUE, max_rows = mock$max_rows, exclude_cols = exclusion_vector),
            digits = roundDigits,
            method = roundMethod
        )
    }
    return(as.data.frame(summary_stats))
}

#-------------------------------------------------------------------------------
#' Exclusion_Table
#'
#' Generates a summary table of excluded observations and subjects identified
#' during NCA preprocessing. Exclusions are derived from the raw dataset compared
#' with interpreted data and can include individual observations or entire subject
#' records that were removed (e.g., due to MDV flags or filtering rules). Table is
#' based on listing coming from helper Exclusion_Listing()
#'
#' The function provides overall counts or stratified summaries by one or more
#' covariates. Summaries include number of subjects, total number of excluded
#' observations, and per-subject statistics (mean, min, max). The table can be
#' returned in horizontal (wide) or vertical (long) format.
#'
#' @param splitCovariates A character vector of covariates to stratify the summary by.
#'                Must correspond to dataset columns with headerTypes of
#'                'catcov', 'contcov', 'occ', or 'time'. If NULL, an overall
#'                summary (no stratification) is provided.
#' @param horizontal Logical flag indicating table layout. If TRUE (default),
#'                   metrics are returned as columns. If FALSE, metrics are
#'                   pivoted into rows (Statistic–Value format).
#'
#' @return A data frame summarizing excluded observations and subjects. If
#'         \code{horizontal = TRUE}, metrics are shown as columns. If
#'         \code{horizontal = FALSE}, metrics are shown as rows with
#'         corresponding values.
#'
#' @examples
#' # Overall summary of exclusions (no stratification)
#' Exclusion_Table()
#'
#' # Stratified summary by visit
#' Exclusion_Table(splitCovariates = "VISIT")
#'
#' # Stratified summary by visit and food status, in vertical format
#' Exclusion_Table(splitCovariates = c("VISIT", "FOOD"), horizontal = FALSE)
Exclusion_Table <- function(projectFile = NULL, splitCovariates = NULL, horizontal = TRUE) {

    .checkProjectFile(projectFile, c("pkanalix", "monolix"))
    .ensureProjectLoaded(projectFile)

    listing <- Exclusion_Listing(projectFile = projectFile)

    hd <- lixoftConnectors::getData()
    cols <- hd$header
    typ <- hd$headerTypes
    hdr <- tibble::tibble(col = cols, type = typ)
    obs <- hdr$col[hdr$type == "observation"]
    id <- hdr$col[hdr$type == "id"]

    # columns we will NEVER allow to group by
    disallow_types <- c("id", "ignore")

    # columns we DO allow to group by
    allow_types <- c("catcov", "contcov", "occ", "time")

    allowed <- hdr$col[hdr$type %in% allow_types]

    # sanitize requested splitCovariates
    if (!is.null(splitCovariates)) {
        bad <- setdiff(splitCovariates, allowed)
        if (length(bad)) {
            stop(
                "Unsupported grouping column(s): ", paste(bad, collapse = ", "),
                "\nAllowed: ", paste(allowed, collapse = ", ")
            )
        }
    } else {
        splitCovariates <- character(0)
    }

    listing_clean <- listing %>%
        # dplyr::mutate(dplyr::across(dplyr::all_of(obs), ~ dplyr::na_if(., "."))) %>%
        dplyr::filter(obs != "." & !is.na(obs))

    subject_group_vars <- c(id, splitCovariates)

    per_subject <- listing_clean %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(subject_group_vars))) %>%
        dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")


    # Create summary table
    if (length(splitCovariates) > 0) {
        summary_tbl <- as.data.frame(per_subject) %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(splitCovariates))) %>%
            dplyr::summarise(
                `Number of Subjects` = dplyr::n_distinct(.data[[id]]),
                `Total Number of Obs.` = sum(n_obs),
                `Mean No. of Obs./Subject` = round(mean(n_obs), 2),
                `Min. No. of Obs./Subject` = min(n_obs),
                `Max. No. of Obs./Subject` = max(n_obs),
                .groups = "drop"
            ) %>%
            dplyr::relocate(dplyr::all_of(splitCovariates), .before = 1) %>%
            dplyr::arrange(dplyr::across(dplyr::all_of(splitCovariates)))
    } else {
        # Overall summary (no grouping)
        summary_tbl <- as.data.frame(per_subject) %>%
            dplyr::summarise(
                `Number of Subjects` = dplyr::n_distinct(.data[[id]]),
                `Total Number of Obs.` = sum(n_obs),
                `Mean No. of Obs./Subject` = round(mean(n_obs), 2),
                `Min. No. of Obs./Subject` = min(n_obs),
                `Max. No. of Obs./Subject` = max(n_obs)
            ) %>%
            dplyr::arrange(dplyr::across(dplyr::all_of(covariates)))
    }

    if (horizontal == FALSE) {
        stat_cols <- setdiff(names(summary_tbl), covariates)

        summary_tbl <- summary_tbl %>%
            tidyr::pivot_longer(
                cols = dplyr::all_of(stat_cols),
                names_to = "Statistic",
                values_to = "Value"
            )
    }
    attr(summary_tbl, "project_source") <- lixoftConnectors::getProjectSettings()$directory
    return(as.data.frame(summary_tbl))
}
################################################################################
# Helpers for Data Listings
#-------------------------------------------------------------------------------
#' Exclusion_Listing
#'
#' Identifies and returns rows from the raw dataset that were excluded during data filtering
#' in the current NCA project (PKanalix) or PKPD project (Monolix). Compares raw data with
#' interpreted data to find discrepancies and returns the excluded observations as a data frame for review.
#'
#' @return If exclusions are found, returns a data frame containing the excluded rows from
#'         the raw dataset. If no exclusions are found, returns NULL and outputs a message.
#'
#' @examples
#' # Example usage:
#' # excluded_data <- Exclusion_Listing()
# df <- Exclusion_Listing()
Exclusion_Listing <- function(projectFile = NULL) {
    # Ensure the correct project is loaded if explicitly provided
    .checkProjectFile(projectFile, c("pkanalix", "monolix"))
    .ensureProjectLoaded(projectFile)

    dataFile <- NULL # Tracks the raw data file path to ensure consistency
    colNameDataRaw <- NULL

    # Determine which software is being used
    software <- lixoftConnectors::getLixoftConnectorsState()$software

    data <- lixoftConnectors::getData()

    if (nchar(data$dataFile) < 2) {
        dataFileProject <- lixoftConnectors::getFormatting()$dataFile
    } else {
        dataFileProject <- data$dataFile
    }
    if (is.null(dataFile)) {
        dataFile <- dataFileProject
        dataFileExt <- tools::file_ext(dataFile)

        # Read raw data, handling CSV or TXT formats only
        if (dataFileExt %in% c("csv", "txt")) {
            dataRaw <- utils::read.csv(dataFile, header = TRUE, sep = .getDelim(dataFile))
        } else {
            stop("Only CSV or TXT format is currently supported.")
        }
    } else {
        if (basename(dataFileProject) != basename(dataFile)) {
            stop("The basename of each data set file is not the same for each project.")
        }
    }

    # Append interpreted data from each project
    dataUsed <- lixoftConnectors::getInterpretedData()

    # Identify indices for relevant columns in the raw data
    iId <- which(data$headerTypes == "id")
    iTime <- which(data$headerTypes == "time")
    iOcc <- which(data$headerTypes == "occ")
    iMdv <- which(data$headerTypes == "mdv")
    iOccUsed <- iOcc
    colNameDataRaw <- names(dataRaw)

    # check if the occasion were created using the data formatting
    if (length(iOcc) > 0) {
        for (indexOcc in 1:length(iOcc)) {
            nameOcc <- data$header[iOcc[indexOcc]]
            if (!(nameOcc %in% colNameDataRaw)) { # The OCC column was created by data formatting
                iOcc[indexOcc] <- which(colNameDataRaw == sub(x = nameOcc, pattern = "_OCC", ""))
                iOccUsed[indexOcc] <- which(names(dataUsed) == colNameDataRaw[iOcc[indexOcc]])
            }
            iOccUsed[indexOcc] <- which(names(dataUsed) == colNameDataRaw[iOcc[indexOcc]])
        }
    }
    nbOccLevel <- length(iOcc)

    iObs <- which(data$headerTypes == "observation")

    # Retrieve scaling factors based on software type
    if (software == "pkanalix") {
        gainTime <- lixoftConnectors::getDataSettings()$scalings$time
        gainConc <- lixoftConnectors::getDataSettings()$scalings$concentration
    } else if (software == "monolix") {
        # Monolix doesn't have scaling, so use 1 (no scaling)
        gainTime <- 1
        gainConc <- 1
    } else {
        stop(paste("Unsupported software:", software))
    }

    # Initialize a vector to store indices of filtered rows
    indexFiltered <- integer(0)

    # Loop through each row in the raw data to identify filtered rows
    for (indexRaw in seq_len(nrow(dataRaw))) {
        rawLine <- dataRaw[indexRaw, ]

        # First check if this row has MDV=1 (or equivalent ignored flag)
        isMdvFiltered <- FALSE
        if (!is.null(iMdv) && length(iMdv) > 0) {
            mdvValue <- suppressWarnings(as.numeric(rawLine[iMdv]))
            if (!is.na(mdvValue) && mdvValue == 1) {
                isMdvFiltered <- TRUE
            }
        }

        # Create a logical vector to find matching rows in the interpreted dataset
        # Apply time scaling based on software
        if (software == "pkanalix") {
            bInter <- (dataUsed[, iId] == as.character(rawLine[iId])) &
                (as.numeric(dataUsed[, iTime]) == as.numeric(rawLine[iTime] * gainTime))
        } else if (software == "monolix") {
            # No scaling for Monolix
            bInter <- (dataUsed[, iId] == as.character(rawLine[iId])) &
                (as.numeric(dataUsed[, iTime]) == as.numeric(rawLine[iTime]))
        }

        # Check occurrence levels if they exist
        if (nbOccLevel > 0) {
            for (indexOcc in seq_len(nbOccLevel)) {
                bInter <- bInter & (dataUsed[, iOccUsed[indexOcc]] == as.character(rawLine[iOcc[indexOcc]]))
            }
        }

        indexInter <- which(bInter)

        # If this is an MDV=1 row and no match found in interpreted data, it's filtered
        if (isMdvFiltered) {
            indexFiltered <- c(indexFiltered, indexRaw)
            next # Skip to next row
        }

        # If no match is found in interpreted data, mark as filtered
        if (length(indexInter) == 0) {
            # No match in id#Occ#OCC between the raw data and the used data
            indexFiltered <- c(indexFiltered, indexRaw)
        } else {
            # Check the value of the observation with appropriate scaling
            obsValue <- suppressWarnings(as.numeric(rawLine[iObs]))
            obsValueRaw <- as.character(rawLine[iObs]) # Keep original string value

            # Find matching rows in interpreted data for this ID/Time/Occasion combination
            matchingRows <- dataUsed[indexInter, ]

            # Check if observation values match
            foundMatch <- FALSE
            for (idx in seq_len(nrow(matchingRows))) {
                interpValue <- as.character(matchingRows[idx, iObs])
                interpValueNum <- suppressWarnings(as.numeric(interpValue))

                if (!is.na(obsValue) && !is.na(interpValueNum)) {
                    # Both are numeric - compare with scaling and tolerance
                    if (software == "pkanalix") {
                        if (abs(interpValueNum - obsValue * gainConc) <= (obsValue * gainConc) * 1e-4) {
                            foundMatch <- TRUE
                            break
                        }
                    } else if (software == "monolix") {
                        if (abs(interpValueNum - obsValue) <= obsValue * 1e-4) {
                            foundMatch <- TRUE
                            break
                        }
                    }
                } else if (is.na(obsValue) && (interpValue == "" || interpValue == "." || is.na(interpValueNum))) {
                    # Both are non-numeric/missing
                    foundMatch <- TRUE
                    break
                } else if (obsValueRaw == interpValue) {
                    # Exact string match (handles BLQ, etc.)
                    foundMatch <- TRUE
                    break
                }
            }

            if (!foundMatch) {
                # No match in observation value between the raw data and the used data
                indexFiltered <- c(indexFiltered, indexRaw)
            }
        }
    }

    # Output filtered rows if any are found
    if (length(indexFiltered) > 0) {
        message(paste0(length(indexFiltered), " lines were filtered in ", software))
        excl <- dataRaw[indexFiltered, ]
        attr(excl, "project_source") <- lixoftConnectors::getProjectSettings()$directory
        return(excl)
    } else {
        message(paste0("All the lines are in the ", software, " project, no table of exclusions is created."))
        return(NULL)
    }
}
#-------------------------------------------------------------------------------

#' DosingRegimen_Listing
#' Builds a dosing regimen listing per subject/occasion with dose amount and time,
#' optionally adding a covariate column (from the dataset) or a constant entry
#' (e.g., ROUTE) when the covariate is not present. If the custom NCA parameter
#' `InfDur` was computed, it is merged on the subject identifier.
#'
#' @param covariate Character scalar; name of a covariate column present in the
#'   interpreted dataset. Mutually exclusive with covariate.entry.
#' @param covariate.entry Character scalar; constant value to inject as a column
#'   (named ROUTE) when the covariate is not available in the dataset.
#'   Mutually exclusive with covariate.
#'

#'
#' @return A data frame with columns: id, occasion columns (if any), time,
#' amount, optional covariate or ROUTE, and optional InfDur.
#'
#' @examples
#' # From dataset covariate:
#' # DosingRegimen_Listing(covariate = "ROUTE")
#' #
#' # Inject constant when covariate absent:
#' # DosingRegimen_Listing(covariate.entry = "oral")
DosingRegimen_Listing <- function(projectFile = NULL,
                                  covariate = NULL,
                                  covariate.entry = NULL) {
    # Ensure the correct project is loaded
    .checkProjectFile(projectFile, c("pkanalix", "monolix"))
    .ensureProjectLoaded(projectFile)

    # CASE 1: covariate and covariate.entry are mutually exclusive: only one should be set depending on whether the covariate exists in the dataset
    if (!is.null(covariate) && !is.null(covariate.entry)) {
        stop(paste("Invalid input: covariate:'", covariate, "' and covariate.entry:'", covariate.entry, "' are mutually exclusive."))
    }

    headers <- lixoftConnectors::getData()$header
    header_types <- lixoftConnectors::getData()$headerTypes

    amount_col <- lixoftConnectors::getData()$header[which(lixoftConnectors::getData()$headerTypes == "amount")]
    time_col <- lixoftConnectors::getData()$header[which(lixoftConnectors::getData()$headerTypes == "time")]
    id_col <- lixoftConnectors::getData()$header[which(lixoftConnectors::getData()$headerTypes == "id")]

    occ_cols <- headers[which(header_types == "occ")]

    df <- lixoftConnectors::getInterpretedData()

    # Build initial listing
    listing <- df %>%
        dplyr::select(dplyr::all_of(c(id_col, occ_cols, time_col, amount_col))) %>%
        dplyr::filter(!is.na(.data[[amount_col]]) & .data[[amount_col]] != "") %>%
        dplyr::rename(id = dplyr::all_of(id_col)) # rename to id column header used in lixoftConnectors::getNCAIndividualParameters()$parameters

    # CASE 2: Covariate exists in dataset
    if (!is.null(covariate) && is.null(covariate.entry)) {
        if (!(covariate %in% colnames(df))) {
            stop(paste("Covariate", covariate, "not found in dataset"))
        }
        listing[[covariate]] <- df %>%
            dplyr::filter(!is.na(!!rlang::sym(amount_col)) & !!rlang::sym(amount_col) != "") %>%
            dplyr::pull(!!rlang::sym(covariate))
    }

    # CASE 3: Covariate not in dataset – inject constant value
    if (is.null(covariate) && !is.null(covariate.entry)) {
        listing[["ROUTE"]] <- covariate.entry
    }

    # Merge with InfDur if available — using the same ID column name
    if (any(lixoftConnectors::getNCASettings()$computedncaparameters == "InfDur")) {
        df_InfDur <- as.data.frame(lixoftConnectors::getNCAIndividualParameters()$parameters)

        df_InfDur <- df_InfDur %>% dplyr::select("id", "InfDur")

        listing <- merge(listing, df_InfDur, by = c("id"))
    }
    attr(listing, "project_source") <- lixoftConnectors::getProjectSettings()$directory
    return(listing)
}
