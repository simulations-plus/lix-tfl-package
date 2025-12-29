################################################################################
#-------------------Quarto Report: Non Compartmental analysis------------------#
#-----------------------------PKanalix Helpers---------------------------------#
################################################################################

################################################################################
# Public Helper Functions for PKanalix
# Helpers for Tables

#' NCAConcentrationSummary_Table
#'
#' Generates a summary table of concentration data from NCA analysis, with optional
#' stratification by covariates. Transforms concentration data from wide to long format
#' and provides summary statistics across time points.
#'
#' @param covariates A character vector specifying covariate names for stratification.
#'                   If NULL, no stratification is applied.
#'
#' @param useNominalTime Logical; if \code{TRUE}, the function attempts to use
#'   nominal time for the summary table. If nominal time is not available in the
#'   project (checked via \code{lixoftConnectors::getData()} and \code{headerTypes == "nominaltime"}),
#'   the function will fall back to actual time and issue a warning. Default is \code{FALSE}.
#'
#' @details
#' The function resets stratification via \code{lixoftConnectors::setNCAResultsStratification()} and
#' optionally splits results by \code{covariates}. Concentration data are retrieved
#' with \code{lixoftConnectors::getNCAConcentrations()}, using \code{useNominalTime} only if nominal
#' time is set and available. Covariate column order is corrected with
#' \code{fixNCAConcentrationsColumnSwap()}. Only summary rows (statistics) are kept,
#' excluding individual profiles. Data are reshaped into long format, numeric values
#' are rounded, and the results are returned in wide format with one row per
#' timepoint/covariate combination.
#'
#' @return A data frame containing concentration summary statistics organized by time points
#'         and optionally stratified by covariates.
#'
#' @examples
#' # Example usage:
#' # NCAConcentrationSummary_Table()
#' # NCAConcentrationSummary_Table(covariates = c("TREAT", "FOOD"))
#'
NCAConcentrationSummary_Table <- function(project = NULL,
                                          covariates = NULL,
                                          useNominalTime = TRUE,
                                          metrics = NULL,
                                          stratify_columns = FALSE,
                                          covariate_labels = NULL,
                                          roundDigits = 3,
                                          roundMethod = c("monolix", "signif"),
                                          prettyLabels = TRUE) {
  roundMethod <- match.arg(roundMethod)

  # ==========================================================================
  # 1. VALIDATION & SETUP
  # ==========================================================================

  info <- .ensureProjectLoaded(project)
  if (!identical(tolower(info$extension), ".pkx")) {
    stop(paste0("This function is only available for PKanalix projects (.pkx). The current project uses '", info$extension, "'."))
  }

  # Validate Stratification Logic
  stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)

  # Validate Covariate Labels
  stratify_columns <- .validateCovariateLabels(covariates, covariate_labels)

  # Standard metrics default if NULL
  if (is.null(metrics)) {
    user_metrics <- "GeomCV"
  } else {
    user_metrics <- metrics
  }

  # ==========================================================================
  # Fetch Stratified Data (if covariates exist)
  if (!is.null(covariates)) {
    raw_strat <- .prepareNCASummaryStats(
      project = project, NCAparams = NULL, covariates = covariates, roundDigits = roundDigits, roundMethod = roundMethod,
      units = FALSE, aliases = FALSE, by_time = TRUE, useNominalTime = useNominalTime
    )
  } else {
    raw_strat <- NULL
  }

  # Fetch Overall Data (Always needed if we want an Overall column or if no covariates exist)
  raw_overall <- .prepareNCASummaryStats(
    project = project, NCAparams = NULL, covariates = NULL, roundDigits = roundDigits, roundMethod = roundMethod,
    units = FALSE, aliases = FALSE, by_time = TRUE, useNominalTime = useNominalTime
  )

  if (is.null(raw_overall) && is.null(raw_strat)) stop("No NCA results available.")

  # Define Time Column
  time_col <- "Time"

  # ==========================================================================
  # TABLE FORMATTING
  if (stratify_columns && !is.null(covariates)) {
    # ----------------------------------------------------------------------
    # CASE: Stratification (Vertical columns for covariates + Overall)
    # ----------------------------------------------------------------------

    # Validate Metrics existence
    available_cols <- names(raw_strat)
    descriptor_cols <- c(time_col, covariates, "Nobs")
    selected_metrics <- .validateAndSelectMetrics(available_cols, user_metrics, descriptor_cols)

    # Prepare Labels
    if (is.null(covariate_labels)) labels <- covariates else labels <- covariate_labels

    # Process Stratified Data (Create Header)
    # IMPORTANT: strictly select only what we need to avoid dupes
    df_prepared <- raw_strat %>%
      dplyr::select(dplyr::all_of(time_col), dplyr::all_of(covariates), dplyr::all_of(selected_metrics)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(header = {
        values <- dplyr::c_across(dplyr::all_of(covariates))
        # Logic: "Label=Value"
        pieces <- paste0(labels, "=", as.character(values))
        paste(pieces, collapse = "\n")
      }) %>%
      dplyr::ungroup()

    # 4. Process Overall Data
    df_overall_clean <- raw_overall %>%
      dplyr::select(dplyr::all_of(time_col), dplyr::all_of(selected_metrics))


    # 5. PIVOT LOGIC
    #    Distinguish between Single Metric vs. Multiple Metrics

    if (length(selected_metrics) > 1) {
      # MULTIPLE METRICS:
      # Target Format: Time | Statistic | Stratum1 | Stratum2 | ... | Overall

      # Pivot Stratified
      df_strat_wide <- df_prepared %>%
        dplyr::select(dplyr::all_of(time_col), header, dplyr::all_of(selected_metrics)) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
        tidyr::pivot_wider(names_from = header, values_from = Value)

      # Pivot Overall
      df_overall_long <- df_overall_clean %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Overall")

      # Join
      final_df <- df_strat_wide %>%
        dplyr::left_join(df_overall_long, by = c(time_col, "Statistic"))
    } else {
      # SINGLE METRIC:
      # Target Format: Time | Stratum1 | Stratum2 | ... | Overall
      # (No "Statistic" column needed, or kept implicitly if desired, but typically removed for single metric tables)

      metric_name <- selected_metrics[1]

      # Pivot Stratified
      df_strat_wide <- df_prepared %>%
        dplyr::select(dplyr::all_of(time_col), header, dplyr::all_of(metric_name)) %>%
        tidyr::pivot_wider(names_from = header, values_from = dplyr::all_of(metric_name))

      # Prepare Overall
      df_overall_renamed <- df_overall_clean %>%
        dplyr::rename(Overall = dplyr::all_of(metric_name))

      # Join
      final_df <- df_strat_wide %>%
        dplyr::left_join(df_overall_renamed, by = time_col)
    }
  } else {
    # ----------------------------------------------------------------------
    # CASE: No Stratification (Just Overall / List by Time)
    # ----------------------------------------------------------------------

    # Use raw_strat if covariates present (but stratify=FALSE), else raw_overall
    source_df <- if (!is.null(covariates)) raw_strat else raw_overall

    descriptor_cols <- c(time_col, covariates) # Keep covariates as columns on the left
    selected_metrics <- .validateAndSelectMetrics(names(source_df), user_metrics, descriptor_cols)

    # Filter columns
    cols_to_keep <- intersect(c(descriptor_cols, selected_metrics), names(source_df))
    final_df <- source_df[, cols_to_keep, drop = FALSE]

    # Optional: If multiple metrics requested but stratify=FALSE,
    # data is usually wide (Metric1, Metric2 columns).
    # If you want "Statistic" column stacking, add pivot here.
    # Assuming standard behavior is wide for simple lists:
    if (length(selected_metrics) > 1) {
      # Optional: Pivot to long format "Statistic" if preferred for non-stratified too
      # final_df <- final_df %>%
      #   tidyr::pivot_longer(cols=dplyr::all_of(selected_metrics), names_to="Statistic", values_to="Value")
    }
  }

  # ==========================================================================
  # 4. PRETTY LABELS & SORTING
  if (isTRUE(prettyLabels)) {
    final_df <- .prettyLabels(final_df)

    # Apply pretty labels to "Statistic" content if it exists
    if ("Statistic" %in% names(final_df)) {
      curr_metrics <- unique(final_df$Statistic)
      # Create dummy to map labels
      dummy_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
      names(dummy_df) <- curr_metrics
      dummy_df <- .prettyLabels(dummy_df)
      metric_map <- setNames(names(dummy_df), curr_metrics)

      final_df$Statistic <- metric_map[final_df$Statistic]
    }
  }

  # Sort by Time numerically if possible
  if (time_col %in% names(final_df)) {
    final_df <- final_df %>%
      dplyr::arrange(suppressWarnings(as.numeric(!!rlang::sym(time_col))))
  }

  return(as.data.frame(final_df))
}


#-------------------------------------------------------------------------------
#' NCAParameter_byMetric_Table
#'
#' Creates a summary table of NCA parameters organized by metrics (mean and variability measures).
#' Formats the output as "mean (variability)" with optional stratification by covariates.
#' Includes sample size information in column headers.
#'
#' @param covariates A character vector specifying covariate names for stratification.
#'                   If NULL, returns overall summary.
#' @param mean_col A string specifying the column name for the central tendency measure.
#'                 Default is "geoMean".
#' @param spread_col A string specifying the column name for the variability measure.
#'                   Default is "geoCV".
#' @param digits An integer specifying the number of decimal places for rounding. Default is 1.
#'
#' @param labels An optional character vector of the same length as `covariates`, used to replace
#'               covariate names in the output headers. If NULL, covariate names are used as-is.
#'
#' @return A data frame with parameters as rows and formatted "mean (spread)" values as columns,
#'         with sample sizes included in column headers.
#'
#' @examples
#' # Example usage:
#' # NCAParameter_byMetric_Table()
#' # NCAParameter_byMetric_Table(covariates = c("TREAT", "FOOD"), mean_col = "mean", spread_col = "CV")
#' # NCAParameter_byMetric_Table(covariates = c("SEX","TINFCAT"), labels = c("Sex", "Inf.Dur."))
#'
NCAParameter_byMetric_Table <- function(project = NULL,
                                        covariates = NULL,
                                        mean_col = "geoMean",
                                        spread_col = "geoCV",
                                        digits = 1,
                                        labels = NULL) { # vector of same length as covariates

  # Ensure the correct project is loaded
  info <- .ensureProjectLoaded(project)

  # Early validation based on project extension
  if (!identical(tolower(info$extension), ".pkx")) {
    stop(
      paste0(
        "This function is only available for PKanalix projects (.pkx). ",
        "The current project uses '", info$extension, "'."
      )
    )
  }

  # reinitialize stratification connector
  lixoftConnectors::setNCAResultsStratification()

  # Set stratification
  if (!is.null(covariates)) {
    lixoftConnectors::setNCAResultsStratification(split = covariates)
  }

  stat_df <- lixoftConnectors::getNCAParameterStatistics()$statistics

  required_cols <- c("parameter", covariates, mean_col, spread_col, "Nobs")
  missing_cols <- setdiff(required_cols, names(stat_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in input:", paste(missing_cols, collapse = ", ")))
  }

  # -------------------------------------------------------------------------
  # Always compute overall summary (unstratified, across all subjects)
  overall <- stat_df %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(c(mean_col, spread_col)), ~ round(mean(.x, na.rm = TRUE), digits)),
      Nobs = sum(Nobs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Overall = paste0(.data[[mean_col]], " (", .data[[spread_col]], ")"),
      header_N = paste0("Overall\n(N=", max(Nobs, na.rm = TRUE), ")")
    ) %>%
    dplyr::select(parameter, Overall, header_N)

  # -------------------------------------------------------------------------
  # Case 1: Stratified summary
  if (!is.null(covariates)) {
    # use covariate names if no labels provided
    if (is.null(labels)) {
      labels <- covariates
    }

    # build header for each row: "Label1=val1\nLabel2=val2..."
    df_prepared <- stat_df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(c(mean_col, spread_col)), ~ round(.x, digits)),
        value = paste0(.data[[mean_col]], " (", .data[[spread_col]], ")")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(header = {
        pieces <- purrr::map2_chr(labels, covariates, ~ paste0(.x, "=", dplyr::pick(dplyr::all_of(.y))))
        paste(pieces, collapse = "\n")
      }) %>%
      dplyr::ungroup()

    # get Nobs per stratum for header labels
    n_per_stratum <- df_prepared %>%
      distinct(header, Nobs) %>%
      dplyr::mutate(header_N = paste0(header, "\n(N=", Nobs, ")"))

    df_strat <- df_prepared %>%
      dplyr::select(parameter, header, value) %>%
      tidyr::pivot_wider(names_from = header, values_from = value)

    # rename columns to include Nobs once per header
    df_strat <- df_strat %>%
      dplyr::rename_with(~ ifelse(
        .x %in% n_per_stratum$header,
        n_per_stratum$header_N[match(.x, n_per_stratum$header)],
        .x
      ))

    # Merge stratified table with Overall summary, rename correctly and drop helper columns
    df_final <- df_strat %>%
      dplyr::left_join(overall %>% dplyr::select(parameter, Overall, header_N), by = "parameter") %>%
      dplyr::rename(!!unique(overall$header_N) := Overall) %>%
      dplyr::select(-header_N)
  } else {
    # -------------------------------------------------------------------------
    # Case 2: Only overall summary
    df_prepared <- stat_df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(c(mean_col, spread_col)), ~ round(.x, digits)),
        value = paste0(.data[[mean_col]], " (", .data[[spread_col]], ")")
      )

    n_total <- max(df_prepared$Nobs, na.rm = TRUE)

    df_final <- df_prepared %>%
      dplyr::select(parameter, value) %>%
      dplyr::rename(!!paste0("Overall\n(N=", n_total, ")") := value)
  }

  return(df_final)
}


#-------------------------------------------------------------------------------
#' NCAParameterSummary_Table
#'
#' Generates a comprehensive summary table of NCA parameters including both standard
#' statistics from PKanalix and additional descriptive statistics (quartiles, median, max).
#' Combines individual parameter data with summary statistics for complete reporting.
#'
#' @param covariates Character vector of covariate names to stratify by
#'   (e.g., `"TREAT"`, `"FOOD"`, or `c("TREAT","FOOD")`). If `NULL`
#'   (default), no stratification is applied and overall summaries are returned.
#'
#' @param NCAparams Character vector of NCA parameter names to include. If
#'   `NULL` (default), all parameters currently enabled in the project are used
#'   (i.e., `lixoftConnectors::getNCASettings()$computedncaparameters`).
#'
#' @param digits Integer number of decimal places used when rounding numeric
#'   statistics. Default is `3`. This rounding is applied to both the PKanalix
#'   summary statistics and the additional descriptive statistics.
#'
#' @param horizontal Logical. If `TRUE` (default), returns a “wide” table with
#'   one row per covariate level (or overall) and one column per statistic for
#'   each parameter, i.e., PKanalix-like structure. If `FALSE`, returns a
#'   “vertical” table where \code{Statistic} forms the first column and the
#'   parameters form the subsequent columns (useful for reporting)
#'
#' @return A data frame combining standard NCA statistics with additional descriptive statistics
#'         (Q1, Q3, median, max) for the specified parameters.
#'
#' @examples
#' # Overall (no stratification), all computed parameters, wide layout
#' # NCAParameterSummary_Table()
#'
#' # Stratified by a categorical covariate, selected parameters, vertical layout
#' # NCAParameterSummary_Table(
#' #   covariates = "FORM",
#' #   NCAparams  = c("AUCINF_obs", "AUClast", "Cmax"),
#' #   digits     = 2,
#' #   horizontal = FALSE
#' # )
#'
NCAParameterSummary_Table <- function(project = NULL,
                                      covariates = NULL,
                                      NCAparams = NULL,
                                      metrics = NULL,
                                      horizontal = TRUE,
                                      stratify_columns = FALSE,
                                      covariate_labels = NULL,
                                      roundDigits = 3,
                                      roundMethod = c("monolix", "signif"),
                                      units = TRUE,
                                      aliases = TRUE,
                                      prettyLabels = TRUE) {

#
# 	project = prj3
#   NCAparams = c("AUCINF_obs", "AUClast", "Cmax")
# 	metrics = c("MeanSD", "GeomCV")
# 	horizontal = TRUE
# 	stratify_columns = TRUE
# 	covariates = c("DAY", "DOSE_mg_kg_")
# 	roundMethod = "monolix"

  roundMethod <- match.arg(roundMethod)

  # ==========================================================================
  # 1. VALIDATION & SETUP
  # ==========================================================================

  info <- .ensureProjectLoaded(project)

  if (!identical(tolower(info$extension), ".pkx")) {
    stop(paste0("This function is only available for PKanalix projects (.pkx). The current project uses '", info$extension, "'."))
  }

  # Validate Stratification Logic: if stratify=TRUE but covariates=NULL -> resets stratify to FALSE and warns.
  stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)

  # ==========================================================================
  # 2. TABLE FORMATTING
  # ==========================================================================

  if (stratify_columns) {
    if (!horizontal) {
      message("Note: stratify_columns=TRUE uses a different table format. The 'horizontal' argument is ignored.")
    }

    # Determine Metrics:
    # For Stratified NCA, we default to "GeomCV" if NULL, unlike Covariates which defaults to all.
    # This prevents the table from exploding in size by default.
    if (is.null(metrics)) {
      user_metrics <- "GeomCV"
      message("No metric specified for stratified table. Using default: 'GeomCV'")
    } else {
      user_metrics <- metrics
    }

    # Get stratified data
    # summary_stats <- .prepareNCASummaryStats(project, NCAparams, covariates, roundDigits, roundMethod, units, aliases)
    summary_stats <- .prepareNCASummaryStats(project, NCAparams, covariates, roundDigits, roundMethod, units, aliases, by_time = FALSE, useNominalTime = FALSE)

    if (is.null(summary_stats)) stop("No NCA results available.")

    # Identify parameter column
    param_col <- "parameter"
    if (!(param_col %in% names(summary_stats))) {
      param_col <- intersect(names(summary_stats), c("parameter", "Parameter"))[1]
    }

    # --- 3. Validate Selection using Helper ---
    descriptor_cols <- c(param_col, covariates, "Nobs")

    selected_metrics <- .validateAndSelectMetrics(
      available_cols = names(summary_stats),
      user_metrics   = user_metrics,
      descriptors    = descriptor_cols
    )

    # --- Case 1: Stratified summary (requires Overall) ---
    if (!is.null(covariates)) {
      # overall_stats <- .prepareNCASummaryStats(project, NCAparams, NULL, roundDigits, roundMethod, units, aliases)
      overall_stats <- .prepareNCASummaryStats(project, NCAparams, NULL, roundDigits, roundMethod, units, aliases, by_time = FALSE, useNominalTime = FALSE)

      # Safety check
      if (!all(selected_metrics %in% names(overall_stats))) {
        stop("Mismatch in metrics between Stratified and Overall results.")
      }

      # 'Overall' column(s)
      overall <- overall_stats %>%
        dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
        dplyr::mutate(header_N = paste0("Overall\n(N=", Nobs, ")"))

      if (is.null(covariate_labels)) labels <- covariates else labels <- covariate_labels

      df_prepared <- summary_stats %>%
        dplyr::rowwise() %>%
        dplyr::mutate(header = {
          values <- dplyr::c_across(dplyr::all_of(covariates))
          pieces <- paste0(labels, "=", as.character(values))
          paste(pieces, collapse = "\n")
        }) %>%
        dplyr::ungroup()

      n_per_stratum <- df_prepared %>%
        distinct(header, Nobs) %>%
        dplyr::mutate(header_N = paste0(header, "\n(N=", Nobs, ")"))

      # --- PIVOT LOGIC ---

      if (length(selected_metrics) > 1) {
        # === MULTIPLE METRICS: Pivot to 'Statistic' column ===

        # Pivot Overall
        overall <- overall %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
          tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Overall") %>%
          dplyr::select(!!rlang::sym(param_col), Statistic, Overall, header_N)

        # Pivot Stratified
        df_strat <- df_prepared %>%
          dplyr::select(!!rlang::sym(param_col), header, dplyr::all_of(selected_metrics)) %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
          tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
          tidyr::pivot_wider(names_from = header, values_from = Value)

        df_strat <- df_strat %>%
          dplyr::rename_with(~ ifelse(
            .x %in% n_per_stratum$header,
            n_per_stratum$header_N[match(.x, n_per_stratum$header)],
            .x
          ))

        overall_label <- unique(overall$header_N)
        summary_stats <- df_strat %>%
          dplyr::left_join(overall %>% dplyr::select(-header_N), by = c(param_col, "Statistic")) %>%
          dplyr::rename(!!overall_label := Overall)
      } else {
        # Single metric:
        overall <- overall %>%
          dplyr::rename(Overall = dplyr::all_of(selected_metrics)) %>%
          dplyr::select(!!rlang::sym(param_col), Overall, header_N)

        df_strat <- df_prepared %>%
          dplyr::select(!!rlang::sym(param_col), header, dplyr::all_of(selected_metrics)) %>%
          tidyr::pivot_wider(names_from = header, values_from = dplyr::all_of(selected_metrics))

        df_strat <- df_strat %>%
          dplyr::rename_with(~ ifelse(
            .x %in% n_per_stratum$header,
            n_per_stratum$header_N[match(.x, n_per_stratum$header)],
            .x
          ))

        summary_stats <- df_strat %>%
          dplyr::left_join(overall, by = param_col) %>%
          dplyr::rename(!!unique(overall$header_N) := Overall) %>%
          dplyr::select(-header_N)
      }
    } else {
      # --- Case 2: No strata provided (just Overall) ---

      if (length(selected_metrics) > 1) {
        summary_stats <- summary_stats %>%
          dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
          dplyr::mutate(header_N = paste0("Overall\n(N=", max(Nobs, na.rm = TRUE), ")")) %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
          tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
          tidyr::pivot_wider(names_from = header_N, values_from = Value) %>%
          dplyr::select(-dplyr::any_of("Nobs"))
      } else {
        summary_stats <- summary_stats %>%
          dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
          dplyr::rename(!!paste0("Overall\n(N=", max(Nobs, na.rm = TRUE), ")") := dplyr::all_of(selected_metrics)) %>%
          dplyr::select(-dplyr::any_of("Nobs"))
      }
    }
  } else {
    # summary_stats <- .prepareNCASummaryStats(project, NCAparams, covariates, roundDigits, roundMethod, units, aliases)
    summary_stats <- .prepareNCASummaryStats(project, NCAparams, covariates, roundDigits, roundMethod, units, aliases, by_time = FALSE, useNominalTime = FALSE)
    if (is.null(summary_stats)) stop("No NCA results available.")

    # Identify parameter column
    param_col <- "parameter"
    if (!(param_col %in% names(summary_stats))) {
      param_col <- intersect(names(summary_stats), c("parameter", "Parameter"))[1]
    }

    # Determine metrics using internal helper
    # If metrics=NULL here, the helper returns ALL available (standard behavior)
    descriptor_cols <- c(param_col, covariates, "Nobs")

    selected_metrics <- .validateAndSelectMetrics(
      available_cols = names(summary_stats),
      user_metrics   = metrics,
      descriptors    = descriptor_cols
    )

    # Filter dataframe
    cols_to_keep <- intersect(c(descriptor_cols, selected_metrics), names(summary_stats))
    summary_stats <- summary_stats[, cols_to_keep, drop = FALSE]

    if (!horizontal) {
      # --- MODE 2: VERTICAL ---
      stat_cols <- setdiff(names(summary_stats), c(param_col, covariates))
      param_order <- unique(summary_stats[[param_col]])
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
          id_cols = c(dplyr::any_of(covariates), Statistic),
          names_from = dplyr::all_of(param_col),
          values_from = Value
        ) %>%
        dplyr::arrange(dplyr::across(dplyr::any_of(covariates)), Statistic) %>%
        dplyr::select(dplyr::any_of(covariates), Statistic, dplyr::all_of(as.character(param_order)))
    }
    # HORIZONTAL (DEFAULT)
    # No additional pivoting needed
  }

  if (isTRUE(prettyLabels)) {
    summary_stats <- .prettyLabels(summary_stats)

    # Apply pretty labels to the content of the 'Statistic' column if it exists
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
#' NCARatioSummary_Table
#'
#' Create a summary table of individual NCA ratio results with optional
#' stratification by covariates (e.g., DOSE, VISIT).
#'
#' The function:
#'  1. Pulls individual ratios (`lixoftConnectors::getNCAIndividualRatios()`) and ratio info (`lixoftConnectors::getNCARatios()`).
#'  2. Identifies candidate grouping variables (all columns except id and ratio columns).
#'  3. Reshapes the data into long format (Parameter, Ratio).
#'  4. Summarises across subjects to provide descriptive statistics (N, Min, Q1, Mean, Median, Q3, Max).
#'  5. Returns results either in wide format (one row per Parameter × group) or long format (Statistic/Value pairs).
#'
#' @param groupby character vector of column names to stratify by (must be in indivratio but not id/ratio columns).
#' @param digits number of digits to round numeric statistics (default = 3).
#' @param horizontal logical; if TRUE (default) results are wide; if FALSE, results are pivoted to long format.
#'
#' @return A data.frame with summary statistics for each ratio parameter, optionally stratified.
#'
#' @examples
#' # Overall summary (all subjects, no stratification):
#' NCARatioSummary_Table()
#'
#' # Stratified by DOSE, results in wide format:
#' NCARatioSummary_Table(groupby = "DOSE", horizontal = TRUE)
#'
#' # Stratified by DOSE and VISIT, results in long format:
#' NCARatioSummary_Table(groupby = c("DOSE", "VISIT"), horizontal = FALSE)
#'
NCARatioSummary_Table <- function(project = NULL,
                                  groupby = NULL,
                                  ratioParams = NULL,
                                  metrics = NULL,
                                  horizontal = TRUE,
                                  roundDigits = 3,
                                  roundMethod = c("monolix", "signif"),
                                  prettyLabels = TRUE) {
  roundMethod <- match.arg(roundMethod)

  # Ensure the correct project is loaded
  info <- .ensureProjectLoaded(project)

  # Early validation based on project extension
  if (!identical(tolower(info$extension), ".pkx")) {
    stop(
      paste0(
        "This function is only available for PKanalix projects (.pkx). ",
        "The current project uses '", info$extension, "'."
      )
    )
  }

  indivratio <- lixoftConnectors::getNCAIndividualRatios()
  ratio_info <- lixoftConnectors::getNCARatios()
  ratio_cols <- names(ratio_info)

  # Candidate group-by columns: in dataframe but not id and not ratio(s)
  groupby_candidates <- setdiff(names(indivratio), c("id", ratio_cols))

  # Sanitize requested groupby
  if (is.null(groupby)) groupby <- character(0)
  bad <- setdiff(groupby, groupby_candidates)
  if (length(bad)) {
    stop(
      "Unsupported grouping column(s): ", paste(bad, collapse = ", "),
      "\nAllowed: ", paste(groupby_candidates, collapse = ", ")
    )
  }

  # Long form over ALL ratio columns
  long_df <- tidyr::pivot_longer(
    indivratio,
    cols      = dplyr::all_of(ratio_cols),
    names_to  = "Parameter",
    values_to = "Ratio"
  )

  # Filter to specific ratio parameters (if requested)
  # ratioParams sanity check
  if (!is.null(ratioParams)) {
    available_params <- unique(long_df$Parameter)
    missing_params <- setdiff(ratioParams, available_params)
    valid_params <- intersect(ratioParams, available_params)

    if (!length(valid_params)) {
      stop(
        paste0(
          "None of the requested ratio parameters were found.\n",
          "Requested: ", paste(ratioParams, collapse = ", "), "\n",
          "Available: ", paste(available_params, collapse = ", ")
        )
      )
    }
    if (length(missing_params)) {
      warning(
        paste0(
          "Ignoring unknown ratio parameters: ",
          paste(missing_params, collapse = ", ")
        )
      )
    }
    long_df <- dplyr::filter(long_df, Parameter %in% valid_params)
  }

  # Create summary table & summarise across subjects
  if (length(groupby) > 0) {
    # Grouped summary
    summary_tbl <- .summarizeStats(
      df         = long_df,
      group_vars = c(groupby, "Parameter"),
      value_col  = "Ratio",
      id_col     = "id"
    ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(groupby, "Parameter"))))
  } else {
    summary_tbl <- .summarizeStats(
      df         = long_df,
      group_vars = "Parameter",
      value_col  = "Ratio",
      id_col     = "id"
    ) %>%
      dplyr::arrange(Parameter)
  }

  # -------------------------------------------------
  # Apply rounding settings
  summary_tbl <- .roundNumericCols(
    summary_tbl,
    digits          = roundDigits,
    rounding_method = roundMethod,
    exclude         = c("Parameter", groupby)
  )

  # Add combined metrics
  summary_tbl <- .combinedStats(summary_tbl)

  # Filter to specific metrics (if requested)
  # metrics sanity check: use internal helper for consistent validation
  descriptor_cols <- c("Parameter", groupby, "Nobs")
  # Note: standardizing count column name if necessary, though usually .summarizeStats is consistent

  selected_metrics <- .validateAndSelectMetrics(
    available_cols = names(summary_tbl),
    user_metrics   = metrics,
    descriptors    = descriptor_cols
  )

  # keep descriptor columns (Parameter + grouping vars) and requested metrics
  summary_tbl <- summary_tbl[, c(descriptor_cols, selected_metrics), drop = FALSE]

  if (isTRUE(prettyLabels)) {
    summary_tbl <- .prettyLabels(summary_tbl)
  }

  if (horizontal == FALSE) {
    param_col <- intersect(names(summary_tbl), c("parameter", "Parameter")) # Identify parameter column (handles both "parameter" and "Parameter")

    stat_cols <- setdiff(names(summary_tbl), c(param_col, groupby)) # Identify which columns are stats (everything else)

    # Define order for pivoting
    stat_order <- stat_cols

    summary_tbl <- summary_tbl %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), as.character)) %>% # <--- force character to pivot
      tidyr::pivot_longer(
        cols = dplyr::all_of(stat_cols),
        names_to = "Statistic",
        values_to = "Value"
      ) %>%
      dplyr::mutate(Statistic = factor(Statistic, levels = stat_order)) %>%
      tidyr::pivot_wider(
        id_cols = c(dplyr::any_of(groupby), Statistic),
        names_from = dplyr::all_of(param_col),
        values_from = Value
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(groupby)), Statistic)
  }

  return(as.data.frame(summary_tbl))
}


#-------------------------------------------------------------------------------
#' CovariateSummary_Table
#'
#' Generates a summary table of continuous covariates with descriptive statistics.
#' Provides overall summary and optional stratification by one or more categorical covariates.
#' Supports different output layouts:
#'   - **by_strata = TRUE**: Always produces one column per stratum + Overall column.
#'   - **by_strata = FALSE, horizontal = TRUE**: Wide layout with one row per covariate and columns for statistics.
#'   - **by_strata = FALSE, horizontal = FALSE**: Long layout with one row per statistic and columns for covariates.
#'
#' Statistics include: N, min, max, mean, SD, CV, median, Q1, Q3, 5th/95th percentiles,
#' as well as a combined Mean (SD) column.
#'
#' @param stratify_by A character vector of categorical covariate(s) for stratification
#'   (must come from `headerTypes == 'catcov'`). If `NULL`, only overall statistics are reported.
#'   Multiple covariates will be combined into a single strata label.
#' @param strata_labels A character vector of labels for the `stratify_by` covariates.
#'   Must be the same length as `stratify_by`. If `NULL`, covariate names are used.
#' @param digits Integer. Number of decimal places to round statistics. Default is 2.
#' @param horizontal Logical. If `TRUE` (default), results are returned in wide layout
#'   (variables in rows, statistics in columns). Ignored when `by_strata = TRUE`.
#' @param by_strata Logical. If `TRUE`, forces output by strata (columns for each stratum + Overall).
#'   Default is `FALSE`. When `TRUE`, the `horizontal` argument is ignored.
#'
#' @return A `data.frame` with descriptive statistics for continuous covariates:
#'   - If `by_strata = TRUE`: Columns for each stratum and Overall.
#'   - If `horizontal = TRUE`: Rows = covariates, columns = statistics.
#'   - If `horizontal = FALSE`: Rows = statistics, columns = covariates (and optionally strata).
#'
#' @examples
#' # Overall summary only
#' CovariateSummary_Table()
#'
#' # Stratify by one covariate (SEX)
#' CovariateSummary_Table(stratify_by = "SEX")
#'
#' # Stratify by multiple covariates with custom labels
#' CovariateSummary_Table(
#'   stratify_by = c("SEX", "TINFCAT"),
#'   strata_labels = c("Sex", "Inf. Dur.")
#' )
#'
#' # Vertical layout
#' CovariateSummary_Table(horizontal = FALSE)
#'
#' # Always by strata (with Overall column)
#' CovariateSummary_Table(stratify_by = "SEX", by_strata = TRUE)
#'
CovariateSummary_Table <- function(project = NULL,
                                   covariates = NULL,
                                   metrics = NULL,
                                   horizontal = TRUE,
                                   stratify_columns = FALSE,
                                   covariate_labels = NULL,
                                   roundDigits = 3,
                                   roundMethod = c("monolix", "signif"),
                                   prettyLabels = TRUE,
                                   mock = list(enable = FALSE, max_rows = 10)) {
  roundMethod <- match.arg(roundMethod)

  # ==========================================================================
  # 1. VALIDATION & SETUP
  .ensureProjectLoaded(project)

  # Validate Stratification Logic: if stratify=TRUE but covariates=NULL -> resets stratify to FALSE and warns.
  stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)

  header_info <- lixoftConnectors::getData()
  cont_cols <- header_info$header[header_info$headerTypes == "contcov"]
  cat_cols <- header_info$header[header_info$headerTypes == "catcov"]

  if (length(cont_cols) == 0) {
    stop("No continuous covariates found in the dataset (headerTypes == 'contcov').")
  }

  # Prepare final_labels (will remain NULL if covariates is NULL)
  final_labels <- NULL

  if (!is.null(covariates)) {
    if (!all(covariates %in% cat_cols)) {
      stop("`covariates` must be categorical covariates (headerTypes == 'catcov').")
    }

    # --- Validate Labels using Helper ---
    final_labels <- .validateCovariateLabels(covariates, covariate_labels)
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
    summary_stats <- .prepareCovariateSummaryStats(project, covariates, cont_cols, roundDigits, roundMethod)

    if (is.null(summary_stats)) stop("Failed to calculate statistics for continuous covariates.")

    var_col <- "variable"

    # Determine Metrics
    descriptor_cols <- c(var_col, covariates, "Nobs")

    selected_metrics <- .validateAndSelectMetrics(
      available_cols = names(summary_stats),
      user_metrics   = metrics,
      descriptors    = descriptor_cols
    )

    # Stratified summary (requires Overall)
    if (!is.null(covariates)) {
      overall_stats <- .prepareCovariateSummaryStats(project, NULL, cont_cols, roundDigits, roundMethod)

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
          values <- dplyr::c_across(dplyr::all_of(covariates))
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
    summary_stats <- .prepareCovariateSummaryStats(project, covariates, cont_cols, roundDigits, roundMethod)

    if (is.null(summary_stats)) stop("Failed to calculate statistics for continuous covariates.")

    var_col <- "variable"

    # Determine Metrics using Helper
    descriptor_cols <- c(var_col, covariates, "Nobs")

    selected_metrics <- .validateAndSelectMetrics(
      available_cols = names(summary_stats),
      user_metrics   = metrics,
      descriptors    = descriptor_cols
    )

    cols_to_keep <- intersect(c(descriptor_cols, selected_metrics), names(summary_stats))
    summary_stats <- summary_stats[, cols_to_keep, drop = FALSE]

    if (horizontal) {
      summary_stats <- summary_stats %>%
        dplyr::mutate(dplyr::across(-dplyr::all_of(c(var_col, covariates)), as.character))

      names(summary_stats)[names(summary_stats) == var_col] <- "Subject characteristics"

      if (!is.null(covariates)) {
        # Apply validated final_labels to column names
        names(summary_stats)[names(summary_stats) %in% covariates] <- final_labels
      }
    } else {
      pivot_id_cols <- if (is.null(covariates)) var_col else c(var_col, covariates)
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

      if (!is.null(covariates)) {
        # Apply validated final_labels to column names
        names(summary_stats)[names(summary_stats) %in% covariates] <- final_labels
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

    # Clean exclusion vector (remove NULLs/blanks introduced by final_labels if covariates=NULL)
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
#'   - **by_strata = TRUE**: Always produces one column per stratum plus an Overall column.
#'   - **by_strata = FALSE**: Returns results in long format (rows = covariate × modality × stratum).
#'
#' For each covariate and modality, the function reports the number of subjects
#' and the corresponding percentage within each stratum or overall population.
#'
#' @param stratify_by A character vector of categorical covariate(s) for stratification
#'   (must come from `headerTypes == 'catcov'`). If `NULL`, only overall frequencies are reported.
#'   Multiple stratifiers will be combined into a single stratum header.
#' @param strata_labels A character vector of labels for the `stratify_by` covariates.
#'   Must be the same length as `stratify_by`. If `NULL`, covariate names are used.
#' @param digits Integer. Number of decimal places to round percentages. Default is 2.
#' @param by_strata Logical. If `TRUE`, forces output by strata (columns for each stratum + Overall).
#'   Default is `FALSE`.
#'
#' @details
#'   The function first retrieves categorical covariates from the dataset
#'   (`headerTypes == 'catcov'`). It then:
#'   - Computes subject counts and percentages for each modality.
#'   - If `by_strata = FALSE`:
#'       * With no stratifier: returns a simple frequency table.
#'       * With stratifier(s): returns a long-format table including the stratifier values.
#'   - If `by_strata = TRUE`:
#'       * Always computes an Overall column (excluding stratifier variables themselves).
#'       * Builds combined stratum headers (e.g. `"Sex=0 | Inf.Dur.=4"`) and creates
#'         one column per stratum.
#'
#' @return A `data.frame` containing counts and percentages for categorical covariates:
#'   - If `by_strata = TRUE`: Columns for each stratum + Overall.
#'   - If `by_strata = FALSE` and `stratify_by = NULL`: Simple frequency table.
#'   - If `by_strata = FALSE` and `stratify_by` is set: Long format table with stratifier(s) in rows.
#'
#' @examples
#' # Overall frequencies only
#' CovariateFrequency_Table()
#'
#' # Stratify frequencies by SEX (long format)
#' CovariateFrequency_Table(stratify_by = "SEX")
#'
#' # Multiple stratifiers with custom labels
#' CovariateFrequency_Table(
#'   stratify_by = c("SEX", "TINFCAT"),
#'   strata_labels = c("Sex", "Inf. Dur.")
#' )
#'
#' # By strata layout: one column per stratum + Overall
#' CovariateFrequency_Table(stratify_by = "SEX", by_strata = TRUE)
#'
CovariateFrequency_Table <- function(project = NULL,
                                     covariates = NULL,
                                     stratify_columns = FALSE,
                                     covariate_labels = NULL,
                                     roundDigits = 3,
                                     roundMethod = c("monolix", "signif"),
                                     mock = list(enable = FALSE, max_rows = "all")) {
  roundMethod <- match.arg(roundMethod)

  # ==========================================================================
  # VALIDATION & SETUP

  .ensureProjectLoaded(project)

  # Validate Stratification Logic: if stratify=TRUE but covariates=NULL -> resets stratify to FALSE and warns.
  stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)

  header_info <- lixoftConnectors::getData()
  catcov_col <- header_info$header[header_info$headerTypes == "catcov"]

  if (length(catcov_col) == 0) stop("No categorical covariates found.")

  if (!is.null(covariates)) {
    if (!all(covariates %in% catcov_col)) {
      stop("All variables in `covariates` must be categorical covariates (headerTypes == 'catcov').")
    }
    if (length(setdiff(catcov_col, covariates)) == 0) {
      stop("All categorical covariates were used as stratifiers; none remain to summarize.")
    }
  }

  # Validate Labels
  final_labels <- .validateCovariateLabels(covariates, covariate_labels)
  if (is.null(final_labels) && !is.null(covariates)) final_labels <- covariates

  df_cov <- getCovariateInformatiodplyr::n()$covariate

  # ==========================================================================
  # DATA GENERATION

  if (stratify_columns) {
    # STRATIFIED (WIDE)

    vars_to_summarize <- setdiff(catcov_col, covariates)

    df_long_all <- df_cov %>%
      tidyr::pivot_longer(cols = dplyr::all_of(catcov_col), names_to = "variable", values_to = "value")

    # Overall summary
    overall_raw <- df_long_all %>%
      dplyr::filter(!variable %in% covariates) %>%
      .calculate_raw_freq()

    overall <- overall_raw %>%
      .format_frequency_table(roundDigits, roundMethod, exclude_cols = c("variable", "Modalities", "N")) %>%
      dplyr::rename(Overall = `N (%)`)

    if (is.null(covariates)) {
      summary_stats <- overall %>% dplyr::rename(`Subject characteristics` = variable)
    } else {
      # Stratified Data
      df_long <- df_cov %>%
        dplyr::select(id, dplyr::all_of(covariates), dplyr::all_of(catcov_col)) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(vars_to_summarize), names_to = "variable", values_to = "value")

      stratified_df <- df_long %>%
        dplyr::rowwise() %>%
        dplyr::mutate(header = {
          pieces <- purrr::map2_chr(final_labels, covariates, ~ paste0(.x, "=", as.character(dplyr::pick(dplyr::all_of(.y)))))
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
    remaining_catcov <- if (is.null(covariates)) catcov_col else setdiff(catcov_col, covariates)

    df_long <- df_cov %>%
      dplyr::select(id, dplyr::all_of(c(remaining_catcov, covariates))) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(remaining_catcov), names_to = "variable", values_to = "value")

    # Calculate Stats
    result_raw <- df_long %>%
      dplyr::group_by(!!!rlang::syms(covariates), variable, value) %>%
      dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
      dplyr::left_join(
        df_long %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(!!!rlang::syms(covariates), variable) %>%
          dplyr::summarise(N = dplyr::n_distinct(id), .groups = "drop"),
        by = c(covariates, "variable")
      ) %>%
      dplyr::mutate(Percentage = (Count / N) * 100) %>%
      dplyr::select(!!!rlang::syms(covariates), variable, Modalities = value, Count, Percentage, N)

    # Format
    summary_stats <- result_raw %>%
      .format_frequency_table(roundDigits, roundMethod, c(covariates, "variable", "Modalities", "Count", "N")) %>%
      dplyr::select(!!!rlang::syms(covariates), `Subject characteristics` = variable, Modalities, Frequency = `N (%)`) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(covariates)), `Subject characteristics`, Modalities)

    # Apply labels to grouping columns
    if (!is.null(covariates)) {
      names(summary_stats)[names(summary_stats) %in% covariates] <- final_labels
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
#' @param groupby A character vector of covariates to stratify the summary by.
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
#' Exclusion_Table(groupby = "VISIT")
#'
#' # Stratified summary by visit and food status, in vertical format
#' Exclusion_Table(groupby = c("VISIT", "FOOD"), horizontal = FALSE)
Exclusion_Table <- function(project = NULL, groupby = NULL, horizontal = TRUE) { # or shall I call it covariates to be more consistent with other helpers?

  listing <- Exclusion_Listing(project = NULL)

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

  # sanitize requested group_by
  if (!is.null(groupby)) {
    bad <- setdiff(groupby, allowed)
    if (length(bad)) {
      stop(
        "Unsupported grouping column(s): ", paste(bad, collapse = ", "),
        "\nAllowed: ", paste(allowed, collapse = ", ")
      )
    }
  } else {
    groupby <- character(0)
  }

  listing_clean <- listing %>%
    # dplyr::mutate(dplyr::across(dplyr::all_of(obs), ~ dplyr::na_if(., "."))) %>%
    dplyr::filter(obs != "." & !is.na(obs))

  subject_group_vars <- c(id, groupby)

  per_subject <- listing_clean %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(subject_group_vars))) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")


  # Create summary table
  if (length(groupby) > 0) {
    summary_tbl <- as.data.frame(per_subject) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groupby))) %>%
      dplyr::summarise(
        `Number of Subjects` = dplyr::n_distinct(.data[[id]]),
        `Total Number of Obs.` = sum(n_obs),
        `Mean No. of Obs./Subject` = round(mean(n_obs), 2),
        `Min. No. of Obs./Subject` = min(n_obs),
        `Max. No. of Obs./Subject` = max(n_obs),
        .groups = "drop"
      ) %>%
      dplyr::relocate(dplyr::all_of(groupby), .before = 1) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(groupby)))
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
      dplyr::arrange(dplyr::across(dplyr::all_of(groupby)))
  }

  if (horizontal == FALSE) {
    stat_cols <- setdiff(names(summary_tbl), groupby)

    summary_tbl <- summary_tbl %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(stat_cols),
        names_to = "Statistic",
        values_to = "Value"
      )
  }
  return(as.data.frame(summary_tbl))
}

################################################################################
# Helpers for Listings

#' DosingRegimen_Listing
#'
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
DosingRegimen_Listing <- function(project = NULL,
                                  covariate = NULL,
                                  covariate.entry = NULL) {
  # Ensure the correct project is loaded
  ensureProjectLoaded(project)

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
  return(listing)
}


#-------------------------------------------------------------------------------
#' Abbreviation_Listing
#'
#' Produces a two-column abbreviation listing initialized from an input table,
#' returning unique abbreviations and an empty \code{Full Term} column for later
#' completion, sorted alphabetically.
#'
#' @param data A data frame containing an abbreviation column or a \code{parameter}
#'   column that will be mapped to the abbreviation list.
#' @param abbrev_col Character scalar; name of the input column that holds the
#'   abbreviations. Defaults to \code{"Abbreviation"}. If not present, the
#'   function will construct it from \code{data$parameter}.
#'
#' @return A data frame with columns \code{Abbreviation} and \code{Full Term},
#'   de-duplicated and sorted.
#'
#' @examples
#' # Abbreviation_Listing(NCAParamSummary_info)
#' # Abbreviation_Listing(df, abbrev_col = "abbr")
# Abbreviation_Listing	 <- function(data, abbrev_col = "Abbreviation") {
#' #  dt <- data.frame(Abbreviation = data$parameter, stringsAsFactors = FALSE)
#' #  abbrev_listing <- dt %>%
#' #    distinct(!!rlang::sym(abbrev_col)) %>%
#' #    dplyr::rename(Abbreviation = !!rlang::sym(abbrev_col)) %>%
#' #    dplyr::mutate(`Full Term` = NA_character_) %>%
#' #    dplyr::arrange(Abbreviation)
#' #  return(abbrev_listing)
# }
Abbreviation_Listing <- function(data,
                                 abbrev_col = "Abbreviation",
                                 fullterm_col = "Full Term") {
  # Validate: data must have a 'parameter' column
  if (!"parameter" %in% names(data)) {
    stop("Input data must contain a column named 'parameter'.")
  }

  # Create dataframe with abbreviations from parameter column
  dt <- data.frame(Abbreviation = data$parameter, stringsAsFactors = FALSE)

  # Build listing with optional renaming
  abbrev_listing <- dt %>%
    distinct(Abbreviation) %>%
    dplyr::mutate(`Full Term` = NA_character_) %>%
    dplyr::arrange(Abbreviation)

  # Rename columns dynamically according to user input
  abbrev_listing <- abbrev_listing %>%
    dplyr::rename(
      !!abbrev_col := Abbreviation,
      !!fullterm_col := `Full Term`
    )

  return(abbrev_listing)
}

#-------------------------------------------------------------------------------
#' NCAConcentration_Listing
#'
#' Creates a long-format listing of individual concentrations by time, with
#' optional stratification columns (\code{covariates}). Summary metric rows are
#' removed, and the listing is left-joined with used concentration data
#' (including censoring flags). Occasion-level identifiers are concatenated to
#' form unique subject IDs when applicable.
#'
#' @param covariates Character vector of covariate names to include as
#'   stratification columns (e.g., \code{c("TREAT","FOOD")}); \code{NULL} for no
#'   stratification.
#'
#' @param useNominalTime Logical; if \code{TRUE}, the function attempts to use
#'   nominal time for the concentration listing. If nominal time is not available
#'   in the project (checked via \code{lixoftConnectors::getData()} and \code{headerTypes ==
#'   "nominaltime"}), the function will fall back to actual time and issue a
#'   warning. Default is \code{FALSE}.
#'
#' @details
#'   The function first resets stratification via
#'   \code{lixoftConnectors::setNCAResultsStratification()} and optionally splits results by
#'   \code{covariates}.
#'
#'   - **Actual time:** Always extracted via
#'   \code{lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE,
#'   useNominalTime = FALSE)}. This guarantees we can join later with
#'   \code{lixoftConnectors::getPointsIncludedForLambdaZ()}, which only provides actual times.
#'
#'   - **Nominal time:** If \code{useNominalTime = TRUE} and nominal time is
#'   available, the function also extracts nominal profiles. Since actual and
#'   nominal use the same wide column grid, both tables are pivoted to long and
#'   aligned per-subject using an artificial row index (\code{row_id}) that
#'   preserves column order. This ensures each actual concentration is paired
#'   with exactly one nominal time point.
#'
#'   - **Used concentrations:** After long-formatting, the table is joined with
#'   \code{lixoftConnectors::getPointsIncludedForLambdaZ()}, which indicates which concentration–
#'   time points were used in the lambda-z calculation and provides censoring
#'   flags.
#'
#'   - If occasion-level covariates exist, they are concatenated to the subject
#'   ID using \code{sep = "#"} to ensure unique identifiers.
#'
#' @return A data frame in long format containing (if present) \code{covariates},
#'   \code{id}, \code{ActualTime}, \code{NominalTime} (if available),
#'   \code{Concentration}, \code{UsedConc}, and \code{Censoring}.
#'
#' @examples
#' # Default: actual time only
#' NCAConcentration_Listing()
#'
#' # With covariates
#' NCAConcentration_Listing(covariates = c("TREAT", "FOOD"))
#'
#' # Include nominal time if available
#' NCAConcentration_Listing(covariates = c("TREAT", "FOOD"), useNominalTime = TRUE)
#'
NCAConcentration_Listing <- function(project = NULL,
                                     covariates = NULL,
                                     useNominalTime = FALSE) {
  # Ensure the correct project is loaded
  ensureProjectLoaded(project)

  # reinitialize stratification connector
  lixoftConnectors::setNCAResultsStratification()

  # Set stratification
  if (!is.null(covariates)) {
    lixoftConnectors::setNCAResultsStratification(split = covariates)
  }

  #--- Always extract ACTUAL time version (baseline)
  df_actual <- lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE, useNominalTime = FALSE)
  df_actual <- fixNCAConcentrationsColumnSwap(df_actual, covariates)
  metrics <- lixoftConnectors::getNCAConcentrations()$metric
  metric_labels <- as.character(metrics[!is.na(metrics)])

  indiv_actual <- df_actual %>%
    dplyr::filter(!(subject %in% metric_labels)) %>% # drop summary rows
    dplyr::mutate(dplyr::across(setdiff(names(.), c("subject", covariates)), as.character)) %>%
    tidyr::pivot_longer(
      cols = setdiff(names(.), c("subject", covariates)), # pivot from wide format to long
      names_to = "ActualTimeLabel",
      values_to = "Concentration"
    ) %>%
    dplyr::group_by(subject, dplyr::across(dplyr::all_of(covariates))) %>%
    dplyr::filter(Concentration != "") %>% # drop empty cells
    dplyr::mutate(row_id = row_number()) %>% # assign row index (per subject)
    dplyr::ungroup() %>%
    dplyr::mutate(
      Time = as.numeric(ActualTimeLabel), # numeric actual time
      id = as.character(subject)
    ) %>%
    dplyr::relocate(dplyr::any_of(covariates), id, Time, Concentration, row_id) %>%
    dplyr::select(-subject)

  #--- Optionally extract NOMINAL time version
  if (useNominalTime) {
    data <- lixoftConnectors::getData()
    has_nomtime <- any(data$headerTypes == "nominaltime")

    if (!has_nomtime) {
      warning("No nominal time available in project; only actual time will be included.")
      indiv <- indiv_actual %>% dplyr::select(-row_id, -ActualTimeLabel)
    } else {
      df_nom <- lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE, useNominalTime = TRUE)
      df_nom <- fixNCAConcentrationsColumnSwap(df_nom, covariates)

      indiv_nom <- df_nom %>%
        dplyr::filter(!(subject %in% metric_labels)) %>%
        dplyr::mutate(dplyr::across(setdiff(names(.), c("subject", covariates)), as.character)) %>%
        tidyr::pivot_longer(
          cols = setdiff(names(.), c("subject", covariates)),
          names_to = "NominalTimeLabel",
          values_to = "dummyConc"
        ) %>%
        dplyr::group_by(subject, dplyr::across(dplyr::all_of(covariates))) %>%
        dplyr::filter(dummyConc != "") %>%
        dplyr::mutate(row_id = row_number()) %>% # align by row_id (same column order)
        dplyr::ungroup() %>%
        dplyr::mutate(
          NominalTime = as.numeric(NominalTimeLabel),
          id = as.character(subject)
        ) %>%
        dplyr::relocate(dplyr::any_of(covariates), id, NominalTime, row_id) %>%
        dplyr::select(-subject)

      # Join actual + nominal on subject + covariates + row_id
      indiv <- indiv_actual %>%
        dplyr::left_join(indiv_nom, by = c("id", covariates, "row_id")) %>%
        dplyr::select(-row_id, -ActualTimeLabel, -NominalTimeLabel, -dummyConc)
    }
  } else {
    # If no nominal requested, clean up helper cols
    indiv <- indiv_actual %>% dplyr::select(-row_id, -ActualTimeLabel)
  }

  # Get used concentrations & censoring flags (can't use getNCAData bc of bug)
  UsedConc <- lixoftConnectors::getPointsIncludedForLambdaZ() %>%
    dplyr::rename(Time = time, UsedConc = concentration) %>%
    dplyr::mutate(id = as.character(id))

  # Handle occasion vars (if present: concat them into id)
  id_cols <- c("id", "Time", "UsedConc", "BLQ", "includedForLambdaZ")
  occasion_vars <- setdiff(names(UsedConc), id_cols)
  if (length(occasion_vars) > 0) {
    UsedConc <- UsedConc %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(occasion_vars), as.character)) %>%
      dplyr::mutate(id = do.call(paste, c(dplyr::across(c("id", dplyr::all_of(occasion_vars))), sep = "#"))) %>%
      dplyr::select(-dplyr::all_of(occasion_vars))
  }

  # Final join: (indiv conc & used conc) + cleanup
  indiv <- indiv %>%
    dplyr::left_join(UsedConc, by = c("id", "Time")) %>%
    dplyr::mutate(
      Concentration = as.character(Concentration),
      Censoring = dplyr::case_when(
        is.na(BLQ) & Concentration != "" ~ 1,
        TRUE ~ BLQ
      )
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(covariates), as.character)) %>%
    dplyr::select(-BLQ, -includedForLambdaZ) %>%
    dplyr::rename(ActualTime = Time) %>%
    dplyr::relocate(dplyr::any_of(c("ActualTime", "NominalTime")), .before = Concentration)

  return(as.data.frame(indiv))
}

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
Exclusion_Listing <- function(project = NULL) {
  # Ensure the correct project is loaded if explicitly provided
  if (!is.null(project)) {
    ensureProjectLoaded(project)
  }

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
    return(excl)
  } else {
    message(paste0("All the lines are in the ", software, " project, no table of exclusions is created."))
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
